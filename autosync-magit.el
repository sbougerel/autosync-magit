;;; autosync-magit.el --- Automatically synchronize content with upstream via magit -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sylvain Bougerel

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience tools vc git
;; URL: https://github.com/sbougerel/autosync-magit

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; [![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
;; [![CI Result](https://github.com/sbougerel/autosync-magit/actions/workflows/makefile.yml/badge.svg)](https://github.com/sbougerel/autosync-magit/actions)
;;
;; This package provides a minor mode to automatically synchronise a local git
;; repository branch with its upstream.  It is intended to be used
;; exceptionally: when git is used solely to synchronise private content between
;; devices.  It should never be used with typical repositories and especially
;; not for team settings.  A typical use case consists in synchronising your
;; personal notes between devices.

;;; Installation:
;;
;; With `straight.el' and `use-package.el', add this to your `~/.emacs.d/init.el':
;;
;; ```elisp
;; (use-package autosync-magit
;;  :straight (:host github
;;             :repo "sbougerel/autosync-magit"
;;             :files ("*.el"))
;;  :config
;;  (setq autosync-magit-dirs
;;        (list (cons "~/dir/to/sync" "Commit message")))
;;  (global-autosync-magit-mode 1))
;;  ```
;;
;; And restart Emacs.  If you're using Doom Emacs, add this to your
;; `~/.doom.d/packages.el':
;;
;; ```elisp
;; (package! autosync-magit
;;   :recipe (:host github
;;            :repo "sbougerel/autosync-magit"
;;            :files ("*.el")))
;; ```
;;
;; Then add the following to `~/.doom.d/config.el':
;;
;; ```elisp
;; (use-package! autosync-magit
;;  :config
;;  (setq autosync-magit-dirs
;;        (list (cons "~/dir/to/sync" "Commit message")))
;;  (global-autosync-magit-mode 1))
;; ```
;;
;; Then run `doom sync' to install it.


;;; Code:

(eval-when-compile (require 'cl-lib))

;; Definitions:
(defgroup autosync-magit nil
  "Automated git synchronisation with upstream."
  :group 'tools
  :group 'vc)

(defcustom autosync-magit-pull-interval 10
  "Buffer-local minimum interval between pull attempts, in seconds.

When the buffer window is selected (i.e. becomes active),
`autosync-magit' attempts to pull updates from the remotes.  This
variable ensures this is not done overly frequently."
  :type 'integer
  :local t
  :group 'autosync-magit)

(defcustom autosync-magit-push-debounce 5
  "Duration in seconds that must elapse before push can be called again.

When you save a buffer, wait for `autosync-magit-push-debounce'
to elapse before pushing to the remote (again).  This ensures that
multiple file saves in a short period of time do not result in
multiple pushes."
  :type 'integer
  :group 'autosync-magit)

(defcustom autosync-magit-dirs nil
  "Alist of `(REPO_DIR . MESSAGE)' that should be synchronised.

REPO_DIR is the top-level directory of the repository to synchronise.
MESSAGE is the commit message to use when committing changes."
  :type '(alist
          :key-type (directory :tag "Repository top-level directory")
          :value-type (string :tag "Commit message"))
  :group 'autosync-magit)

(cl-defstruct (autosync-magit--sync
               (:constructor autosync-magit--sync-create)
               (:copier nil))
  "A synchronisation object for a directory.

Stores timing about the pull and push operations."
  last-pull next-push)

(defvar autosync-magit--sync-alist ()
  "Global alist of `(REPO_DIR . OBJ)': sync OBJ for each DIRS.")

;; Check-declare lazy-loaded functions:
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-rev-eq "magit-git" (a b))
(declare-function magit-rev-ancestor-p "magit-git" (a b))
(declare-function magit-run-git-async "magit-process" (&rest args))

;; Implementation:
(defmacro autosync-magit--after (process &rest body)
  "Run BODY after async PROCESS."
  (declare (indent 1) (debug t))
  `(set-process-sentinel ,process (lambda (&rest _) ,@body)))

(defmacro autosync-magit--with-repo (repo_dir &rest body)
  "Run BODY in a temporary buffer where current directory is REPO_DIR."
  (declare (indent 1) (debug t))
  `(with-temp-buffer (cd ,repo_dir) ,@body))

;;;###autoload
(defun autosync-magit-pull (repo_dir)
  "Do `git fetch' then `git merge' from REPO_DIR.

This interactive function does not check wether the repository
belongs to `autosync-magit-dirs'.  It is not thorttled either."
  (interactive "D")
  (require 'magit-process nil t)
  (autosync-magit--after
      (autosync-magit--with-repo repo_dir
                                 (magit-run-git-async "fetch"))
    (autosync-magit--with-repo repo_dir
                               (when (not (magit-rev-ancestor-p "@{upstream}" "HEAD"))
                                 (magit-run-git-async "merge")))))

;;;###autoload
(defun autosync-magit-push (repo_dir message)
  "Do `git add -A', `git commit -m -a MESSAGE' then `git push' from REPO_DIR.

This interactive function does not check wether the repository
belongs to `autosync-magit-dirs'.  It is not debounced either."
  (interactive "D\nMCommit message: ")
  (require 'magit-process nil t)
  (autosync-magit--after
      (autosync-magit--with-repo repo_dir
                                 (magit-run-git-async "add" "-A"))
    (autosync-magit--after
        (autosync-magit--with-repo repo_dir
                                   (magit-run-git-async "commit" "-a" "-m" message))
      (autosync-magit--with-repo repo_dir
                                 (when (not (magit-rev-eq "@{push}" "HEAD"))
                                   (magit-run-git-async "push"))))))

(defun autosync-magit-dirs--assoc ()
  "Return non-nil when buffer's file belong to a directory to synchronise.

The value returned is an element of `autosync-magit-dirs'."
  (require 'magit-process nil t) ; also loads magit-git
  (when (featurep 'magit-git)
    (let ((repo_dir (magit-toplevel))) ; use buffer's defaults-directory
      (and repo_dir
           (assoc repo_dir autosync-magit-dirs)))))

(defun autosync-magit--do-pull (&optional _)
  "Buffer-local function which pulls upstream change."
  (when (buffer-file-name) ; avoid running on *minibuffer* when deselecting, e.g.
    (let* ((sync-cons (autosync-magit-dirs--assoc))
           (sync (cdr (assoc (car sync-cons) autosync-magit--sync-alist))))
      (when (and sync
                 (time-less-p
                  (time-add (autosync-magit--sync-last-pull sync)
                            (seconds-to-time autosync-magit-pull-interval))
                  (current-time)))
        (setf (autosync-magit--sync-last-pull sync) (current-time))
        (autosync-magit-pull (car sync-cons))))))

(defun autosync-magit--do-push (&optional _)
  "Buffer-local function which pushes change upstream."
  (let* ((sync-cons (autosync-magit-dirs--assoc))
         (sync (cdr (assoc (car sync-cons) autosync-magit--sync-alist))))
    (when (and sync
               (time-less-p
                (autosync-magit--sync-next-push sync)
                (current-time)))
      (setf (autosync-magit--sync-next-push sync)
            (time-add (current-time) autosync-magit-push-debounce))
      (run-with-timer autosync-magit-push-debounce nil
                      #'autosync-magit-push
                      (car sync-cons) (cdr sync-cons)))))

;;;###autoload
(define-minor-mode autosync-magit-mode
  "Autosync-Magit minor mode."
  :init-value nil
  :global nil
  :lighter " ↕"
  :group 'autosync-magit
  (if autosync-magit-mode
      (let ((sync-cons (autosync-magit-dirs--assoc)))
        (if sync-cons
            (progn
              (if (not (assoc (car sync-cons) autosync-magit--sync-alist))
                  (push (cons (car sync-cons)
                              (autosync-magit--sync-create
                               :last-pull (seconds-to-time 0)
                               :next-push (seconds-to-time 0)))
                        autosync-magit--sync-alist))
              (add-hook 'after-save-hook #'autosync-magit--do-push nil t)
              (add-hook 'find-file-hook #'autosync-magit--do-pull nil t)
              (add-hook 'window-selection-change-functions #'autosync-magit--do-pull nil t))
          (autosync-magit-mode -1)))
    (remove-hook 'after-save-hook #'autosync-magit--do-push t)
    (remove-hook 'find-file-hook #'autosync-magit--do-pull t)
    (remove-hook 'window-selection-change-functions #'autosync-magit--do-pull t)))

(defun autosync-magit--turn-on ()
  "Turn on `autosync-magit-mode'."
  (when (buffer-file-name)
    (autosync-magit-mode +1)))

;;;###autoload
(define-global-minor-mode global-autosync-magit-mode autosync-magit-mode
  autosync-magit--turn-on
  :group 'autosync-magit)

(provide 'autosync-magit)
;;; autosync-magit.el ends here
