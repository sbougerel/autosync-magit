;;; autosync-magit.el --- Automatically synchronize content with upstream via magit -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sylvain Bougerel

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1") (magit "2.9.0"))
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
;;
;; To configure a repository to automatically synchronise, turn on
;; `autosync-magit-mode` in a buffer, and set the package variables accordingly.
;; Settings can be made permanent by adding `.dir-locals.el` in repositories you
;; want to synchronise.  Example:
;;
;;     ((nil . ((eval . (autosync-magit-mode 1))
;;              (autosync-magit-commit-message . "My commit message")
;;              (autosync-magit-pull-interval . 30))))
;;
;; The configuration above turns on the minor mode for any file visited in the
;; same directory as `.dir-locals.el' or in its sub-directories.  The
;; `autosync-magit-commit-message' is used as the commit message for each
;; commit.  The `autosync-magit-pull-interval' is the minimum interval between
;; pull attempts, in seconds.  See the documentation for each variable for more
;; details.
;;
;; This is a simple package, that lends much of its functionality to `magit'
;; that does all the work asynchronously under the hood.

;;; Installation:
;;
;; With `straight.el' and `use-package.el', add this to your `~/.emacs.d/init.el':
;;
;;     (use-package autosync-magit
;;       :straight (:host github
;;                  :repo "sbougerel/autosync-magit"
;;                  :files ("*.el")))
;;
;; And restart Emacs.  If you're using Doom Emacs, add this to your
;; `~/.doom.d/packages.el':
;;
;;     (package! autosync-magit
;;       :recipe (:host github
;;                :repo "sbougerel/autosync-magit"
;;                :files ("*.el")))
;;
;; Then add the following to `~/.doom.d/config.el':
;;
;;     (use-package! autosync-magit)
;;
;; Then run `doom sync' to install it.

;;; Change Log:
;;
;; 0.2.0 - Use per-directory local variables
;; Deprecation of `autosync-magit-dirs' in favor of `.dir-locals.el'.
;;
;; 0.1.0 - initial release

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Definitions:
(defgroup autosync-magit nil
  "Automated git synchronisation with upstream."
  :group 'tools
  :group 'vc)

;;;###autoload (put 'autosync-magit-pull-interval 'safe-local-variable #'integerp)
(defcustom autosync-magit-pull-interval 10
  "Minimum interval between pull attempts, in seconds.

This variable is buffer-local.  When the buffer window is
selected (i.e. becomes active), `autosync-magit' attempts to pull
updates from the remotes.  This variable ensures this is not done
overly frequently."
  :type 'integer
  :safe #'integerp
  :local t
  :version "0.2.0"
  :group 'autosync-magit)

;;;###autoload (put 'autosync-magit-push-debounce 'safe-local-variable #'integerp)
(defcustom autosync-magit-push-debounce 5
  "Duration in seconds that must elapse before push can be called again.

This variable is buffer-local.  When you save a buffer, wait for
`autosync-magit-push-debounce' to elapse before pushing to the
remote (again).  This ensures that multiple file saves in a short
period of time do not result in multiple pushes."
  :type 'integer
  :safe #'integerp
  :local t
  :version "0.2.0"
  :group 'autosync-magit)

;;;###autoload (put 'autosync-magit-commit-message 'safe-local-variable #'stringp)
(defcustom autosync-magit-commit-message "Automated commit by autosync-magit"
  "Commit message to use for each commit.

This variable is buffer-local."
  :type 'string
  :safe #'stringp
  :local t
  :version "0.2.0"
  :group 'autosync-magit)

(defcustom autosync-magit-dirs nil
  "Alist of `(REPO_DIR . MESSAGE)` that should be synchronised.

*DEPRECATED*: use `.dir-locals.el' instead.  By using
`.dir-locals.el', you ensure that your private configuration does
not depends on any particular project's location on a host, and
you can set per-repository configuration.  Use of the variable
will be removed in version 0.3.0.

REPO_DIR is the top-level directory of the repository to
synchronise.  MESSAGE is the commit message to use when
committing changes."
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
  "Global alist of `(REPO_DIR . OBJ)': sync OBJ for each DIRS.

Do not modify this variable directly.  Visit files or close
related buffers instead.")

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

This interactive function is not throttled, it is executed
asynchronously, as soon as it called."
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

This interactive function is not debounced, it is executed
asynchronously, as soon as it called."
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

The value returned is an element of `autosync-magit-dirs`."
  (require 'magit-process nil t) ; also loads magit-git
  (when (featurep 'magit-git)
    (let ((repo_dir (magit-toplevel))) ; use buffer's defaults-directory
      (and repo_dir
           (assoc repo_dir autosync-magit-dirs)))))

(defun autosync-magit--do-pull (&optional _)
  "Pull upstream change with a thorttle."
  (when (buffer-file-name) ; avoid running on *minibuffer* when deselecting, e.g.
    (let* ((repo_dir (magit-toplevel))
           (sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
      (when (and sync
                 (time-less-p
                  (time-add (autosync-magit--sync-last-pull sync)
                            (seconds-to-time autosync-magit-pull-interval))
                  (current-time)))
        (setf (autosync-magit--sync-last-pull sync) (current-time))
        (autosync-magit-pull repo_dir)))))

(defun autosync-magit--do-push (&optional _)
  "Push change upstream with a debounce."
  (let* ((repo_dir (magit-toplevel))
         (sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
    (when (and sync
               (time-less-p
                (autosync-magit--sync-next-push sync)
                (current-time)))
      (setf (autosync-magit--sync-next-push sync)
            (time-add (current-time) autosync-magit-push-debounce))
      (run-with-timer autosync-magit-push-debounce nil
                      #'autosync-magit-push
                      repo_dir autosync-magit-commit-message))))

;;;###autoload
(define-minor-mode autosync-magit-mode
  "Autosync-Magit minor mode.

Turn on `autosync-magit-mode' with `.dir-locals.el' in repositories you want to
synchronise; example:

#+BEGIN_SRC: elisp
\((nil . ((eval . (autosync-magit-mode 1))
         (autosync-magit-commit-message . \"My commit message\")
         (autosync-magit-pull-interval . 30))))
#+END_SRC

Customize these values to your liking."
  :init-value nil
  :global nil
  :lighter " ↕"
  :group 'autosync-magit
  (if autosync-magit-mode
      (let ((repo_dir (magit-toplevel)))
        (if repo_dir
            (progn
              (if (not (assoc repo_dir autosync-magit--sync-alist))
                  (push (cons repo_dir
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
    ;; `autosync-magit-dirs' is being deprecated in favor of `.dir-locals.el';
    ;; to ensure compatibility with older configuration, we perform the check
    ;; with `autosync-magit-dirs' here, where we also set the associated
    ;; buffer-local variable `autosync-magit-commit-message'.  This will be
    ;; removed in a future version.
    (let ((elem (autosync-magit-dirs--assoc)))
      (if elem
          (progn
            (message "autosync-magit: deprecated use of `autosync-magit-dirs'.  Please use `.dir-locals.el' instead, see documentation.")
            (setq autosync-magit-commit-message (cdr elem))
            (autosync-magit-mode +1))))))

;;;###autoload
(define-global-minor-mode global-autosync-magit-mode autosync-magit-mode
  autosync-magit--turn-on
  :group 'autosync-magit)

(provide 'autosync-magit)
;;; autosync-magit.el ends here
