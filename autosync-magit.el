;;; autosync-magit.el --- Automatically synchronize content with upstream via magit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Sylvain Bougerel
;;

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>

;; Created: April 19, 2023
;; Modified: April 19, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;; Keywords: convenience tools vc git
;; Homepage: https://github.com/sbougerel/autosync-magit

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;  This package provides a minor mode to automatically synchronise a local git
;;  repository branch with its upstream. It is intended to be used only when an
;;  individual relies on git as a mean to synchronise content privately between
;;  machines, and should not be used when control over commits is desired and
;;  especially not for team work.
;;
;;  A typical use case consists in synchronising your personal notes between
;;  machines.

;;; Code:

;; Definitions:
(defgroup autosync-magit nil
  "Automated git synchronisation with upstream."
  :group 'tools
  :group 'vc)

(defcustom autosync-magit-interval 300
  "Interval between synchronisation attempts, in seconds."
  :type 'integer
  :group 'autosync-magit)

(defcustom autosync-magit-dirs nil
  "Alist of (DIR . MESSAGE) that should be synchronised.

DIR is the top-level directory of the repository to synchronise.
MESSAGE is the commit message to use when committing changes."
  :type '(alist
          :key-type (directory :tag "Top-level directory")
          :value-type (string :tag "Commit message"))
  :group 'autosync-magit)

;; Check-declare lazy-loaded functions:
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-rev-eq "magit-git" (a b))
(declare-function magit-rev-ancestor-p "magit-git" (a b))
(declare-function magit-run-git-async "magit-process" (&rest args))

;; Implementation:
(defmacro autosync-magit--when-idle (&rest body)
  "Run `BODY' when Emacs is idle."
  (declare (indent 0) (debug t))
  `(run-with-idle-timer 0 nil (lambda () ,@body)))

(defmacro autosync-magit--if (process &rest body)
  "Run `BODY' if async `PROCESS' is successful."
  (declare (indent 1) (debug t))
  `(set-process-sentinel
    ,process
    (lambda (val _)
      (when (and (memq (process-status val) '(exit signal))
                 (zerop (process-exit-status val)))
        ,@body))))

(defmacro autosync-magit--after (process &rest body)
  "Run `BODY' after async `PROCESS'."
  (declare (indent 1) (debug t))
  `(set-process-sentinel ,process (lambda (_ _) ,@body)))

(defun autosync-magit--req-sync ()
  "Return '(top-level-dir . message)' for repositories to synchronise or nil."
  (require 'magit-process nil t) ; also loads magit-git
  (when (featurep 'magit-git)
    (let ((git-dir (magit-toplevel)))
      (and git-dir
           (assoc git-dir autosync-magit-dirs)))))

;;;###autoload
(defun autosync-magit-pull ()
  "Execute `git fetch` then `git merge'."
  (interactive)
  (autosync-magit--when-idle
    (when (autosync-magit--req-sync)
      (autosync-magit--after
          (magit-run-git-async "fetch")
        (when (not (magit-rev-ancestor-p "@{upstream}" "HEAD"))
          (magit-run-git-async "merge"))))))

;;;###autoload
(defun autosync-magit-push ()
  "Execute `git add -A', `git commit -m -a' then `git push'."
  (interactive)
  (autosync-magit--when-idle
    (let ((sync-cons (autosync-magit--req-sync)))
      (when sync-cons
        (autosync-magit--after
            (magit-run-git-async "add" "-A")
          (autosync-magit--after
              (magit-run-git-async "commit" "-a" "-m" (cdr sync-cons))
            (when (not (magit-rev-eq "@{push}" "HEAD"))
              (magit-run-git-async "push"))))))))

;;;###autoload
(define-minor-mode autosync-magit-mode
  "Autosync-Magit mode."
  :init-value nil
  :global nil
  :lighter " ↕"
  :group 'autosync-magit
  (if autosync-magit-mode
      (if (autosync-magit--req-sync)
          (progn
            (add-hook 'after-save-hook #'autosync-magit-push nil t)
            (add-hook 'find-file-hook #'autosync-magit-pull nil t))
        (autosync-magit-mode -1))
    (remove-hook 'after-save-hook #'autosync-magit-push t)
    (remove-hook 'find-file-hook #'autosync-magit-pull t)))

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
