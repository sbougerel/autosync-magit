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
;;  individual relies on git as a trivial to synchronise content between
;;  machines, and should not be used when control over history of changes is
;;  desired and especially not for team work. A typical use case consists in
;;  synchronising your personal notes between machines.

;;; Code:

(require 'magit-git)
(require 'magit-process)

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

(defun autosync-magit--sync-cons ()
  "Return '(top-level-dir . message)' for repositories to synchronise or nil."
  (let ((git-dir (magit-toplevel)))
    (and git-dir
         (assoc git-dir autosync-magit-dirs))))

(defun autosync-magit--pull ()
  "Execute `git pull`."
  (when (autosync-magit--sync-cons)
    (magit-run-git-async "pull")))

(defun autosync-magit--commit-push ()
  "Execute `git push`."
  (let ((sync-cons (autosync-magit--sync-cons)))
    (message "autosync-magit--git-push: %s" sync-cons)
    (when sync-cons
      (set-process-sentinel
       (magit-run-git-async "commit" "-a" "-m" (cdr sync-cons))
       (lambda (process _)
         (when (and (memq (process-status process) '(exit signal))
                    (zerop (process-exit-status process)))
           (magit-run-git-async "push")))))))

;;;###autoload
(define-minor-mode autosync-magit-mode
  "Automatically sync new buffers with Git repository."
  :global t
  :require 'autosync-magit
  :group 'autosync-magit
  :lighter " ↕"
  (if autosync-magit-mode
      (progn
        (add-hook 'after-save-hook #'autosync-magit--commit-push nil t)
        (add-hook 'find-file-hook #'autosync-magit--pull nil t))
    (remove-hook 'after-save-hook #'autosync-magit--commit-push t)
    (remove-hook 'find-file-hook #'autosync-magit--pull t)))

(provide 'autosync-magit)
;;; autosync-magit.el ends here
