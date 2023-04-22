;;; auto-git-sync.el --- Automated git synchronisation with upstream -*- lexical-binding: t; -*-
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
;; Homepage: https://github.com/sbougerel/auto-git-sync

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
;;  branch with its upstream. It is intended to be used only when an individual
;;  relies on git as a mean to synchronise versioned content between machines,
;;  and should not be used in a team environment. A typical use case consists in
;;  synchronising your personal notes between machines.

;;; Code:

(require 'magit-git)
(require 'magit-process)

(defgroup auto-git-sync nil
  "Automated git synchronisation with upstream."
  :group 'tools
  :group 'vc)

(defcustom auto-git-sync-interval 300
  "Interval between synchronisation attempts, in seconds."
  :type 'integer
  :group 'auto-git-sync)

(defcustom auto-git-sync-dirs nil
  "Alist of (DIR . MESSAGE) that should be synchronised.

DIR is the top-level directory of the repository to synchronise.
MESSAGE is the commit message to use when committing changes."
  :type '(alist
          :key-type (directory :tag "Top-level directory")
          :value-type (string :tag "Commit message"))
  :group 'auto-git-sync)

(defun auto-git-sync--sync-cons ()
  "Return '(top-level-dir . message)' for repositories to synchronise or nil."
  (let ((git-dir (magit-toplevel)))
    (and git-dir
         (assoc git-dir auto-git-sync-dirs))))

(defun auto-git-sync--git-pull ()
  "Execute `git pull`."
  (when (auto-git-sync--sync-cons)
    (run-hooks 'magit-credential-hook)
    (magit-run-git-async "pull")))

(defun auto-git-sync--git-push ()
  "Execute `git push`."
  (let ((sync-cons (auto-git-sync--sync-cons)))
    (when sync-cons
      (magit-run-git-async "commit" "-a" "-m" (cdr sync-cons))
      (run-hooks 'magit-credential-hook)
      (magit-run-git-async "push"))))

(define-minor-mode auto-git-sync-mode
  "Automatically sync buffer with Git repository."
  :lighter " ↕"
  (if auto-git-sync-mode
      (progn
        (add-hook 'after-save-hook #'auto-git-sync--git-push nil t)
        (add-hook 'find-file-hook #'auto-git-sync--git-pull nil t))
    (remove-hook 'after-save-hook #'auto-git-sync--git-push t)
    (remove-hook 'find-file-hook #'auto-git-sync--git-pull t)))

(provide 'auto-git-sync)
;;; auto-git-sync.el ends here
