;;; autosync-magit.el --- Automatically synchronize content with upstream via magit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Sylvain Bougerel
;;

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>

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

;;  Test 'autosync-magit'.

;;; Code:


(require 'ert)
(require 'autosync-magit)

(ert-deftest autosync-magit--sync-cons-test ()
  "Test autosync-magit--sync-cons function."
  (let* ((repo (magit-toplevel))
         (autosync-magit-dirs (list
                              (cons repo "commit message 1")
                              (cons repo "commit message 2"))))
    (should (equal (autosync-magit--sync-cons) (cons repo "commit message 1"))))
  ;; TODO add test for directory that is not part of a repository
  (let* ((autosync-magit-dirs nil))
    (should (equal (autosync-magit--sync-cons) nil)))
  (let* ((autosync-magit-dirs ()))
    (should (equal (autosync-magit--sync-cons) nil)))
  (let* ((autosync-magit-dirs '(("not a directory" . "not a message"))))
    (should (equal (autosync-magit--sync-cons) nil)))
  )

(ert-deftest autosync-magit-mode--required-test ()
  "Test that autosync-magit-mode is autoloaded."
  (memq 'global-autosync-magit-mode global-minor-modes))
