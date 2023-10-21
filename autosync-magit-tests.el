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

(require 'cl-lib)
(require 'ert)

(defun always-return (value)
  "Return VALUE always, regardless of its arguments."
  (lambda (&rest _) value))

(defun always-nil ()
  "Return nil always, alias of `always-return'."
  (always-return nil))

(defvar call-recorder nil
  "Record the arguments of the last call to `record-and-return'.")

(defun record-calls-and-return (value)
  "Return VALUE always, and record call arguments in LIST-VAR."
  (lambda (&rest args)
    (if (consp call-recorder)
        (setcdr (last call-recorder) (list args))
      (setq call-recorder (list args)))
    value))

(require 'autosync-magit)

(ert-deftest autosync-magit-dirs--assoc--found ()
  (cl-letf (((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-value 'autosync-magit-dirs)
             '(("/dir-other" . "commit message 1")
               ("/dir" . "commit message 2"))))
    (should
     (equal '("/dir" ."commit message 2")
            (autosync-magit-dirs--assoc)))))

(ert-deftest autosync-magit-dirs--assoc--not-found ()
  (cl-letf (((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-value 'autosync-magit-dirs)
             '(("/dir-other" . "commit message"))))
    (should
     (equal nil
            (autosync-magit-dirs--assoc)))))

(ert-deftest autosync-magit-dirs--assoc--empty ()
  (cl-letf (((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-value 'autosync-magit-dirs) nil))
    (should
     (equal nil
            (autosync-magit-dirs--assoc)))))

(ert-deftest autosync-magit-push--ahead ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-function 'cd) #'ignore)
            ((symbol-function 'magit-run-git-async) (record-calls-and-return nil))
            ((symbol-function 'magit-rev-eq) (always-nil))
            ((symbol-function 'set-process-sentinel)
             (lambda (process sentinel) process (funcall sentinel))))
    (autosync-magit-push "/dir" "other message")
    (should
     (equal '(("add" "-A")
              ("commit" "-a" "-m" "other message")
              ("push"))
            call-recorder))))

(ert-deftest autosync-magit-push--no-changes ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-function 'cd) #'ignore)
            ((symbol-function 'magit-run-git-async) (record-calls-and-return nil))
            ((symbol-function 'magit-rev-eq) (always-return t))
            ((symbol-function 'set-process-sentinel)
             (lambda (process sentinel) process (funcall sentinel))))
    (autosync-magit-push "/dir" "other message")
    (should
     (equal '(("add" "-A")
              ("commit" "-a" "-m" "other message"))
            call-recorder))))

(ert-deftest autosync-magit-pull--behind ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-function 'cd) #'ignore)
            ((symbol-function 'magit-run-git-async) (record-calls-and-return nil))
            ((symbol-function 'magit-rev-ancestor-p) (always-nil))
            ((symbol-function 'set-process-sentinel)
             (lambda (process sentinel) process (funcall sentinel))))
    (autosync-magit-pull "/dir")
    (should
     (equal '(("fetch")
              ("merge"))
            call-recorder))))

(ert-deftest autosync-magit-pull--ahead ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-function 'cd) #'ignore)
            ((symbol-function 'magit-run-git-async) (record-calls-and-return nil))
            ((symbol-function 'magit-rev-ancestor-p) (always-return t))
            ((symbol-function 'set-process-sentinel)
             (lambda (process sentinel) process (funcall sentinel))))
    (autosync-magit-pull "/dir")
    (should
     (equal '(("fetch"))
            call-recorder))))

;; Add push bounce / pull bounce tests
(ert-deftest autosync-magit-dirs--do-pull--elapsed ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-value 'autosync-magit--sync-alist)
             (list (cons "/dir" (autosync-magit--sync-create
                                 :last-pull (seconds-to-time 0)
                                 :next-push (seconds-to-time 0)))))
            ((symbol-function 'buffer-file-name) (always-return t))
            ((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-function 'autosync-magit-pull) (record-calls-and-return t)))
    (autosync-magit--do-pull)
    (should
     (equal '(("/dir"))
            call-recorder))))

(ert-deftest autosync-magit-dirs--do-pull--throttled ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-value 'autosync-magit--sync-alist)
             (list (cons "/dir" (autosync-magit--sync-create
                                 :last-pull (seconds-to-time (current-time))
                                 :next-push (seconds-to-time 0)))))
            ((symbol-function 'buffer-file-name) (always-return t))
            ((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-function 'autosync-magit-pull) (record-calls-and-return t)))
    (autosync-magit--do-pull)
    (should
     (equal nil
            call-recorder))))

(ert-deftest autosync-magit-dirs--do-push--elapsed ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-value 'autosync-magit-commit-message) "commit message")
            ((symbol-value 'autosync-magit--sync-alist)
             (list (cons "/dir" (autosync-magit--sync-create
                                 :last-pull (seconds-to-time 0)
                                 :next-push (seconds-to-time 0)))))
            ((symbol-function 'buffer-file-name) (always-return t))
            ((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-function 'run-with-timer) (record-calls-and-return t)))
    (autosync-magit--do-push)
    (should
     (equal (list (list autosync-magit-push-debounce nil
                        #'autosync-magit-push "/dir" "commit message"))
            call-recorder))))

(ert-deftest autosync-magit-dirs--do-push--debounced ()
  (cl-letf (((symbol-value 'call-recorder) nil)
            ((symbol-value 'autosync-magit-commit-message) "commit message")
            ((symbol-value 'autosync-magit--sync-alist)
             (list (cons "/dir" (autosync-magit--sync-create
                                 :last-pull (seconds-to-time 0)
                                 :next-push (seconds-to-time (time-add (current-time)
                                                                       autosync-magit-push-debounce))))))
            ((symbol-function 'buffer-file-name) (always-return t))
            ((symbol-function 'magit-toplevel) (always-return "/dir"))
            ((symbol-function 'run-with-timer) (record-calls-and-return t)))
    (autosync-magit--do-push)
    (should
     (equal nil
            call-recorder))))

(provide 'autosync-magit-tests)
;;; autosync-magit-tests.el ends here
