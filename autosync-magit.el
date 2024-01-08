;;; autosync-magit.el --- Automatically synchronize content with upstream via magit -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sylvain Bougerel

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1") (magit "2.9.0"))
;; Keywords: convenience tools magit git
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
;; Autosync-Magit provides a minor mode to automatically synchronise a local git
;; repository branch with its upstream.  It is intended to be used
;; exceptionally: when git is used solely to synchronise private content between
;; devices.  With this use case, there is typically no need to create branches,
;; and all changes can be pushed to the remote as soon as they are committed.
;; The author created it to synchronise his personal notes between different
;; devices.
;;
;; Autosync-Magit should never be used for other use cases and especially not
;; for team settings.
;;
;; To configure a repository to automatically synchronise, turn on
;; `autosync-magit-mode' in a buffer, and set the package variables accordingly.
;; Settings can be made permanent by adding `.dir-locals.el' in repositories you
;; want to synchronise.  Example:
;;
;;     ((nil . ((autosync-magit-commit-message . "My commit message")
;;              (autosync-magit-pull-timer . 300)
;;              (mode . autosync-magit))))
;;
;; The configuration above turns on the minor mode for any file visited in the
;; same directory as `.dir-locals.el' or in its sub-directories.  The
;; `autosync-magit-commit-message' is used as the commit message for each
;; commit.  The `autosync-magit-pull-timer' controls the period between
;; background pull attempts, in seconds.  See the documentation of each variable
;; for more details.
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
;; 0.4.0 - Introduces a background timer for periodic pull.  This is superior to
;; the previous pull-on-events model, which does not work fast enough in a
;; variety of use cases.  Add `autosync-magit-pull-when-visiting' and
;; `autosync-magit-pull-timer' for background periodic pull.  Also removed
;; `autosync-magit-dirs'.
;;
;; 0.3.0 - Merges are synchronous, all other operations are asynchronous.  This
;; prevents any possible concurrency issues with `find-file-hook' functions.
;;
;; 0.2.0 - Use per-directory local variables
;; Deprecation of `autosync-magit-dirs' in favor of `.dir-locals.el'.
;;
;; 0.1.0 - initial release

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'magit-git)
(require 'magit-process)

;; Definitions:
(defgroup autosync-magit nil
  "Automatically synchronize content with upstream via magit."
  :group 'tools)

;;;###autoload(put 'autosync-magit-pull-interval 'safe-local-variable 'integerp)
(defcustom autosync-magit-pull-interval 10
  "Minimum interval between any pull attempts, in seconds.

`autosync-magit' starts pulling
updates from remotes periodically if `autosync-magit-mode' is
turned on for the buffer.  It pulls update via a timer or when
visiting a file if `autosync-magit-pull-when-visiting' is t for
that buffer.

This variable sets the minimum interval between any two pull
attempts, it is always enforced.  This is to ensure that
`autosync-magit-pull-timer' or
`autosync-magit-pull-when-visiting' will never run too close to
one another.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el'.  `autosync-magit' keeps a single copy of
this value per repository.  When `autosync-magit-mode' is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value."
  :type 'integer
  :group 'autosync-magit)

;;;###autoload(put 'autosync-magit-pull-timer 'safe-local-variable 'integerp)
(defcustom autosync-magit-pull-timer 300
  "Interval between background pull attempts, in seconds.

`autosync-magit' start pulling updates from remotes periodically
via a background timer runing as soon as a buffer with
`autosync-magit-mode' visits a file in a repository.  This
variable sets or updates the period of the background timer.
Updates are not guaranteed to be pulled from the remote on the
timer expiring; see `autosync-magit-pull-interval'.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el'.  `autosync-magit' keeps a single copy of
this value per repository.  When `autosync-magit-mode' is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value."
  :type 'integer
  :group 'autosync-magit)

;;;###autoload(put 'autosync-magit-pull-when-visiting 'safe-local-variable 'booleanp)
(defcustom autosync-magit-pull-when-visiting t
  "When non-nil, `find-file' triggers `autosync-magit-pull'.

This variable is buffer-local.  When `autosync-magit-mode' is
enabled for the buffer, visiting a file also triggers a pull.

Do note that pulls are done asynchronously, even if merges are
synchronous.  All hooks running on the buffer will likely have
completed before git merge.  This setting is best used when
paired with Auto-Revert Mode.

Since it was the default behaviour in 0.3.0, before this tunable
was added; it is kept as t by default.  It will likely become nil
in future versions."
  :type 'boolean
  :group 'autosync-magit)

;;;###autoload(put 'autosync-magit-push-debounce 'safe-local-variable 'integerp)
(defcustom autosync-magit-push-debounce 5
  "Default duration in seconds that must elapse before the next push.

When you save a buffer, wait for `autosync-magit-push-debounce'
to elapse before pushing to the remote (again).  This ensures
that multiple file saves in a short period of time do not result
in multiple pushes.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el'.  `autosync-magit' keeps a single copy of
this value per repository.  When `autosync-magit-mode' is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value."
  :type 'integer
  :group 'autosync-magit)

;;;###autoload(put 'autosync-magit-commit-message 'safe-local-variable 'stringp)
(defcustom autosync-magit-commit-message "Automated commit by autosync-magit"
  "Commit message to use for each commit.

This variable is buffer-local.  Since the variable is
buffer-local, and commits & pushes are triggered from
`write-file-functions', each file can have its custom commit
message.  *Caveat*: when multiple file saves occur within
`autosync-magit-push-debounce', the commit message is the
buffer-local value of the first file saved."
  :type 'string
  :group 'autosync-magit)

(cl-defstruct (autosync-magit--sync
               (:constructor autosync-magit--sync-create)
               (:copier nil))
  "A synchronisation object for a directory.

Stores timing about the pull and push operations."
  last-pull next-push timer)

(defvar autosync-magit--sync-alist ()
  "Global alist of (REPO_DIR . OBJ): sync OBJ for each DIRS.

Do not modify this variable directly.  Visit files in buffers
with `autosync-magit-mode' turned on or use `autosync-magit-set'
instead.")

;; Implementation:
(defmacro autosync-magit--after (process &rest body)
  "Run BODY after async PROCESS."
  (declare (indent 1) (debug t))
  `(set-process-sentinel ,process (lambda (&rest _) ,@body)))

(defmacro autosync-magit--with-repo (repo_dir &rest body)
  "Run BODY in a temporary buffer where current directory is REPO_DIR.

Since file-visiting buffer may be killed before this function runs,
we don't rely on them."
  (declare (indent 1) (debug t))
  `(with-temp-buffer (cd ,repo_dir) ,@body))

;;;###autoload
(defun autosync-magit-pull (repo_dir)
  "Fetch then merge (if needed) from REPO_DIR.

This interactive function is not throttled, it is executed as
soon as it called.  Merges are synchronous, to minimize possible
conflicts with files modified by Emacs in the repository."
  (interactive "D")
  (let ((repo_dir (magit-toplevel repo_dir)))
    (if (not repo_dir)
        (message (format "Autosync-Magit: \"%s\" is not a path to a git repository" repo_dir))
      (when-let ((sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
        (setf (autosync-magit--sync-last-pull sync) (current-time)))
      (autosync-magit--after
          (autosync-magit--with-repo repo_dir
            (magit-run-git-async "fetch"))
        (autosync-magit--with-repo repo_dir
          (when (not (magit-rev-ancestor-p "@{upstream}" "HEAD"))
            (magit-run-git "merge"))))))) ; synchronous

;;;###autoload
(defun autosync-magit-push (repo_dir message)
  "Stage any change, create commit with MESSAGE and push to REPO_DIR.

This interactive function is not debounced, it is executed
asynchronously, as soon as it called."
  (interactive "D\nMCommit message: ")
  (let ((repo_dir (magit-toplevel repo_dir)))
    (if (not repo_dir)
        (message (format "Autosync-Magit: \"%s\" is not a path to a git repository" repo_dir))
      (autosync-magit--after
          (autosync-magit--with-repo repo_dir
            (magit-run-git-async "add" "-A"))
        (autosync-magit--after
            (autosync-magit--with-repo repo_dir
              (magit-run-git-async "commit" "-a" "-m" message))
          (autosync-magit--with-repo repo_dir
            (when (not (magit-rev-eq "@{push}" "HEAD"))
              (magit-run-git-async "push"))))))))

(defun autosync-magit--throttle-pull (repo_dir)
  "Pull change from upstream into REPO_DIR."
  (when-let ((sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
    (when (time-less-p
           (time-add (autosync-magit--sync-last-pull sync)
                     (seconds-to-time autosync-magit-pull-interval))
           (current-time))
      (autosync-magit-pull repo_dir))))

(defun autosync-magit--pull-on-find-file (&optional _)
  "Pull upstream change when visiting a file."
  (when (buffer-file-name) ; avoid running on *minibuffer* when deselecting, e.g.
    (when-let ((repo_dir (magit-toplevel)))
      (autosync-magit--throttle-pull repo_dir))))

(defun autosync-magit--pull-on-timer (repo_dir)
  "Periodically pulls REPO_DIR from upstream."
  (when-let ((time_triggered (current-time))
             (sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
    (autosync-magit--throttle-pull repo_dir)
    (run-with-timer (max 1 (- (autosync-magit--sync-timer sync)
                              (floor (float-time (time-subtract nil time_triggered)))))
                    nil #'autosync-magit--pull-on-timer repo_dir)))

(defun autosync-magit--push-after-save (&optional _)
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

    ((nil . ((autosync-magit-commit-message . \"My commit message\")
             (autosync-magit-pull-timer . 300)
             (mode . autosync-magit))))

Customize these values to your liking."
  :init-value nil
  :global nil
  :lighter " ↕"
  :group 'autosync-magit
  (if autosync-magit-mode
      (let ((repo_dir (magit-toplevel)))
        (if (not repo_dir)
            (autosync-magit-mode -1)
          (let ((sync (cdr (assoc repo_dir autosync-magit--sync-alist))))
            (if sync
                ;; Repo already visited: update timer
                (setf (autosync-magit--sync-timer sync)
                      autosync-magit-pull-timer)
              ;; First file visited for repo: add to alist and launch timer
              (push (cons repo_dir
                          (autosync-magit--sync-create
                           :last-pull (seconds-to-time 0)
                           :next-push (seconds-to-time 0)
                           :timer autosync-magit-pull-timer))
                    autosync-magit--sync-alist)
              (autosync-magit--pull-on-timer repo_dir)))
          (add-hook 'after-save-hook #'autosync-magit--push-after-save nil t)
          (if autosync-magit-pull-when-visiting
              (add-hook 'find-file-hook #'autosync-magit--pull-on-find-file nil t))))
    (remove-hook 'after-save-hook #'autosync-magit--push-after-save t)
    (remove-hook 'find-file-hook #'autosync-magit--pull-on-find-file t)))

(provide 'autosync-magit)
;;; autosync-magit.el ends here
