<a href="https://github.com/sbougerel/autosync-magit"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## autosync-magit.el
*Automatically synchronize content with upstream via magit*

---

[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CI Result](https://github.com/sbougerel/autosync-magit/actions/workflows/makefile.yml/badge.svg)](https://github.com/sbougerel/autosync-magit/actions)

Autosync-Magit provides a minor mode to automatically synchronise a local git
repository branch with its upstream.  It is intended to be used
exceptionally: when git is used solely to synchronise private content between
devices.  With this use case, there is typically no need to create branches,
and all changes can be pushed to the remote as soon as they are committed.
The author created it to synchronise his personal notes between different
devices.

Autosync-Magit should never be used for other use cases and especially not
for team settings.

To configure a repository to automatically synchronise, turn on
`autosync-magit-mode` in a buffer, and set the package variables accordingly.
Settings can be made permanent by adding `.dir-locals.el` in repositories you
want to synchronise.  Example:

    ((nil . ((autosync-magit-commit-message . "My commit message")
             (autosync-magit-pull-timer . 300)
             (mode . autosync-magit))))

The configuration above turns on the minor mode for any file visited in the
same directory as `.dir-locals.el` or in its sub-directories.  The
`autosync-magit-commit-message` is used as the commit message for each
commit.  The `autosync-magit-pull-timer` controls the period between
background pull attempts, in seconds.  See the documentation of each variable
for more details.

This is a simple package, that lends much of its functionality to `magit`
that does all the work asynchronously under the hood.

### Installation


With `straight.el` and `use-package.el`, add this to your `~/.emacs.d/init.el`:

    (use-package autosync-magit
      :straight (:host github
                 :repo "sbougerel/autosync-magit"
                 :files ("*.el")))

And restart Emacs.  If you're using Doom Emacs, add this to your
`~/.doom.d/packages.el`:

    (package! autosync-magit
      :recipe (:host github
               :repo "sbougerel/autosync-magit"
               :files ("*.el")))

Then add the following to `~/.doom.d/config.el`:

    (use-package! autosync-magit)

Then run `doom sync' to install it.

### Change Log


0.4.0 - Introduces a background timer for periodic pull.  This is superior to
the previous pull-on-events model, which does not work fast enough in a
variety of use cases.  Add `autosync-magit-pull-when-visiting` and
`autosync-magit-pull-timer` for background periodic pull.  Also removed
`autosync-magit-dirs`.

0.3.0 - Merges are synchronous, all other operations are asynchronous.  This
prevents any possible concurrency issues with `find-file-hook` functions.

0.2.0 - Use per-directory local variables
Deprecation of `autosync-magit-dirs` in favor of `.dir-locals.el`.

0.1.0 - initial release



### Customization Documentation

#### `autosync-magit-pull-interval`

Minimum interval between any pull attempts, in seconds.

`autosync-magit` starts pulling
updates from remotes periodically if `autosync-magit-mode` is
turned on for the buffer.  It pulls update via a timer or when
visiting a file if `autosync-magit-pull-when-visiting` is t for
that buffer.

This variable sets the minimum interval between any two pull
attempts, it is always enforced.  This is to ensure that
`autosync-magit-pull-timer` or
`autosync-magit-pull-when-visiting` will never run too close to
one another.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el`.  `autosync-magit` keeps a single copy of
this value per repository.  When `autosync-magit-mode` is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value.

#### `autosync-magit-pull-timer`

Interval between background pull attempts, in seconds.

`autosync-magit` start pulling updates from remotes periodically
via a background timer runing as soon as a buffer with
`autosync-magit-mode` visits a file in a repository.  This
variable sets or updates the period of the background timer.
Updates are not guaranteed to be pulled from the remote on the
timer expiring; see `autosync-magit-pull-interval`.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el`.  `autosync-magit` keeps a single copy of
this value per repository.  When `autosync-magit-mode` is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value.

#### `autosync-magit-pull-when-visiting`

When non-nil, `find-file` triggers `autosync-magit-pull`.

This variable is buffer-local.  When `autosync-magit-mode` is
enabled for the buffer, visiting a file also triggers a pull.

Do note that pulls are done asynchronously, even if merges are
synchronous.  All hooks running on the buffer will likely have
completed before git merge.  This setting is best used when
paired with Auto-Revert Mode.

Since it was the default behaviour in 0.3.0, before this tunable
was added; it is kept as t by default.  It will likely become nil
in future versions.

#### `autosync-magit-push-debounce`

Default duration in seconds that must elapse before the next push.

When you save a buffer, wait for `autosync-magit-push-debounce`
to elapse before pushing to the remote (again).  This ensures
that multiple file saves in a short period of time do not result
in multiple pushes.

This variable controls a buffer-local value that can be specified
in `.dir-locals.el`.  `autosync-magit` keeps a single copy of
this value per repository.  When `autosync-magit-mode` is turned
on in a buffer, the buffer-local value is copied to the
per-repository setting, overriding any previous value.

#### `autosync-magit-commit-message`

Commit message to use for each commit.

This variable is buffer-local.  Since the variable is
buffer-local, and commits & pushes are triggered from
`write-file-functions`, each file can have its custom commit
message.  *Caveat*: when multiple file saves occur within
`autosync-magit-push-debounce`, the commit message is the
buffer-local value of the first file saved.

### Function and Macro Documentation

#### `(autosync-magit-pull REPO_DIR)`

Fetch then merge (if needed) from REPO_DIR.
This interactive function is not throttled, it is executed as
soon as it called.  Merges are synchronous, to minimize possible
conflicts with files modified by Emacs in the repository.

#### `(autosync-magit-push REPO_DIR MESSAGE)`

Stage any change, create commit with MESSAGE and push to REPO_DIR.
This interactive function is not debounced, it is executed
asynchronously, as soon as it called.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
