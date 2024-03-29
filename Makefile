.POSIX:
.PHONY: all compile test clean purge
.SUFFIXES: .el .elc
.INTERMEDIATE: make-readme-markdown.el

RM = rm -f
EMACS = emacs
SRC = autosync-magit.el
BYTEC = $(SRC)c

# Should pull the following dependencies:
#   dash
#   with-editor
#   magit-section
REQS := magit

PKGCACHE := $(abspath $(PWD)/package-cache)

# INIT_PACKAGE_EL from package-lint (https://github.com/purcell/package-lint)
# Copyrights: Steve Purcell (https://github.com/purcell)
INIT_PACKAGE_EL := "(progn \
  (require 'package) \
  (setq package-user-dir \"$(PKGCACHE)\") \
  (setq package-archives \
	'((\"gnu\" . \"https://elpa.gnu.org/packages/\") \
	  (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '($(REQS))) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

BATCH = $(EMACS) -Q --batch --eval $(INIT_PACKAGE_EL)

all: compile

compile: $(BYTEC)

test: $(BYTEC)
	$(BATCH) \
		-L . \
		-l autosync-magit-tests.el \
		-f ert-run-tests-batch-and-exit

purge: clean
	$(RM) -r $(PKGCACHE)

clean:
	$(RM) $(BYTEC)

README.md: make-readme-markdown.el $(SRC)
	$(EMACS) -Q --script $< <$(SRC) >$@

make-readme-markdown.el:
	curl -L -o $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.el.elc:
	@echo "Compiling $<"
	@$(BATCH) \
		-L . \
		-f batch-byte-compile $<
