.POSIX:
.PHONY: all compile test clean purge
.SUFFIXES: .el .elc

RM = rm -f
EMACS = emacs
BYTEC = autosync-magit.elc

REQS = dash \
	with-editor \
	magit-section \
	magit

REQ_DASH_VERSION=2.19.1
REQ_WITH_EDITOR_VERSION=3.2.0
REQ_MAGIT_SECTION_VERSION=3.3.0
REQ_MAGIT_VERSION=3.3.0

PKGS = dash-$(REQ_DASH_VERSION)
PKGS += with-editor-$(REQ_WITH_EDITOR_VERSION)
PKGS += magit-section-$(REQ_MAGIT_SECTION_VERSION)
PKGS += magit-$(REQ_MAGIT_VERSION)

PKGCACHE = $(abspath $(PWD)/package-cache)
PKGDIRS := $(addprefix $(PKGCACHE)/,$(PKGS))
LOADPATH = $(addprefix -L ,$(PKGDIRS))

all: compile

compile: $(PKGDIRS) $(BYTEC)

$(PKGDIRS):
	@echo "Installing dependencies: $(REQS)"
	mkdir -p $(PKGCACHE)
	$(EMACS) -Q --batch --eval "(progn (setq package-user-dir \"$(PKGCACHE)\") (package-initialize) (package-refresh-contents) (mapc 'package-install '($(REQS))))"
	@for pkg in $<; do \
		[ -d $pkg ] || (echo "Failed to install $pkg, version changed?" && exit 1) \
	done

test: $(PKGDIRS)
	$(EMACS) --version
	$(EMACS) -Q --batch \
		$(LOADPATH) \
		-L . \
		-l autosync-magit-tests.el -f ert-run-tests-batch-and-exit

purge: clean
	$(RM) -r $(PKGCACHE)

clean:
	$(RM) $(BYTEC)

.el.elc:
	@echo "Compiling $<"
	@$(EMACS) -Q --batch \
		$(LOADPATH) \
		-L . \
		-f batch-byte-compile $<
