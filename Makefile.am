SUBDIRS = 
ACLOCAL_AMFLAGS = -I build-aux
PKG_LIST_VERSION=$(shell echo $(PACKAGE_VERSION) | sed "s/\./ /g")
SOURCES = p-rout-collect.scm p-rout-view.scm p-rout-restore.scm
bin_SCRIPTS = $(SOURCES) p-rout-dump.sh
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GOBJECTS = $(SOURCES:%.scm=%.go)
CLEANFILES = $(GOBJECTS)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
