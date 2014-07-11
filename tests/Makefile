##  Makefile

IDRIS := idris
PKG   := test
OPTS  :=

.PHONY: clean build

build:
	${IDRIS} ${OPTS} --build ${PKG}.ipkg

clean:
	${IDRIS} --clean ${PKG}.ipkg
	find . -name "*~" -delete

clean_build: clean build

check: clean
	${IDRIS} --checkpkg ${PKG}.ipkg
