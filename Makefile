##  Makefile

IDRIS := idris
PKG   := edda
OPTS  :=

.PHONY: clean build

install: build
	${IDRIS} ${OPTS} --install ${PKG}.ipkg

build: src/**/*.idr
	${IDRIS} ${OPTS} --build ${PKG}.ipkg

clean_build: clean build

clean:
	${IDRIS} --clean ${PKG}.ipkg
	find . -name "*~" -delete

check: clean
	${IDRIS} --checkpkg ${PKG}.ipkg

test :
	(cd tests; bash runtests.sh)

doc:
	${IDRIS} --mkdoc ${PKG}.ipkg