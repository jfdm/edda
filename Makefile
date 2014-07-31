##  Makefile

IDRIS := idris
LIB   := edda
BIN   := eddabin
OPTS  :=

.PHONY: clean build

exe: install
	${IDRIS} ${OPTS} --build ${BIN}.ipkg

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	${IDRIS} --clean ${BIN}.ipkg
	find . -name "*~" -delete

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

clobber : clean
	rm eddabin
	find . -name "*.ibc" -delete

test :
	(cd tests; bash runtests.sh)

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg
