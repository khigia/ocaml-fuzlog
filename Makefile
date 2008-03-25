TEST_SOURCES=${wildcard test/*Test.ml}
TEST_BYTE   =${TEST_SOURCES:.ml=.byte}

EX_SOURCES=${wildcard example/*.ml}
EX_BYTE   =${EX_SOURCES:.ml=.byte}

OCAMLBUILD=ocamlbuild -classic-display


.PHONY: ex
ex:
	$(OCAMLBUILD) ${EX_BYTE}

.PHONY: build
build:
	$(OCAMLBUILD) fuzlog.cma

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean

.PHONY: test
test:
	$(OCAMLBUILD) ${TEST_BYTE}
	for i in ${TEST_BYTE} ; do ./`basename $$i` ; done
