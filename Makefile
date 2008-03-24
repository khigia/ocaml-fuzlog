OCAMLBUILD=ocamlbuild -classic-display

.PHONY: ex
ex:
	$(OCAMLBUILD) \
		example/ex_simple_inference.byte

.PHONY: build
build:
	$(OCAMLBUILD) fuzlog.cma

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean

.PHONY: test
test: build
	$(OCAMLBUILD) tools/make_suite.byte
	cd test && ../make_suite.byte *.ml > tests.ml && cd -
	$(OCAMLBUILD) -I fuzlog -lib fuzlog test/tests.byte
	./tests.byte
