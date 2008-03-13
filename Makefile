.PHONY: ex
ex:
	ocamlbuild \
		example/ex_simple_inference.byte

.PHONY: build
build:
	ocamlbuild fuzlog.cma

.PHONY: clean
clean:
	ocamlbuild -clean

.PHONY: test
test: build
	ocamlbuild tools/make_suite.byte
	cd test && ../make_suite.byte *.ml > tests.ml && cd -
	ocamlbuild -I fuzlog -lib fuzlog test/tests.byte
	./tests.byte
