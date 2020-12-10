
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest_advanced.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

basic:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest_basic.native

advanced:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest_advanced.native

demo: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph1 1 2 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

clean:
	-rm -rf _build/
	-rm ftest_advanced.native

clean_basic:
	-rm -rf _build/
	-rm ftest_basic.native

clean_advanced:
	-rm -rf _build/
	-rm ftest_advanced.native
