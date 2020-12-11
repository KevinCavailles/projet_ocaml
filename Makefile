
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

demo_advanced: build
	@echo "\n==== EXECUTING ====\n"
	./ftest_advanced.native graphs/graph2 graphs/graph4
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat graphs/graph4

demo_basic: build
	@echo "\n==== EXECUTING ====\n"
	./ftest_basic.native graphs/graph1 graphs/graph3 1 5
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat graphs/graph3

clean:
	-rm -rf _build/
	-rm ftest_advanced.native

clean_basic:
	-rm -rf _build/
	-rm ftest_basic.native

clean_advanced:
	-rm -rf _build/
	-rm ftest_advanced.native
