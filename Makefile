PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"
.PHONY : all

py-test:
	@${PYTHON} tests/test_yasi.py
.PHONY : py-test

new-test:
	@newlisp tests/test-yasi-module.lsp
.PHONY : new-test

test: py-test new-test
.PHONY : test

tags: yasi.py
	ctags yasi.py
.PHONY : tags

checks:
	pep8 yasi.py tests/test_yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py tests/test_yasi.py
.PHONY : checks

clean:
	rm -rf __pycache__ tags *.pyc *.bak~ tests/cases/*.bak~
.PHONY : clean

help:
	@echo "Valid targets:"
	@echo " ... all(default target)"
	@echo " ... py-test"
	@echo " ... new-test"
	@echo " ... test"
	@echo " ... checks"
	@echo " ... clean"
.PHONY : help
