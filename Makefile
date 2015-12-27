PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"
.PHONY : all

python-test:
	@${PYTHON} tests/test_yasi.py
.PHONY : python-test

newlisp-test:
	@newlisp tests/test-yasi-module.lsp
.PHONY : newlisp-test

test: python-test newlisp-test
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
	rm -rf __pycache__ tags *.pyc
.PHONY : clean

help:
	@echo "Valid targets:"
	@echo " ... all(default target)"
	@echo " ... python-test"
	@echo " ... newlisp-test"
	@echo " ... test"
	@echo " ... checks"
	@echo " ... clean"
.PHONY : help
