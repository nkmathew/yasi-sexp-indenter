PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"

python-test:
	@${PYTHON} tests/test_yasi.py

newlisp-test:
	@newlisp tests/test-yasi-module.lsp

test: python-test newlisp-test

tags: yasi.py
	ctags yasi.py

checks:
	pep8 yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py

clean:
	rm -rf __pycache__ tags *.pyc

.PHONY:
	all clean checks python-test newlisp-test
