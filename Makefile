PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"

test:
	@${PYTHON} tests/test_yasi.py
	@newlisp tests/test-yasi-module.lsp

tags: yasi.py
	ctags yasi.py

checks:
	pep8 yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py

clean:
	rm -rf __pycache__ tags *.pyc

.PHONY:
	all clean
