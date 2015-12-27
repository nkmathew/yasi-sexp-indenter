PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"
.PHONY : check

python-test:
	@${PYTHON} tests/test_yasi.py
.PHONY : check

newlisp-test:
	@newlisp tests/test-yasi-module.lsp
.PHONY : check

test: python-test newlisp-test
.PHONY : check

tags: yasi.py
	ctags yasi.py
.PHONY : check

checks:
	pep8 yasi.py tests/test_yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py tests/test_yasi.py
.PHONY : check

clean:
	rm -rf __pycache__ tags *.pyc
.PHONY : check

help:
	@echo "Valid targets:"
	@echo " ... all(default target)"
	@echo " ... python-test"
	@echo " ... newlisp-test"
	@echo " ... test"
	@echo " ... checks"
	@echo " ... clean"
.PHONY : help
