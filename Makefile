PYTHON?=python

ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

all: help
.PHONY : all

egg: rst
	${PYTHON} setup.py sdist
.PHONY : egg

dist:
	${PYTHON} setup.py sdist upload
.PHONY : dist

README.rst: README.md
	pandoc --from markdown --to rst --output README.rst README.md

rst: README.rst
.PHONY : rst

htm: README.html
.PHONY : htm

html: README.html
.PHONY : html

README.html: README.rst
	rst2html.py -stg README.rst README.html

py-test:
	@${PYTHON} tests/test_yasi.py
.PHONY : py-test

deps:
	-pip install -r $(ROOT_DIR)/requirements.txt
.PHONY : deps

new-test:
	@newlisp tests/test-yasi-module.lsp
.PHONY : new-test

test: py-test new-test
.PHONY : test

tags: yasi.py
	ctags yasi.py

checks:
	pep8 yasi.py tests/test_yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py tests/test_yasi.py
.PHONY : checks

clean:
	rm -rf __pycache__ tags *.pyc *.bak~ tests/cases/*.bak~
.PHONY : clean

help:
	@echo "Targets:"
	@echo " -> test(new-test, py-test)"
	@echo " -> checks"
	@echo " -> clean"
	@echo " -> egg"
	@echo " -> dist"
	@echo " -> tags"
	@echo " -> deps"
	@echo " -> rst"
	@echo " -> htm(html)"
.PHONY : help
