PYTHON?=python

all:
	@echo "Usage:"
	@echo "    make test"

test: tests/test_yasi.py
	@echo Running unit tests...
	@${PYTHON} $^

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
