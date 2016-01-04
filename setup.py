#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Setup for yasi """

import io
import sys
import ast
import setuptools

README = ''
with io.open('README.rst') as f:
    README = f.read()

DEPS = ['colorama'] + (['argparse'] if sys.version_info < (2, 7) else [])

def version():
    """Return version string."""
    with io.open('yasi.py') as input_file:
        for line in input_file:
            if line.startswith('__version__'):
                return ast.parse(line).body[0].value.s


setuptools.setup(
    name='yasi',
    version=version(),
    description='A dialect aware s-expression indenter',
    long_description=README,
    author="Mathew Ng'etich",
    author_email='kipkoechmathew@gmail.com',
    download_url="https://github.com/nkmathew/yasi-sexp-indenter/zipball/master",
    url='https://github.com/nkmathew/yasi-sexp-indenter',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Intended Audience :: Developers',
        "License :: OSI Approved",
        "License :: OSI Approved :: MIT License",
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Topic :: Software Development :: Quality Assurance',
    ],
    keywords='scheme, formatter, newlisp, beautifier, clojure, lisp, indenter',
    test_suite='test.test_yasi',
    py_modules=['yasi'],
    install_requires=DEPS,
    entry_points={'console_scripts': ['yasi = yasi:main']}
)
