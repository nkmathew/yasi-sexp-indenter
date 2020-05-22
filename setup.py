#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Setup for yasi """

import io
import sys
import ast
import setuptools

README = ''
with io.open('README.md') as f:
    README = f.read()

DEPS = []

if sys.version_info[:2] != (3, 4):
    DEPS += ['colorama']

if sys.version_info < (2, 7):
    DEPS += ['argparse']

def version():
    """Return version string."""
    with io.open('yasi.py') as input_file:
        for line in input_file:
            if line.startswith('__version__'):
                return ast.parse(line).body[0].value.s


setuptools.setup(
    author="Mathew Ng'etich",
    author_email='kipkoechmathew@gmail.com',
    description='A dialect aware s-expression indenter',
    download_url="https://github.com/nkmathew/yasi-sexp-indenter/zipball/master",
    entry_points={'console_scripts': ['yasi = yasi:main']},
    install_requires=DEPS,
    keywords='scheme, formatter, newlisp, beautifier, clojure, lisp, indenter',
    long_description=README,
    long_description_content_type='text/markdown',
    name='yasi',
    py_modules=['yasi'],
    test_suite='test.test_yasi',
    url='https://github.com/nkmathew/yasi-sexp-indenter',
    version=version(),
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
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Topic :: Software Development :: Quality Assurance',
    ]
)
