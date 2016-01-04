#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Setup for yasi """

import yasi
import sys
import setuptools

setuptools.setup(
    name='yasi',
    version=yasi.__version__,
    description='A dialect aware s-expression indenter',
    author='Mathew Ngetich',
    author_email='kipkoechmathew@gmail.com',
    download_url="https://github.com/nkmathew/yasi-sexp-indenter/zipball/master",
    url='https://github.com/nkmathew/yasi-sexp-indenter',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: Public Domain',
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
    keywords='lisp, scheme, newlisp, indenter, formatter',
    test_suite='test.test_yasi',
    py_modules=['yasi'],
    install_requires=['argparse'] if sys.version_info < (2, 7) else [],
    entry_points={'console_scripts': ['yasi = yasi:main']}
)
