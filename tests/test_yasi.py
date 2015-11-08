#!/usr/bin/env python
# coding: utf-8

""" Test suite for yasi
"""

import os
import re
import sys
import shutil

if sys.version_info < (2, 7):
    import unittest2 as unittest
else:
    import unittest

PROJECT_DIR = os.path.split(os.path.abspath(os.path.dirname(__file__)))[0]
sys.path.insert(0, PROJECT_DIR)

import yasi

class UnitTests(unittest.TestCase):

    def test_find_lf_line_ending(self):
        source = 'silent realm\n the lost dimension\n'
        self.assertEqual(yasi.LF, yasi.line_ending(source))

    def test_find_crlf_line_ending(self):
        source = 'silent realm\r\n the lost dimension\r\n'
        self.assertEqual(yasi.CRLF, yasi.line_ending(source))

    def test_find_cr_line_ending(self):
        source = 'silent realm\r the lost dimension\r'
        self.assertEqual(yasi.CR, yasi.line_ending(source))

    def test_find_cr_wth_lf_mixed_line_ending(self):
        source = 'silent realm\r the lost dimension\n the lonely angel\n end \n\r'
        self.assertEqual(yasi.CR, yasi.line_ending(source))


class SystemTests(unittest.TestCase):
    pass

if __name__ == '__main__':
    unittest.main()
