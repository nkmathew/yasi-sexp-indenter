#!/usr/bin/env python
# coding: utf-8

""" Test suite for yasi
"""

import os
import re
import sys
import shutil
import unittest

PROJECT_DIR = os.path.split(os.path.abspath(os.path.dirname(__file__)))[0]
sys.path.insert(0, PROJECT_DIR)

import yasi

class UnitTests(unittest.TestCase):

    def test_find_line_ending_only_lf(self):
        source = 'First Line\n Second Line\n'
        self.assertEqual(yasi.LF, yasi.find_line_ending(source))

    def test_find_line_ending_only_crlf(self):
        source = 'First Line\r\n Second Line\r\n'
        self.assertEqual(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_only_cr(self):
        source = 'First Line\r Second Line\r'
        self.assertEqual(yasi.CR, yasi.find_line_ending(source))

    def test_find_line_ending_lf_with_cr_and_crlf(self):
        source = 'First Line\r Second Line\r\n Third Line\n Fourth Line \r\n'
        self.assertEqual(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_lf_with_crlf(self):
        source = 'First Line\n Second Line\r\n Third Line\n Fourth Line \r\n'
        self.assertEqual(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_cr_with_crlf(self):
        source = 'First Line\r Second Line\r\n Third Line\r Fourth Line \r\n'
        self.assertEqual(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_cr_with_lf(self):
        source = 'First Line\r Second Line\n Third Line\n Fourth Line \n\r'
        self.assertEqual(yasi.CR, yasi.find_line_ending(source))

    def test_find_line_ending_should_default_to_lf(self):
        source = 'Line without ending'
        self.assertEqual(yasi.LF, yasi.find_line_ending(source))

class SystemTests(unittest.TestCase):
    pass

if __name__ == '__main__':
    unittest.main()
