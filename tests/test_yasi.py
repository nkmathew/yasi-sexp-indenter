#!/usr/bin/env python
# coding: utf-8

"""Test suite for yasi
"""

import os
import re
import sys
import shutil

if sys.version_info < (2, 7):
    import unittest2 as unittest
else:
    import unittest

class UnitTests(unittest.TestCase):
    pass

class SystemTests(unittest.TestCase):
    pass
