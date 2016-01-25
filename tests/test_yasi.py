#!/usr/bin/env python
# coding: utf-8

# Disable pylint warnings on methods without docstrings and too many public methods
# pylint: disable=C0111,R0904,C0413

""" Test suite for yasi
"""

import os
import sys
import unittest

PROJECT_DIR = os.path.split(os.path.abspath(os.path.dirname(__file__)))[0]
sys.path.insert(0, PROJECT_DIR)

import yasi  # noqa


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

    def test_all_whitespace_spaces_only(self):
        source = '           '
        self.assertTrue(yasi.all_whitespace(source))

    def test_all_whitespace_no_whitespace(self):
        source = 'karamba!!'
        self.assertFalse(yasi.all_whitespace(source))

    def test_all_whitespace_empty_string(self):
        source = ''
        self.assertTrue(yasi.all_whitespace(source))

    def test_all_whitespace_tabs_and_spaces(self):
        source = '	    	    	    	    	    	    		'
        self.assertTrue(yasi.all_whitespace(source))

    def test_all_whitespace_tabs_only(self):
        source = '								'
        self.assertTrue(yasi.all_whitespace(source))

    def test_find_first_arg_pos1(self):
        source = "(     list 'one-sheep 'two-sheep )"
        self.assertEqual([11, 5], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos2(self):
        source = "(    list 'one-sheep 'two-sheep )"
        self.assertEqual([10, 4], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos3(self):
        source = "   (    list 'one-sheep 'two-sheep )"
        self.assertEqual([10, 4], yasi.find_first_arg_pos(3, source))

    def test_find_first_arg_pos_argument_in_next_line_no_trailing_space(self):
        source = '(    list'
        self.assertEqual([5, 4], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_argument_in_next_line_no_spaces_before_func(self):
        source = '(list     '
        self.assertEqual([1, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_argument_is_func_call(self):
        source = '(list (* 12 13) (* 13 14)  '
        self.assertEqual([6, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_no_function1(self):
        source = '(        '
        self.assertEqual([1, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_no_function2(self):
        source = '('
        self.assertEqual([1, 0], yasi.find_first_arg_pos(0, source))

    def test_trim_separate_adjacent_opening_and_closing_brackets(self):
        source = ')('
        self.assertEqual(') (', yasi.trim(source))

    def test_trim_space_between_succeeding_opening_brackets(self):
        source = '( ( ( '
        self.assertEqual('(((', yasi.trim(source))

    def test_trim_adjacent_function_and_argument_opening_bracket(self):
        source = '(print(+ 1 1))'
        self.assertEqual('(print (+ 1 1))', yasi.trim(source))

    def test_trim_space_between_succeeding_closing_brackets(self):
        source = ') ) )'
        self.assertEqual(')))', yasi.trim(source))

    def test_trim_spaces_before_closing_brackets(self):
        source = '(print 12    )'
        self.assertEqual('(print 12)', yasi.trim(source))

    def test_trim_extra_whitespace(self):
        source = "(print       'this)"
        self.assertEqual("(print 'this)", yasi.trim(source))

    def test_trim_leading_whitespace(self):
        source = '       (exit)'
        self.assertEqual('(exit)', yasi.trim(source))

    def test_trim_spaces_between_quote_and_opening_bracket_in_list_literal(self):
        source = "'        (12 13 14)"
        self.assertEqual("'(12 13 14)", yasi.trim(source))

    def test_find_trim_limit_literal_double_quote(self):
        source = r'(list #\; #\")'
        self.assertEqual(len(source), yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote(self):
        source = '(list 1123 " ) " 542)'
        self.assertEqual(12, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_and_single_quote(self):
        source = """(list 1123 ' " ) " 542)"""
        self.assertEqual(14, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_after_semi_colon(self):
        source = """(list 1123 ; " )" ";" 542)"""
        self.assertEqual(10, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_after_comment_block(self):
        source = """(list 1123 '  #|  " ); "  |# 542) """
        self.assertEqual(19, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_before_semi_colon(self):
        source = """(list 1123 ' " ); " 542)"""
        self.assertEqual(14, yasi.find_trim_limit(source))

    def test_find_trim_limit_newlisp_brace_string_before_comment(self):
        source = '(println {            Hello     World                   }) ;; jjj'
        self.assertEqual(9, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_find_trim_limit_comment_alone_in_newlisp(self):
        source = 'thhjh h               jgjh             ;;            hjbjh'
        self.assertEqual(26, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_find_trim_limit_newlisp_brace_string(self):
        source = '(string         {   Hello world                }    " message"))'
        self.assertEqual(16, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_split_preserve_empty_lines_at_EOF(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo\n\n\n\n\n"
        self.assertEqual(['Tengo una pregunta\n',
                          'Sobre todo \n',
                          ' en este mundo\n',
                          '\n',
                          '\n',
                          '\n',
                          '\n'], yasi.split_preserve(source, '\n'))

    def test_split_preserve_no_line_ending_at_EOF(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo"
        self.assertEqual(['Tengo una pregunta\n',
                          'Sobre todo \n',
                          ' en este mundo'], yasi.split_preserve(source, '\n'))

    def test_split_preserve_no_delimiter(self):
        source = "Tengo una pregunta  Sobre todo    en este mundo  "
        self.assertEqual(["Tengo una pregunta  Sobre todo    en este mundo  "],
                         yasi.split_preserve(source, '\n'))

    def test_split_preserve(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo\n"
        self.assertEqual(['Tengo una pregunta\n',
                          'Sobre todo \n',
                          ' en este mundo\n'], yasi.split_preserve(source, '\n'))

    def test_is_macro_name_not_actual_macro(self):
        self.assertFalse(yasi.is_macro_name('files-with-code', 'lisp'))

    def test_is_macro_name_newlisp_macros(self):
        newlisp_macro_list = [
        ]
        for x in newlisp_macro_list:
            self.assertTrue(yasi.is_macro_name(x, 'newlisp'))

    def test_is_macro_name_scheme_macros(self):
        scheme_macro_list = [
        ]
        for x in scheme_macro_list:
            self.assertTrue(yasi.is_macro_name(x, 'scheme'))

    def test_is_macro_name_clojure_macros(self):
        clojure_macro_list = [
            "defmacro"
        ]
        for x in clojure_macro_list:
            self.assertTrue(yasi.is_macro_name(x, 'clojure'))

    def test_is_macro_name_lisp_macros(self):
        lisp_macro_list = [
            "defmacro",
            "define-macro",
            "defstruct"
        ]
        for x in lisp_macro_list:
            self.assertTrue(yasi.is_macro_name(x, 'lisp'))


class SystemTests(unittest.TestCase):
    def test_all_case_files(self):
        self.maxDiff = None
        cases = [
            {
                'before': 'tests/cases/#1-if-expression.lisp',
                'after': 'tests/cases/#1-if-expression~.lisp',
                'options': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#2-multiple-value-bind.lisp',
                'after': 'tests/cases/#2-multiple-value-bind~.lisp',
                'options': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#3-multiple-value-bind.lisp',
                'after': 'tests/cases/#3-multiple-value-bind~.lisp',
                'options': '--uniform --dialect=lisp'
            }, {
                'before': 'tests/cases/#4-flet-indentation.lisp',
                'after': 'tests/cases/#4-flet-indentation~.lisp',
                'options': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#5-looks-like-a-macro.lisp',
                'after': 'tests/cases/#5-looks-like-a-macro~.lisp',
                'options': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#6-default-indent.lisp',
                'after': 'tests/cases/#6-default-indent~.lisp',
                'options': '--dialect=lisp --default-indent 2'
            }, {
                'before': 'tests/cases/#7-uniform-if-expression.lisp',
                'after': 'tests/cases/#7-uniform-if-expression~.lisp',
                'options': '--dialect=lisp --uniform'
            }, {
                'before': 'tests/cases/#8-macrolet-special-operator.lisp',
                'after': 'tests/cases/#8-macrolet-special-operator~.lisp',
                'options': '--dialect=lisp --indent-comments'
            }, {
                'before': 'tests/cases/#9-standard-emacs-form-indentation.lisp',
                'after': 'tests/cases/#9-standard-emacs-form-indentation~.lisp',
                'options': '--dialect=all'
            }, {
                'before': 'tests/cases/#10-newlisp-hash-comment.lsp',
                'after': 'tests/cases/#10-newlisp-hash-comment~.lsp',
                'options': '--dialect=newlisp'
            }, {
                'before': 'tests/cases/#11-gradual-space-reduction-reindentation.lsp',
                'after': 'tests/cases/#11-gradual-space-reduction-reindentation~.lsp',
                'options': '--dialect=newlisp'
            }, {
                'before': 'tests/cases/#12-zero-level-hanging-indentation.lsp',
                'after': 'tests/cases/#12-zero-level-hanging-indentation~.lsp',
                'options': '--dialect=newlisp --no-compact'
            }, {
                'before': 'tests/cases/#13-hanging-with-non-hanging.lsp',
                'after': 'tests/cases/#13-hanging-with-non-hanging~.lsp',
                'options': '--dialect=newlisp --no-compact'
            }, {
                'before': 'tests/cases/#14-tabbed-indentation.lsp',
                'after': 'tests/cases/#14-tabbed-indentation~.lsp',
                'options': '--dialect=newlisp --no-compact --tab=4'
            }, {
                'before': 'tests/cases/#15-input-space-output-tabs.lisp',
                'after': 'tests/cases/#15-input-space-output-tabs~.lisp',
                'options': '--dialect=lisp --no-compact --tab=4'
            }, {
                'before': 'tests/cases/#16-lisp-flets-and-labels.lisp',
                'after': 'tests/cases/#16-lisp-flets-and-labels~.lisp',
                'options': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#17-clojure-letfn.clj',
                'after': 'tests/cases/#17-clojure-letfn~.clj',
                'options': '--dialect=clojure'
            }
        ]
        for i in range(0, 17):
            case = cases[i]
            before_path = os.path.join(PROJECT_DIR, case['before'])
            after_path = os.path.join(PROJECT_DIR, case['after'])
            before = yasi.read_file(before_path)
            after = yasi.read_file(after_path)
            options = '--no-rc ' + case['options']
            result = yasi.indent_code(before, options=options)
            indented_code = ''.join(result['indented_code'])
            try:
                self.assertEqual(indented_code, after)
            except AssertionError as exception:
                print('??? Test failed: ' + case['before'] + '\n')
                print(exception)

if __name__ == '__main__':
    unittest.main()
