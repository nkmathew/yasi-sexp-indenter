#!/usr/bin/env python
# coding: utf-8

# Disable pylint warnings on methods without docstrings and too many public methods
# pylint: disable=C0111,R0904,C0413

""" Test suite for yasi
"""

import os
import re
import sys
import traceback
import unittest

try:
    import colorama
    colorama.init()
except ImportError:
    pass


PROJECT_DIR = os.path.split(os.path.abspath(os.path.dirname(__file__)))[0]
sys.path.insert(0, PROJECT_DIR)

import yasi  # noqa


def addcolour(message):
    Fore = colorama.Fore
    message = message.split('\n')
    nmessage = ''
    for line in message:
        if line.startswith('+ '):
            nmessage += '%s%s%s' % (Fore.GREEN, line, Fore.WHITE)
        elif line.startswith('? '):
            nmessage += '%s%s%s' % (Fore.YELLOW, line, Fore.WHITE)
        elif line.startswith('- '):
            nmessage += '%s%s%s' % (Fore.RED, line, Fore.WHITE)
        elif line[:4] in ['====', '....', 'FAIL']:
            nmessage += '%s%s%s' % (Fore.CYAN, line, Fore.WHITE)
        else:
            nmessage += line
        nmessage += '\n'

    def testname(match):
        funcname = Fore.CYAN + match.group(1) + Fore.WHITE
        return ', in %s' % funcname
    nmessage = re.sub(r', in (test_[\w\d]+)', testname, nmessage)
    return nmessage


class UnitTests(unittest.TestCase):

    def assertEq(self, left, right):
        try:
            self.assertEqual(left, right)
        except AssertionError as ex:
            testname = traceback.format_stack()[-2].split('\n')[0]
            testname = '\n\n%s\n\n%s' % (testname, str(ex))
            try:
                print(addcolour(testname))
            except NameError:
                print(testname)

    def test_find_line_ending_only_lf(self):
        source = 'First Line\n Second Line\n'
        self.assertEq(yasi.LF, yasi.find_line_ending(source))

    def test_find_line_ending_only_crlf(self):
        source = 'First Line\r\n Second Line\r\n'
        self.assertEq(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_only_cr(self):
        source = 'First Line\r Second Line\r'
        self.assertEq(yasi.CR, yasi.find_line_ending(source))

    def test_find_line_ending_lf_with_cr_and_crlf(self):
        source = 'First Line\r Second Line\r\n Third Line\n Fourth Line \r\n'
        self.assertEq(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_lf_with_crlf(self):
        source = 'First Line\n Second Line\r\n Third Line\n Fourth Line \r\n'
        self.assertEq(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_cr_with_crlf(self):
        source = 'First Line\r Second Line\r\n Third Line\r Fourth Line \r\n'
        self.assertEq(yasi.CRLF, yasi.find_line_ending(source))

    def test_find_line_ending_cr_with_lf(self):
        source = 'First Line\r Second Line\n Third Line\n Fourth Line \n\r'
        self.assertEq(yasi.CR, yasi.find_line_ending(source))

    def test_find_line_ending_should_default_to_lf(self):
        source = 'Line without ending'
        self.assertEq(yasi.LF, yasi.find_line_ending(source))

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
        self.assertEq([11, 5], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos2(self):
        source = "(    list 'one-sheep 'two-sheep )"
        self.assertEq([10, 4], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos3(self):
        source = "   (    list 'one-sheep 'two-sheep )"
        self.assertEq([10, 4], yasi.find_first_arg_pos(3, source))

    def test_find_first_arg_pos_argument_in_next_line_no_trailing_space(self):
        source = '(    list'
        self.assertEq([5, 4], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_argument_in_next_line_no_spaces_before_func(self):
        source = '(list     '
        self.assertEq([1, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_argument_is_func_call(self):
        source = '(list (* 12 13) (* 13 14)  '
        self.assertEq([6, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_no_function1(self):
        source = '(        '
        self.assertEq([1, 0], yasi.find_first_arg_pos(0, source))

    def test_find_first_arg_pos_no_function2(self):
        source = '('
        self.assertEq([1, 0], yasi.find_first_arg_pos(0, source))

    def test_trim_separate_adjacent_opening_and_closing_brackets(self):
        source = ')('
        self.assertEq(') (', yasi.trim(source))

    def test_trim_space_between_succeeding_opening_brackets(self):
        source = '( ( ( '
        self.assertEq('(((', yasi.trim(source))

    def test_trim_adjacent_function_and_argument_opening_bracket(self):
        source = '(print(+ 1 1))'
        self.assertEq('(print (+ 1 1))', yasi.trim(source))

    def test_trim_space_between_succeeding_closing_brackets(self):
        source = ') ) )'
        self.assertEq(')))', yasi.trim(source))

    def test_trim_spaces_before_closing_brackets(self):
        source = '(print 12    )'
        self.assertEq('(print 12)', yasi.trim(source))

    def test_trim_extra_whitespace(self):
        source = "(print       'this)"
        self.assertEq("(print 'this)", yasi.trim(source))

    def test_trim_leading_whitespace(self):
        source = '       (exit)'
        self.assertEq('(exit)', yasi.trim(source))

    def test_trim_spaces_between_quote_and_opening_bracket_in_list_literal(self):
        source = "'        (12 13 14)"
        self.assertEq("'(12 13 14)", yasi.trim(source))

    def test_find_trim_limit_literal_double_quote(self):
        source = r'(list #\; #\")'
        self.assertEq(len(source), yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote(self):
        source = '(list 1123 " ) " 542)'
        self.assertEq(12, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_and_single_quote(self):
        source = """(list 1123 ' " ) " 542)"""
        self.assertEq(14, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_after_semi_colon(self):
        source = """(list 1123 ; " )" ";" 542)"""
        self.assertEq(10, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_after_comment_block(self):
        source = """(list 1123 '  #|  " ); "  |# 542) """
        self.assertEq(19, yasi.find_trim_limit(source))

    def test_find_trim_limit_double_quote_before_semi_colon(self):
        source = """(list 1123 ' " ); " 542)"""
        self.assertEq(14, yasi.find_trim_limit(source))

    def test_find_trim_limit_newlisp_brace_string_before_comment(self):
        source = '(println {            Hello     World                   }) ;; jjj'
        self.assertEq(10, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_find_trim_limit_comment_alone_in_newlisp(self):
        source = 'thhjh h               jgjh             ;;            hjbjh'
        self.assertEq(26, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_find_trim_limit_newlisp_brace_string(self):
        source = '(string         {   Hello world                }    " message"))'
        self.assertEq(17, yasi.find_trim_limit(source, '--dialect=newlisp'))

    def test_split_preserve_empty_lines_at_EOF(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo\n\n\n\n\n"
        self.assertEq(['Tengo una pregunta\n',
                       'Sobre todo \n',
                       ' en este mundo\n',
                       '\n',
                       '\n',
                       '\n',
                       '\n'], yasi.split_preserve(source, '\n'))

    def test_split_preserve_no_line_ending_at_EOF(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo"
        self.assertEq(['Tengo una pregunta\n',
                       'Sobre todo \n',
                       ' en este mundo'], yasi.split_preserve(source, '\n'))

    def test_split_preserve_no_delimiter(self):
        source = "Tengo una pregunta  Sobre todo    en este mundo  "
        self.assertEq(["Tengo una pregunta  Sobre todo    en este mundo  "],
                      yasi.split_preserve(source, '\n'))

    def test_split_preserve(self):
        source = "Tengo una pregunta\nSobre todo \n en este mundo\n"
        self.assertEq(['Tengo una pregunta\n',
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
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#2-multiple-value-bind.lisp',
                'after': 'tests/cases/#2-multiple-value-bind~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#3-multiple-value-bind.lisp',
                'after': 'tests/cases/#3-multiple-value-bind~.lisp',
                'args': '--uniform --dialect=lisp'
            }, {
                'before': 'tests/cases/#4-flet-indentation.lisp',
                'after': 'tests/cases/#4-flet-indentation~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#5-looks-like-a-macro.lisp',
                'after': 'tests/cases/#5-looks-like-a-macro~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#6-default-indent.lisp',
                'after': 'tests/cases/#6-default-indent~.lisp',
                'args': '--dialect=lisp --default-indent 2'
            }, {
                'before': 'tests/cases/#7-uniform-if-expression.lisp',
                'after': 'tests/cases/#7-uniform-if-expression~.lisp',
                'args': '--dialect=lisp --uniform'
            }, {
                'before': 'tests/cases/#8-macrolet-special-operator.lisp',
                'after': 'tests/cases/#8-macrolet-special-operator~.lisp',
                'args': '--dialect=lisp --indent-comments'
            }, {
                'before': 'tests/cases/#9-standard-emacs-form-indentation.lisp',
                'after': 'tests/cases/#9-standard-emacs-form-indentation~.lisp',
                'args': '--dialect=all'
            }, {
                'before': 'tests/cases/#10-newlisp-hash-comment.lsp',
                'after': 'tests/cases/#10-newlisp-hash-comment~.lsp',
                'args': '--dialect=newlisp'
            }, {
                'before': 'tests/cases/#11-gradual-space-reduction-reindentation.lsp',
                'after': 'tests/cases/#11-gradual-space-reduction-reindentation~.lsp',
                'args': '--dialect=newlisp'
            }, {
                'before': 'tests/cases/#12-zero-level-hanging-indentation.lsp',
                'after': 'tests/cases/#12-zero-level-hanging-indentation~.lsp',
                'args': '--dialect=newlisp --no-compact'
            }, {
                'before': 'tests/cases/#13-hanging-with-non-hanging.lsp',
                'after': 'tests/cases/#13-hanging-with-non-hanging~.lsp',
                'args': '--dialect=newlisp --no-compact'
            }, {
                'before': 'tests/cases/#14-tabbed-indentation.lsp',
                'after': 'tests/cases/#14-tabbed-indentation~.lsp',
                'args': '--dialect=newlisp --no-compact --tab=4'
            }, {
                'before': 'tests/cases/#15-input-space-output-tabs.lisp',
                'after': 'tests/cases/#15-input-space-output-tabs~.lisp',
                'args': '--dialect=lisp --no-compact --tab=4'
            }, {
                'before': 'tests/cases/#16-lisp-flets-and-labels.lisp',
                'after': 'tests/cases/#16-lisp-flets-and-labels~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#17-clojure-letfn.clj',
                'after': 'tests/cases/#17-clojure-letfn~.clj',
                'args': '--dialect=clojure'
            }, {
                'before': 'tests/cases/#18-letfn-binding-block-indentation-only.clj',
                'after': 'tests/cases/#18-letfn-binding-block-indentation-only~.clj',
                'args': '--dialect=clojure'
            }, {
                'before': 'tests/cases/#19-hash-quoted-expressions.lisp',
                'after': 'tests/cases/#19-hash-quoted-expressions~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#20-unclosed-string.lisp',
                'after': 'tests/cases/#20-unclosed-string~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#21-closing-brackets-separate-lines.lisp',
                'after': 'tests/cases/#21-closing-brackets-separate-lines~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#22-defmacro-example.lisp',
                'after': 'tests/cases/#22-defmacro-example~.lisp',
                'args': '--dialect=lisp'
            }, {
                'before': 'tests/cases/#23-newlisp-long-string-tag-spacing.lsp',
                'after': 'tests/cases/#23-newlisp-long-string-tag-spacing~.lsp',
                'args': '--dialect=newlisp'
            }
        ]
        for i in range(0, 23):
            case = cases[i]
            before_path = os.path.join(PROJECT_DIR, case['before'])
            after_path = os.path.join(PROJECT_DIR, case['after'])
            before = yasi.read_file(before_path)
            after = yasi.read_file(after_path)
            args = '--no-rc ' + case['args']
            result = yasi.indent_code(before, args=args)
            indented_code = ''.join(result['indented_code'])
            try:
                self.assertEqual(indented_code, after)
            except AssertionError as exception:
                message = '\nFAIL: %s\n\n%s' % (case['before'], exception)
                try:
                    print(addcolour(message))
                except NameError:
                    print(message)


if __name__ == '__main__':
    unittest.main()
