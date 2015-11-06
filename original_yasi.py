#-*- encoding: utf-8 -*-
__author__  = "nkmathew <kipkoechmathew@gmail.com>"
__date__    = "4th December 2013"
__started__ = "1st October 2013"

"""
@Started: 1st October 2013
@Good version: 4th November 2013

This is the first version of Yasi. It's a bit of a mess compared to the
refactored one. I had to rewrite it because pychecker wasn't pleased with it.
It kept complaining about excessively long functions, too many branches in the
program too many local variables...

I decided to keep it so that I could appreciate how much better the refactored
one is and how awfull my organisation was when I began the project. That doesn't
mean that it doesn't work correctly. It works as perfectly as the new one and
the newLISP version. All the concepts implemented in the new yasi were copied
here

It's downsides are:
    + You can't import it because it needs a file in sys.argv to indent
    + You can't pycheck it without hardcoding the filename
"""

import re
import os
import sys
import time    # to tell you the last time you indented a file.
import shutil  # to a backup file
 # haslib is used to determine if a file has already been formatted by comparing
 # the hashes of before and after. It's really not that necessary though since
 # you can simply compare the strings.
import hashlib

if sys.argv[1:] == []:
    print  '''
 _________________________________________________________________________________________________________________
|    Usage: newlisp yasi.lsp [[<file>] [--backup-dir<directory>] [--no-compact] [--no-backup] [--no-warning]      |
|                   [--clojure] [--lisp] [--scheme] [--default-indent <num>]]                                     |
|            -nb,    --no-backup     # Don't create a backup file even if --backup-dir is specified               |
|            -nc,    --no-compact    # Try to preserve the structure of the file.                                 |
|            -nw,    --no-warning    # Don't issue warnings                                                       |
|            -ne,    --no-exit       # Instructs the program not to exit when a warning is raised. True by default|
|            -uni,   --uniform       # Dictates whether the if-clause and else-clause of an if-like block should  |
|                                       have the same indent level. False by default                              |
|            -no,    --no-output     # Suppress output of the indented code                                       |
|            -nm,    --no-modify     # Don't modify the file                                                      |
|            --backup-dir            # The directory where the backup file is to be written                       |
|            --clojure               # Use Clojure keywords                                                       |
|            --lisp                  # Use Lisp keywords                                                          |
|            --newlisp               # Use newLISP keywords                                                       |
|            --scheme                # Use Scheme keywords                                                        |
|            --default-indent <num>  # The indent level to be used in case a                                      |
|                                    function's argument is in the next line. Vim uses 2, the most common being 1.|
|            --indent-comments, -ic  # False by default. If true, comment lines will be indented possibly         |
|                                        messing with any deliberate comment layout                               |
+-----------------------------------------------------------------------------------------------------------------+
'''
    exit()

def read_file(path):
    '''
        Read in binary mode in order to get CRLF as \r\n and not \n and CR as \r
        instead of \n(LF).
    '''
    with open(path, "rb") as fp:
            return fp.read()

def current_time():
    '''
        Returns the current time e.g 14:28:04 . It's definitely overkill, but hey
        it doesn't hurt to know the last time you indented the file especially
        when running it from SciTE
    '''
    return time.strftime("%X", (time.localtime()))

## Assign default values here depending on the command line arguments 
_BACKUP   =  False if '--no-backup'  in sys.argv or '-nb' in sys.argv else True
_COMPACT  =  False if '--no-compact' in sys.argv or '-nc' in sys.argv else True
_EXIT     =  False if '--no-exit'    in sys.argv or '-ne' in sys.argv else True
_MODIFY   =  False if '--no-modify'  in sys.argv or '-nm' in sys.argv else True
_OUTPUT   =  False if '--no-output'  in sys.argv or '-no' in sys.argv else True
_UNIFORM  =  True  if '--uniform'    in sys.argv or '-uni' in sys.argv else False
_WARNINGS =  False if '--no-warning' in sys.argv or '-nw' in sys.argv else True
_INDENT_COMMENTS = True if '--indent-comments' in sys.argv or '-ic' in sys.argv else False


_DEFAULT_INDENT = 1
if '--default-indent' in sys.argv:
    ## The default is the value to be used in case a functions's argument is in
    ## the next line and that function is not a two or one space indenter
    pos = sys.argv.index('--default-indent')
    if len(sys.argv) > pos:
        _DEFAULT_INDENT = int(sys.argv[pos + 1])

_DIALECT = "All"
if "--clojure" in sys.argv:
    _DIALECT  = "Clojure"
elif "--scheme" in sys.argv:
    _DIALECT = "Scheme"
elif "--newlisp" in sys.argv:
    _DIALECT = "newLISP"
elif "--lisp" in sys.argv:
    _DIALECT = "Common Lisp"

# Default backup directory is the current working directory
backup_dir = os.getcwd()

# The filename to be formatted should be specified as the first argument to make
# things easier for me

fpath = sys.argv[1]
fname = os.path.split(fpath)[1] 
warn = sys.stderr.write
if not os.path.exists(fpath):
    warn("\n--%s-- Filename `%s' does not exist, aborting. . ."
            % (current_time(), fpath))
    exit()

code = read_file(fpath)

fhash = hashlib.md5(code).hexdigest()

def trim_extra_whitespace(string):
    ''' Uses every usefull hack to try and reduce extra whitespace without 
    messing with character literals
    '''
    ## turn "(print(+ 1 1))" to "(print (+ 1 1))"
    string = re.sub(r'''([^\\(\[,{@~`^'#])(\(|\[|{)''', r'\1 \2', string, re.X)
    ## turn  ")(" to ") ("
    string = re.sub(r'(\)|\]|})(\[|\(|{)',r'\1 \2',string)
    ## Remove any space before closing brackets "(print 12   )" ==> "(print 12)"
    string = re.sub('[ \t]*(\)|\]|})',r'\1', string)
    ## remove extra whitespace "(print     'this) ==> "(print 'this)"
    string = re.sub('[ \t]{2,}', ' ', string)
    ## turn ") ) ) " into "))) "
    string = re.sub(r"(\))[ \t]*(?=(\)))", r"\1", string)
    string = re.sub(r"(\])[ \t]*(?=(\]))", r"\1", string)
    string = re.sub(r"(})[ \t]*(?=(}))", r"\1", string)
    ## turn "( ( ( " into "((( "
    string = re.sub(r"(\()[ \t]*(?=(\())", r"\1", string)
    string = re.sub(r"(\[)[ \t]*(?=(\[))", r"\1", string)
    string = re.sub(r"({)[ \t]*(?=({))", r"\1", string)
    ## remove leading whitespace "   print" ==> "print"
    string = re.sub('^[ \t]*','',string)
    ## Remove space before list literal, " ' (1 2 3)" ==> " '(1 2 3)"
    string = re.sub(r" ('|`) (\(|\[|{)", r" \1\2",string)
    return string

def find_trim_limit(string):
    '''
    The function attempts to identify upto which point we are supposed to trim
    so that we don't mess with strings or any aligned comments.
    It does this by comparing the positions of semicolons and double 
    quotes. It doesn't consider the multiline comment marker. If your
    code uses multiline comments, you'll have to use --no-compact mode
    '''
    ## Find position of the first unescaped semi colon
    comment_start = re.search(r'([^\\];)|(^;)', string)
    ## Find position of the first unescaped double quote
    limit = re.search(r'([^\\]")|(^")', string)
    ## Assign -1 if there's no match
    limit = limit.end() if limit else -1
    comment_start = comment_start.end() if comment_start else -1
    if comment_start != -1:
        ## If semi colon is found, include all the whitespace before it so
        ## just in case someone had 'prettified' and aligned the comments
        comment_start = re.search("[ \t]*;", string).start() + 1

    if comment_start != -1 and limit != -1:
        if comment_start < limit:
            ## If the semicolon comes before the comma, it means the string
            ## has been commented out
            limit = comment_start
    elif comment_start != -1 and limit == -1:
        ## If there's a semicolon but no quote, use the semicolon position
        ## as the limit
        limit = comment_start
    elif limit == -1:
        ## If neither a semicolon nor a double quote have been found, use
        ## the length of the string as the limit
         limit = len(string)
    return limit



# Keywords that indent by two spaces
scheme_keywords =  ['define', 'local-odd?', 'when', 'begin', 'case',
'local-even?', 'do', 'call-with-bytevector-output-port', 'call-with-input-file',
'call-with-port', 'call-with-current-continuation', 'open-file-input-port',
'call-with-port', 'call-with-values', 'call-with-output-file',
'call-with-string-output-port', 'define-syntax', 'if', 'let', 'let*', 'library',
'unless', 'lambda', 'syntax-rules', 'syntax-case', 'let-syntax', 'letrec*',
'letrec', 'let-values', 'let*-values', 'with-exception-handler',
'with-input-from-file', 'with-interrupts-disabled', 'with-input-from-string',
'with-output-to-file', 'with-input-from-port', 'with-output-to-string',
'with-source-path', 'with-syntax', 'with-implicit', 'with-error-handler', 
'module', 'parameterize']

clojure_keywords = ['defn', 'fn', 'dorun', 'doseq', 'loop', 'when', 'let',
'defmacro', 'binding', 'doto', 'ns', ':import', 'defstruct', 'condp', 'comment',
'when', 'when-let', '->', '->>', 'extend-type', 'reify', 'binding', 'when-not',
'proxy', 'dotimes', 'try', 'finally', 'for', 'letfn', 'catch', 'iterate',
'while', 'with-local-vars', 'locking', 'defmulti', 'defmethod', 'extend'
]

lisp_keywords = [':implementation', ':method', 'case', 'defclass',
'defconstant', 'defgeneric', 'defimplementation', 'define-condition',
'define-implementation-package', 'definterface', 'defmacro', 'defmethod',
'defpackage', 'defproject', 'deftype', 'defun', 'defvar', 'do-external-symbols',
'dolist', 'dotimes', 'ecase', 'etypecase', 'flet', 'handler-bind', 'if',
'lambda', 'let', 'let*', 'print-unreadable-object', 'macrolet', 'defparameter',
'with-slots', 'typecase', 'loop', 'when', 'prog1', 'unless', 'with-open-file',
'with-output-to-string', 'with-input-from-string', 'block', 'handler-case',
'defstruct', 'eval-when', 'tagbody', 'ignore-errors', 'labels' 
]
 
newlisp_keywords = ['while', 'if', 'case', 'dotimes', 'define', 'dolist', 'catch', 
'throw', 'lambda', 'lambda-macro', 'when', 'unless', 'letex', 'letn', 'begin', 
'dostring', 'let', 'letn', 'doargs', "define-macro", 'until', 'do-until', 
'do-while', 'for-all', 'find-all', 'for' 
]

# Keywords that indent by one space
one_space_indenters = ['call-with-port']

# The 'if' and 'else' part of an if block should have different indent levels so
# that they can stand out since there's no else Keyword in Lisp/Scheme to make
# this explicit.  list if_like helps us track these keywords.
if_like = ["if"]

if _DIALECT == "Common Lisp": # Lisp
    two_space_indenters = lisp_keywords
    if_like+=["multiple-value-bind", "destructuring-bind", 'do', 'do*']
elif _DIALECT == "Scheme": # Scheme
    two_space_indenters = scheme_keywords
    if_like+=['with-slots', 'do', 'do*']
elif _DIALECT == "Clojure": # Clojure
    two_space_indenters = clojure_keywords
    if_like+=[]
elif _DIALECT == "newLISP": # newLISP
    two_space_indenters = newlisp_keywords
    if_like+=[]
elif _DIALECT == "All":
    two_space_indenters = lisp_keywords+scheme_keywords+clojure_keywords+ \
                        newlisp_keywords


def is_macro_name(form, dialect):
    ''' is_macro_name(form : str, dialect : str) -> bool

    example: is_macro_name("yacc:define-parser")
                True
        Tests if a word is a macro using the language's/dialect's convention,
        e.g macros in Lisp usually start with 'def' and 'with' in Scheme. Saves
        the effort of finding all the macros in Lisp/Scheme/Clojure/newLISP and
        storing them in a list.
    '''
    if not form:
        return form
    if dialect  == 'Common Lisp':
        return re.search('macro|def|do|with-', form, re.I)
    if dialect == 'Scheme':
        return re.search('call-|def|with-', form)
    if dialect == 'Clojure':
        return re.search('def|with', form)
    if dialect == 'newLISP':
        return re.search('macro|def', form)

# The list holds the locations of brackets and first arguments. When a
# closing bracket is encountered, the most recently pushed item is popped.
bracket_locations = []
indented_code=""

CR = "\r"
LF = "\n"
CRLF = CR+LF

if CRLF in code:
    line_ending = CRLF
elif CR in code:
    line_ending = CR
else:
    line_ending = LF

# split the whole string into a list so that you can iterate line by line.
code_lines = code.split(line_ending)
if code_lines[-1] == "":
    # If you split "this\nthat\n" you get ["this", "that", ""] if you add newlines to
    # every string in the list you get ["this\n", "that\n", "\n"]. 
    # Notice that you've just added another newline at the end of the file. We
    # try to avoid that here.
    del code_lines[-1]
    code_lines = map(lambda x:x+line_ending, code_lines)
else:
    # ["this", "that"] will become ["this\n", "that\n"] when mapped. A newline has been
    # added to the file. We don't want this, so we strip it below.
    code_lines = map(lambda x:x+line_ending, code_lines)
    code_lines[-1] = code_lines[-1].rstrip()


# Safeguards against processing brackets inside strings
in_string = False

# zero_level helps us get the same results as Sitaram's indenter when in
# --no-compact mode.
zero_level = 0

# newLISP uses curly brackets as a syntax for multiline strings 
# this variable here tries to keep track of that
in_newlisp_string = 0
newlisp_brace_locations = []
in_newlisp_tag_string = False
newlisp_brace_locations = []
first_tag_string = ()

# The two variables prevent formatting comment regions or symbols with whitespace
in_comment = 0
in_symbol_with_space = False
comment_locations = []
last_symbol_location = ()

# lacked a better word for a region that processing should not
# take place.
in_symbol_region = in_string or in_comment or in_symbol_with_space  or \
                    in_newlisp_string or in_newlisp_tag_string

# we need to know the line number in order to issue almost accurate messages
# about unclosed brackets and string
line_number = 1

last_quote_location = []

def comment_line_or_space(string):
    ## Returns true if the line is 'empty' or has a comment
    return re.search("^[ \t]*;", string) or re.search("^[ \t]*($|\r)", string)

for line in code_lines:
    escaped = False
    curr_line = line

    if not _COMPACT and zero_level == 0 and bracket_locations == [] and not in_comment:
        # The zero level is always calculated once because once set to a non
        # zero values, execution never reaches here.
        leading_spaces = re.search("^[ \t]+[^; )\n\r]", line)
        if leading_spaces:
            zero_level = leading_spaces.end()-1

    if in_symbol_region:
        # No processing done in strings and comments
        pass

    elif not in_string and bracket_locations!=[]:
        if not _COMPACT and not comment_line_or_space(curr_line) or _INDENT_COMMENTS:
            # --no-compact mode is on. add leading spaces according to the value
            # of the zero level.
            curr_line = ' '*zero_level+re.sub("^[ \t]+", '', curr_line, count=0, flags = re.M)

        if not comment_line_or_space(curr_line) or _INDENT_COMMENTS:
            # Split the string into two along a comment start, trim the first
            # portion and leave the comment untouched.
            trim_limit = find_trim_limit(curr_line)
            substr1 = curr_line[0:trim_limit]
            substr2 = curr_line[trim_limit:]
            substr1 = trim_extra_whitespace(substr1) if _COMPACT else substr1
            curr_line = substr1+substr2
            curr_line = ' '*(bracket_locations[-1]["indent_level"]-zero_level)+curr_line

    elif bracket_locations == []:
        # This happens when all the previous braces have been closed or when we
        # are at the first line in the file.
        if not _COMPACT and not comment_line_or_space(curr_line) or _INDENT_COMMENTS:
            curr_line = ' '*zero_level+re.sub("^[ \t]+", '', curr_line, count=0, flags = re.M)

        # Split the string into two along a comment start, trim the first
        # portion and leave the comment untouched.
        if not comment_line_or_space(curr_line) or _INDENT_COMMENTS:
            trim_limit = find_trim_limit(curr_line)
            substr1 = curr_line[0:trim_limit]
            substr2 = curr_line[trim_limit:]
            substr1 = trim_extra_whitespace(substr1) if _COMPACT else substr1
            curr_line = substr1+substr2

# ********************************************************************
    indented_code+=curr_line
    offset = 0 # number of characters from the beginning of the line

    for curr_char in curr_line:

        in_symbol_region = in_string or in_comment or in_symbol_with_space or \
                        in_newlisp_string or in_newlisp_tag_string

        prev_char = curr_line[offset-1:offset]
        prev_prev_char = curr_line[offset-2:offset-1]
        next_char = curr_line[offset+1:offset+2]

        substr = curr_line[offset+1:] # slice to the end

        if escaped:
            # Move to the next character if the current one has been escaped
            escaped = False
            offset += 1
            continue

        if curr_char == "\\" and not (in_newlisp_string or in_newlisp_tag_string):
            # the next character has been escaped
            escaped = True

        if curr_char == ';' and not in_symbol_region and not ((prev_char == "#"
                    and _DIALECT == "Scheme")):
            # a comment has been found, go to the next line
            break


        # ----------------------------------------------------------
        # Any symbols with spaces and multiline comment regions are skipped
        # with the help of the in_comment variable. Clojure does multiline comments
        # a different way so skip if the dialect is Clojure. It can be hard
        # to take care of comments in newLISP because it allows comments
        # like #| extends to eof |. This'll automatically trip the indenter 
        if _DIALECT not in ["Clojure", "newLISP"] and curr_char == '|' and not in_string and not \
                in_newlisp_string and not in_newlisp_tag_string:
            if prev_char == "#" and not in_symbol_with_space:
                #print "Found starter", line_number
                comment_locations.append((line_number, offset))
                in_comment += 1
            elif in_comment and next_char == "#":
                #print "Found closer", line_number
                in_comment -= 1
                comment_locations.pop()
            elif not in_comment:
                #print "Symbol ending", line_number
                if in_symbol_with_space:
                    last_symbol_location = ()
                    in_symbol_with_space = False
                else:
                    last_symbol_location = (line_number, offset)
                    in_symbol_with_space = True

        # ----------------------------------------------------------

        if not (in_symbol_with_space or in_comment or in_newlisp_tag_string):
            if curr_char == '"':
                last_quote_location = (fname, line_number, offset)
                in_string = True if not in_string else False
            if _DIALECT == 'newLISP' and not in_string:
                # We handle newLISP's multiline strings here
                if curr_char == '{':
                    newlisp_brace_locations.append((line_number, offset))
                    in_newlisp_string += 1
                elif curr_char == '}':
                    if newlisp_brace_locations:
                        newlisp_brace_locations.pop()
                    else:
                        if _WARNINGS:
                            warn("\n--%s-- `%s': Attempt to close a non-existent newLISP string" %
                                    (current_time(), fname))
                    in_newlisp_string -= 1

        if curr_char == "[" and _DIALECT == "newLISP" and not \
            (in_newlisp_string or in_string):
            ## We have to handle tag strings in newLISP here.
            if re.match("\[text\]", curr_line[offset:offset+7]):
                in_newlisp_tag_string = True
                if first_tag_string == ():
                    first_tag_string = (line_number, offset)
            elif re.match("\[/text\]", curr_line[offset:offset+7]):
                in_newlisp_tag_string = False
                first_tag_string = ()

        in_symbol_region = in_string or in_comment or in_symbol_with_space \
                            or in_newlisp_string or in_newlisp_tag_string

        if in_symbol_region:
            # move on if you are in a string, a symbol with space or a comment
            # altogether known as the symbol region
            offset += 1
            continue

        # Finds the real position of a bracket to be used in pinpointing where
        # the unclosed bracket is because the current offset is the position of
        # the bracket in the trimmed string not the original.
        real_position = (offset-zero_level)+len(re.findall("^[ \t]*", line)[0])

        if curr_char in ['(', '[', '{']:
            if curr_char in ['[', '{'] and _DIALECT in ["Common Lisp", "newLISP"]:
                ## Square brackets are used as symbols in CL and newLISP
                offset += 1
                continue
            leading_spaces = 0

            if re.search("^[ \t]*($|\r)", substr):
                # whitespace extending to the end of the line means there's no
                # function in this line. The indentation level defaults to one.
                first_arg_pos = 1

            else:
                if offset!=len(curr_line)-1 and curr_line[offset+1] == ' ':

                    # control reaches here if we are not at the end of the line
                    # and whitespace follows
                    match = re.search(" +[^)\]]", substr)
                    if match:
                        leading_spaces = match.end() - match.start() - 1
                        end = match.end()
                    else:
                        end = 0
                    first_arg_pos = re.search(" +([^)])|( *(\(|\[))", substr[end:])

                    if first_arg_pos:
                        first_arg_pos = first_arg_pos.end()+ leading_spaces+1
                    else:
                        first_arg_pos = leading_spaces + 1
                    if re.match("^[ \t]*(;|$|\r)", 
                        substr[end - 1 + substr[end - 1:].find(' '):]):
                        first_arg_pos = leading_spaces + _DEFAULT_INDENT

                else: # There's no space after the bracket.

                    match = re.search(" +([^)}\n\r])|( *(\(|\[|{))", substr)
                    if match: # found the argument
                        first_arg_pos = match.end()
                    else: # Either empty list or argument is in the next line
                        first_arg_pos = 1

                    if re.match("^[\t ]*(;|$|\r)", substr[substr.find(' '):]):
                        first_arg_pos = leading_spaces+_DEFAULT_INDENT

            func_name = substr[0:first_arg_pos-1].strip(')]\t\n\r ').lower()
            
            in_list_literal = False
            if re.search("('|`|#)([ \t]*\(|\[)($|\r)", curr_line[0:offset+1]):
                in_list_literal = True
            
            if re.search("^[^ \t]+[ \t]*($|\r)", substr):
                ## The function is the last symbol/form in the line
                func_name = substr.strip(')]\t\n\r ').lower()

            if func_name == '' or in_list_literal:
                # an empty string is always in a non-empty string, we don't want
                # this. We set False as the func_name because it's not a string
                # in_list_literal prevents an keyword in a list literal from
                # affecting the indentation
                func_name = False

            if func_name in ["define-macro", "defmacro"]:
                # Macro names are part of two_space_indenters space indenters.
                # This part tries to find the name so that it is not indented
                # like a function the next time it's used. 
                end_of_space = re.search("^[ \t]*", substr).end()
                substr = substr[end_of_space:]
                substr = substr[re.search("[ \t]*", substr).start():].strip()
                macro_name = substr[:substr.find(" ")] # macro name is delimeted by whitespace
                if macro_name!='':
                    two_space_indenters.append(macro_name)

            # first_item stores the position of the first item in the literal list
            # it's necessary so that we don't assume that the first item is always 
            # after the opening bracket.
            first_item = re.search("[ \t]*", curr_line[offset+1:]).end()+offset+1

            pos_hash = {"character":curr_char, 
                      "line_number":line_number, 
                      "bracket_pos":offset, 
                      "indent_level":offset+first_arg_pos, # the default value, e.g in normal function
                      "func_name":func_name, 
                      "spaces":0}

            if  in_list_literal or curr_char == '{' or (curr_char == '[' \
                        and _DIALECT == "Clojure"): # found quoted list or clojure hashmap/vector
                pos_hash["indent_level"] = first_item
            elif func_name in if_like:
                pos_hash["indent_level"] = leading_spaces+((offset+4) if not _UNIFORM else (offset+2))
            elif func_name in one_space_indenters and func_name != '':
                pos_hash["indent_level"] = leading_spaces+offset+1
            elif (func_name in two_space_indenters or is_macro_name(func_name, _DIALECT)) and func_name != '':
                pos_hash["indent_level"] = leading_spaces+offset+2

            bracket_locations.append(pos_hash)
            try:
                # A hack to make flets and labels in Lisp not indent like
                # functions. The 'labels' indentation may not be exactly
                # perfect.
                parent_func = bracket_locations[-3]["func_name"]
                if parent_func in ["flet", "labels", "macrolet"]:
                    bracket_locations[-1]["indent_level"] = offset+2
            except:
                pass

        elif curr_char in [']', ')', '}']:
            if curr_char in [']', '}'] and _DIALECT in ["Common Lisp", "newLISP"]:
                offset += 1
                continue
            if curr_char == ']':
                correct_closer='['
            elif curr_char == ')':
                correct_closer = '('
            else:
                correct_closer = '{'
            if bracket_locations != []:
                popped = bracket_locations.pop()
                popped_char = popped["character"]
                popped_pos = popped["line_number"]
                popped_offset  =  popped["bracket_pos"]
                if popped_char is not correct_closer:
                    warn("\n--%s-- %s: Warning: Bracket `%s' at (%d, %d) does not match `%s' at (%d, %d)" 
                    %(current_time(), fname, popped_char, popped_pos, popped_offset, curr_char, line_number, real_position))
                    if _EXIT:
                        exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))
            else:
                if _EXIT:
                    bpos = real_position+1
                else:
                    bpos = offset+1
                if _WARNINGS:
                    if _COMPACT:
                        warn("\n--%s-- %s: Warning: Unmatched `%s' near (%d, %d). " \
                        % (current_time(), fname, curr_char, line_number, bpos))
                    else:
                        warn("\n--%s-- %s: Warning: Unmatched brackets at (%d, %d). " \
                                % (current_time(), fname, line_number, bpos))
                    if _EXIT:
                        exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))

        if bracket_locations and curr_char in [' ', '\t'] and bracket_locations[-1]['func_name'] in if_like:
            ''' This part changes the indentation level of a then clause so that
                we can achieve something like: 
                        (if (= this that)
                            'then-form
                          'else-form)
                This is done by keeping track of the number of spaces found. If
                you find two spaces it means that, for example that we have just
                passed the then-form and hence should decrease the indentation
                level by 2.(I shamelessly copied this algorithm from Dorai's
                indenter)
            '''
            if prev_char not in [' ', '\t', ''] or  not \
                    re.search("^[ \t]*(;|#\||$|\r)", curr_line):
                # The level shouldn't be decreased if the line is a comment
                # line. The regex above takes care of that.
                bracket_locations[-1]['spaces']+=1
            if bracket_locations[-1]['spaces'] == 2:
                bracket_locations[-1]['indent_level']-=0 if _UNIFORM else 2
                bracket_locations[-1]['spaces'] = 999 # some dummy value to prevent it
                                            # from coming here again

        offset+=1
    line_number+=1

if bracket_locations:
    # If the bracket_locations list is not empty it means that there are some
    # brackets(opening) that haven't been closed. 
    for bracket in bracket_locations:
        y = bracket["line_number"]
        x = bracket["bracket_pos"]
        character = bracket["character"]
        if _WARNINGS:
            # The bracket_locations are not very accurate. The warning might be
            # misleading because it considers round and square brackets to be
            # the same.
            if _COMPACT and _EXIT:
                warn("\n--%s-- `%s': Warning : Unmatched `%s' near (%d, %d). " % 
                            (current_time(), fname, character, y, x))
            else:
                warn("\n--%s-- `%s': Warning : Unmatched `%s' at (%d, %d). " % 
                        (current_time(), fname, character, y, x))
            if _EXIT:
                exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))

if in_string:
    warn("\n--%s-- `%s': Warning: The string starting from (%d, %d) extends to end-of-file. "
            % ((current_time(),)+last_quote_location))
    if _EXIT:
        exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))


if last_symbol_location:
    warn("\n--%s-- `%s': Warning: Unclosed symbol near: (%d, %d). " % 
            ((current_time(), fname) + last_symbol_location))
    if _EXIT:
        exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))


if comment_locations:
    for comment in comment_locations:
        warn("\n--%s-- `%s': Warning: Unclosed comment near: (%d, %d)" % 
                ((current_time(), fname) + comment))
        if _EXIT:
            exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))

if first_tag_string:
    warn("\n--%s-- `%s': Warning: The tag string starting from (%d, %d) extends to end-of-file. "
            % ((current_time(), fname) + first_tag_string))
    if _EXIT:
        exit("\n--%s-- Exiting. File `%s' unchanged. . ." % (current_time(), fname))

if hashlib.md5(indented_code).hexdigest() == fhash:
    exit("\n--%s-- File `%s' has already been formatted. Leaving it unchanged. . ." % (current_time(), fname))
else:
    if '--backup-dir' in sys.argv[1:] and _BACKUP:
        backup_dir=sys.argv[sys.argv.index('--backup-dir')+1]
        if not os.path.exists(os.path.abspath(backup_dir)):
            warn( "\n--%s-- `%s': Warning: The directory `%s' does not exist and can't be used as a backup directory." 
                    % (current_time(), fname, backup_dir))
            if _EXIT:
                exit("\n--%s-- Exiting. . ." % (current_time(), ))

    backup_file_name = backup_dir+os.sep+os.path.split(fpath)[1]+".yasi.bak~"

    if _BACKUP:
        try:
            shutil.copyfile(fpath, backup_file_name)
        except IOError:
            warn("\n--%s-- Warning: Couldn't backup the file `%s' in `%s', check if you have enough permissions. " 
                    % (current_time(), fname, backup_dir))
            if _EXIT:
                exit("\n--%s-- Exiting. . ." % (current_time()))


    if _OUTPUT:
        print indented_code

if _MODIFY:
    # write in binary mode to preserve the original line ending
    with open(fpath, "wb") as indented_file:
        indented_file.write(indented_code)
