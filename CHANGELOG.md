### v1.2.1 - 22nd January 2016

Fixes:
  - **yasi.lsp**
    + Local functions in Clojure `letfn` construct not being indented the right way
    + Dialect detection by file extension not working properly

  - **yasi.py**
    + Defaults to plain diff output when colorama is not installed. Prevents import
      errors when used as a module in Sublime text plugin where non-standard
      packages through pip are not supported
    + Made runnable in embedded interpreters where sys.argv doesn't exist e.g. as a
      sublime text plugin
    + Local functions in Clojure `letfn` construct not being indented the right way
    + Dialect detection by file extension not working properly


### v1.2.0 - 10th January 2016

Features:
  - **yasi.py**
    + **--suffix** option for specifying backup file suffix
    + The zero level of the first block doesn't define that of the rest of the code.
      Other blocks can start at different columns
    + Indentation with tabs now supported by specifying tab width using the **--tabs**
      commandline argument

  - **yasi.lsp**
    + **--suffix** option for specifying backup file suffix
    + The zero level of the first block doesn't define that of the rest of the code.
      Other blocks can start at different columns
    + Indentation with tabs now supported by specifying tab width using the **--tabs**
      commandline argument

Fixes:

  - **yasi.py**
    + Backup being ran on the already modified file
    + Newlisp comments starting with hash(`#`) being evaluated
    + Space before comment decreasing by a single character on re-indentation
    + Error due to unsupported keyword argument in python 2.6

  - **yasi.lsp**
    + Backup being ran on the already modified file
    + Newlisp comments starting with hash(`#`) being evaluated
    + Space before comment decreasing by a single character on re-indentation
    + **--no-output** option not being parsed properly, affecting compact setting


### v1.1.2 - 4th January 2016
Fixes:

  - **yasi.py**
    + Circular imports causing failed pip installs

### v1.1.1 - 4th January 2016
Fixes:

  - **yasi.py**
    + Coloured diff not showing context lines
    + Syntax error due to omitted comma in setup.py argument list

### v1.1.0 - 4th January 2016

Features:

  - **yasi.py**
    + New **--color** option for displaying coloured diff output

Fixes:

  - **yasi.py**
    + **--diff** creates reversed diff due to wrong argument order in call to
      `difflib.unified_diff`

### v1.0.1 - 4th January 2016
Fixes:

  - Add License file
  - Include testcases and newlisp scripts in pypi source distribution
  - README.rst generated from README.md for pypi documentation using pandoc
  - Long description for pypi

### v1.0.0 - 4th January 2016

Features:

  - **yasi.py**
    + New commandline parameter **--indent-size** for changing indent size
    + Support for additional keywords through a configuration file(yasirc.json)
    + Can output unified diff of the initial and final result

  - **yasi.lsp**
    + New commandline parameter **--indent-size** for changing indent size
    + Support for additional keywords through a configuration file(yasirc.json)

### v0.2.1 - 29th December 2015

Fixes:

  - **yasi.py**
    + Warnings being printed when script is used as a filter

  - **yasi.lsp**
    + Warnings being printed when script is used as a filter

### v0.2.0 - 29th December 2015

Fixes:

  - **yasi.py**
    + Newlisp brace strings being trimmed
    + **--indent-comments** now indents comments without trimming whitespace
    + Newlisp tag strings being trimmed

  - **yasi.lsp**
    + Newlisp brace strings being trimmed
    + **--indent-comments** now indents comments without trimming whitespace
    + Newlisp tag strings being trimmed

Features:

  - **yasi.py**
    + Indentation from standard input
    + Indentation of multiple files specified in the command line
    + Able to specify output filename
    + Guesses dialect by file extension when the dialect flag is not supplied
    + Tildes in backup folder path in Windows

  - **yasi.lsp**
    + Indentation from standard input
    + Indentation of multiple files specified in the command line
    + Guesses dialect by file extension when the dialect flag is not supplied
    + Able to specify output filename
    + Tildes in backup folder path in Windows
