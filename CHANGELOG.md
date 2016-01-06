### Unreleased

Freatures:
  - **yasi.py**
    + **--suffix** option for specifying backup file suffix
    + The zero level of the first block doesn't define that of the rest of the code.
      Other blocks can start at different columns

  - **yasi.lsp**
    + **--suffix** option for specifying backup file suffix
    + The zero level of the first block doesn't define that of the rest of the code.
      Other blocks can start at different columns

Fixes:

  - **yasi.lsp**
    + Backup being ran on the already modified file
    + Newlisp comments starting with hash(`#`) being evaluated
    + Space before comment decreasing by a single character on re-indentation

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
    + Syntax error due to ommitted comma in setup.py argument list

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
