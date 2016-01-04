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
