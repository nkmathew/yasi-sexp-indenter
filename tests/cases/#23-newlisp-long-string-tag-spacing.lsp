(print "<A href=\"http://mysite.com\">" ) ; the cryptic way

(print {<A href="http://mysite.com">} )   ; the readable way


; path names on MS Windows

(set 'path "C:\\MyDir\\example.lsp")

; no escaping when using braces

(set 'path {C:\MyDir\example.lsp})

; on MS Windows the forward slash can be used in path names

(set 'path "C:/MyDir/example.lsp")

; inner braces are balanced
(regex {abc{1,2}} line) 

(print [text]
  this could be
  a very long (> 2048 characters) text,
  i.e. HTML.
[/text])
