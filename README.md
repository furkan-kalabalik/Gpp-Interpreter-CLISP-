# CLISP-Interpreter
CLISP interpreter implementation in CLISP

to start interpreter you can write 
```
clisp gpp_parser.lisp
```
There must be gpp_lexer.lisp since to create token list, we need lexer code and it will be
on gpp_parser.lisp code.
After that printing g++ starts interpreter and (exit) stops interpreter.
Optionally, you can print by parsing a file g++ <filename>.