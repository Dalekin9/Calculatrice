let ast = Parser.expr Lexer.token (Lexing.from_channel stdin)
in print_string (Syntax.to_string  ast); print_newline ()
