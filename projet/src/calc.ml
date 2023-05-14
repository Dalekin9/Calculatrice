let ast = Parser.command Lexer.token (Lexing.from_channel stdin)
in match ast with
  | Syntax.Eval(expr) -> print_float (Syntax.eval expr) 
  | Syntax.Subst(e1, var, e2) -> print_string(Syntax.to_string (Syntax.subst e1 var e2))
  | Syntax.Derive(expr, var) -> print_string(Syntax.to_string (Syntax.derive expr var))
  | Syntax.Simpl(expr) -> print_string(Syntax.to_string (Syntax.loop expr))
  | _ -> assert false

