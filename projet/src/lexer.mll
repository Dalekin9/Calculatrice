{
  open Parser
  open Syntax
  exception Error of string

  let op0_table = Hashtbl.create 2;;
  List.iter (fun (kwd, tok) -> Hashtbl.add op0_table kwd tok)
              [ ("e", E);
                ("pi", Pi)]
  let op1_table = Hashtbl.create 10;;
  List.iter (fun (kwd, tok) -> Hashtbl.add op1_table kwd tok)
              [ ("sqrt", Sqrt);
                ("exp", Exp);
                ("log", Log);
                ("sin", Sin);
                ("cos", Cos);
                ("tan", Tan);
                ("asin", ASin);
                ("acos", ACos);
                ("atan", ATan)]
}

rule token = parse
  | [' ' '\t']       { token lexbuf }
  | '\n'             { EOL }
  | ['0'-'9']+ as i  { INT (int_of_string i) }
  | '+'              { PLUS }
  | '*'              { TIMES }
  | '/'              { DIV }
  | '-'              { MINUS }
  | '^'              { EXPO }
  | '('              { LPARA }
  | ')'              { RPARA }
  | ','              { COL }
  | "eval"   { EVAL }
  | "subst"  { SUBST }
  | "simpl"  { SIMPL }
  | "derive" { DERIVE }
  | "plot"   { PLOT }
  | "integ"  { INTEG }
  | ['a'-'z']* as v  { try OP0(Hashtbl.find op0_table v)
                       with Not_found ->
                             try OP1(Hashtbl.find op1_table v)
                             with Not_found -> IDENT v}
  | _       { raise (Error (Lexing.lexeme lexbuf)) }
