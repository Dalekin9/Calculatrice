%token <int> INT
%token <string> IDENT
%token <Syntax.op0> OP0
%token <Syntax.op1> OP1
%token PLUS MINUS TIMES DIV EXPO LPARA RPARA EOL COL
%token EVAL SUBST SIMPL DERIVE INTEG PLOT
%start <Syntax.expr> expr
%left PLUS MINUS
%left TIMES DIV
%right EXPO
%start <Syntax.cmd> command
%{ open Syntax %}
%%

expr: e1=e EOL          {e1}
e: e1=e PLUS e2=e    {App2(Plus,e1,e2)}
  | e1=e MINUS e2=e  {App2(Minus,e1,e2)}
  | e1=e TIMES e2=e  {App2(Mult,e1,e2)}
  | e1=e DIV e2=e    {App2(Div,e1,e2)}
  | e1=e EXPO e2=e   {App2(Expo,e1,e2)}
  | LPARA e1=e RPARA {e1}
  | i1=INT           {Num(i1)}
  | v1=IDENT         {Var(v1)}
  | op0=OP0          {App0(op0)}
  | op1=OP1 LPARA e1=e RPARA {App1(op1,e1)}
  | MINUS e1=e       {App1(UMinus,e1)}

command: c1=c EOL         {c1}
c:
  | EVAL LPARA e1=e RPARA {Eval(e1)}
  | SUBST LPARA e1=e COL v1=IDENT COL e2=e RPARA {Subst(e1,v1,e2)}
  | SIMPL LPARA e1=e RPARA {Simpl(e1)}
  | DERIVE LPARA e1=e COL v1=IDENT RPARA {Derive(e1,v1)}
  | PLOT LPARA e1=e COL v1=IDENT RPARA {Plot(e1,v1)}
  | INTEG LPARA e1=e COL v1=IDENT COL lower=e COL upper=e RPARA
      {Integ(e1,v1,lower,upper)}

