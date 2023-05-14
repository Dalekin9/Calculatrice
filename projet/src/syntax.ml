(* Syntaxe abstraite des expressions arithmétiques *)

type op0 = Pi | E
type op1 = Sqrt | Exp | Log | Sin | Cos | Tan | ASin | ACos | ATan | UMinus
type op2 = Plus | Mult | Minus | Div | Expo

type nums = int

type expr =
  | Num of nums
  | Var of string
  | App0 of op0
  | App1 of op1 * expr
  | App2 of op2 * expr * expr

type cmd =
  | Eval of expr
  | Subst of expr*string*expr
  | Simpl of expr
  | Derive of expr*string
  | Integ of expr*string*expr*expr
  | Plot of expr*string

let rec simpl expr = match expr with
    | App1 (o1,expr2) -> (match o1 with
        | Sqrt -> simplSqrt expr2
        | Exp -> simplExp expr2
        | Log -> simplLog expr
        | Sin -> simplSin expr2
        | Cos -> simplCos expr2
        | Tan -> simplTan expr
        | ASin -> simplASin expr
        | ACos -> simplACos expr
        | ATan -> simplATan expr
        | UMinus -> simplUMinus expr )
    | App2 (o2,e1,e2) -> (match o2 with
        | Plus -> simplPlus e1 e2
        | Mult -> simplMult e1 e2
        | Minus -> simplMinus e1 e2
        | Div -> simplDiv e1 e2
        | Expo -> simplExpo e1 e2)
    | _ -> expr

and simplUMinus = function
  |App1(UMinus, expr) -> (match expr with
      |App1(UMinus, expr2) -> simpl expr2 
      | _ -> App1(UMinus, (simpl expr)))
  | _ -> assert false


and simplLog = function
  |App1(Log, expr) -> (match expr with
    |App2(Mult, expr1, expr2) -> App2(Plus, App1(Log, (simpl expr1)), App1(Log, (simpl expr2)))
    |App2(Div, expr1, expr2) -> App2(Minus, App1(Log, (simpl expr1)), App1(Log, (simpl expr2)))
    |App2(Expo, expr1, expr2) -> App2(Mult, (simpl expr2), App1(Log, (simpl expr1)))
    |_ -> App1(Log, (simpl expr)))
  | _ -> assert false


and simplACos = function
  |App1(ACos, expr) -> (match expr with
    |App1(Cos, expr2) -> simpl expr2
    |_ -> App1(ACos, (simpl expr)))
  | _ -> assert false


and simplASin = function
  |App1(ASin, expr) -> (match expr with
    |App1(Sin, expr2) -> simpl expr2
    |_ -> App1(ASin, (simpl expr)))
  | _ -> assert false


and simplATan = function
  |App1(ATan, expr) -> (match expr with
    |App1(Tan, expr2) -> simpl expr2
    |_ -> App1(ATan, (simpl expr)))
  |_ -> assert false

and simplTan = function
  |App1(Tan, expr) -> (match expr with
    |App1(UMinus, expr2) -> App1(UMinus, App1(Tan, (simpl expr2)))
    |_ -> App1(Tan, (simpl expr)))
  | _ -> assert false

and simplSqrt e =
    match e with 
    | App2(Expo,e,Num 2) -> simpl e
    | App2(Expo,e, Num p) -> if p mod 2 = 0 then simpl (App2(Expo,e,(Num (p/2)))) else App1(Sqrt, (simpl (App2(Expo,e,(Num p)))))
    | App2(Mult,e1,e2) -> App2(Mult, (simpl (App1(Sqrt, e1))), (simpl (App1(Sqrt, e2))))
    | App2(Div,Num 1,Num 2) -> App1(Sqrt, (App1(Exp, (Num 1))))

    | App2(Div,Num 1,e2) -> App2(Div, (Num 1), (simpl (App1(Sqrt, e2))))
    | _ -> App1(Sqrt, (simpl e))
  
and simplExp e = 
  match e with
  | App1(Log,e) -> simpl e
  | Num 0 -> Num 1
  | App2(Plus,e1,e2) -> App2(Mult, (simpl (App1(Exp, e1))) ,(simpl (App1(Exp, e2))))
  | App1(UMinus, e1) -> App2(Div ,(Num 1), (simpl (App1(Exp, e1))))
  | App2(Minus,e1,e2) -> App2(Div, (simpl (App1(Exp, e1))), (simpl (App1(Exp, e2))))
  | App2(Expo,e1,e2) -> App1(Exp, (simpl (App2(Mult, e1, e2))))
  | _ -> App1(Exp, (simpl e))

and simplSin e =
  match e with
  | Num(0) -> Num(0)
  | App1(UMinus,e) -> App1(UMinus, (simpl (App1(Sin, e))))
  | App2(Mult,App0 Pi,Num n) -> Num 0
  | App2(Div,e,Num 6) ->  (match e with
                            | App0 Pi -> App2(Div, (Num 1), (Num 2))
                            | App2(Mult,App0 Pi,Num n) -> 
                              if n mod 12 = 1 || n mod 12 = 5 then App2(Div, (Num 1), (Num 2))
                              else if n mod 12 = 7 || n mod 12 = 11 then App1(UMinus, (App2(Div, (Num 1), (Num 2))))
                              else App1(Sin, (simpl (App2(Div, e, (Num 6)))))
                            | _ -> App1(Sin, (simpl (App2(Div, e, (Num 6))))))
  | App2(Div,e,Num 4) ->  (match e with
                            | App0 Pi -> App2(Div, (App1(Sqrt, Num 2)), (Num 2))
                            | App2(Mult,App0 Pi,Num n) -> 
                              if n mod 8 = 1 || n mod 8 = 3 then App2(Div, (App1(Sqrt, Num 2)), (Num 2))
                              else if n mod 8 = 5 || n mod 8 = 7 then App1(UMinus, (App2(Div, (App1(Sqrt, Num 2)), (Num 2))))
                              else App1(Sin, (simpl (App2(Div, e,(Num 4)))))
                            | _ -> App1(Sin, (simpl (App2(Div, e,(Num 4))))))
  | App2(Div,e,Num 3) ->  (match e with
                            | App0 Pi -> App2(Div, (App1(Sqrt, Num 3)), (Num 2))
                            | App2(Mult,App0 Pi,Num n) -> 
                              if n mod 6 = 1 || n mod 6 = 2 then App2(Div, (App1(Sqrt, Num 3)), (Num 2))
                              else if n mod 6 = 4 || n mod 6 = 5 then App1(UMinus, (App2(Div, (App1(Sqrt, Num 3)), (Num 2))))
                              else App1(Sin, (simpl (App2(Div, e, (Num 3)))))
                            | _ -> App1(Sin, (simpl (App2(Div, e, (Num 3))))))
  | App2(Div,e,Num 2) ->  (match e with
                            | App0 Pi -> Num 1
                            | App2(Mult,App0 Pi,Num n) -> 
                              if n mod 4 = 1 then Num 1
                              else if n mod 4 = 3 then App1(UMinus, (Num 1))
                              else Num 0
                            | _ -> Num 0)
  | _ -> App1(Sin, (simpl e))



and simplCos e = 
  match e with
  | Num(0) -> Num(1)
  | App1(UMinus,e) -> simpl (App1(Cos, e))
  | App2(Mult,App0 Pi,Num n) -> if n mod 2 = 0 then Num 1 else App1(UMinus, (Num 1))
  | App2(Div,e,Num 6) ->  (match e with
                          | App0 Pi -> App2(Div,((App1(Sqrt, Num 3))), (Num 2))
                          | App2(Mult,App0 Pi,Num n) ->
                            if n mod 12 = 1 || n mod 12 = 11 then App2(Div, (App1(Sqrt, Num 3)), (Num 2))
                            else if n mod 12 = 5 || n mod 12 = 7 then App1(UMinus, (App2(Div, (App1(Sqrt, Num 3)) ,(Num 2))))
                            else App1(Cos, (simpl (App2(Div, e, (Num 6)))))
                          | _ -> App1(Cos, (simpl (App2(Div, e, (Num 6))))))
  | App2(Div,e,Num 4) ->  (match e with
                          | App0 Pi -> App2(Div, (App1(Sqrt, Num 2)), (Num 2))
                          | App2(Mult,App0 Pi,Num n) -> 
                            if n mod 8 = 1 || n mod 8 = 7 then App2(Div, (App1(Sqrt, Num 2)), (Num 2))
                            else if n mod 8 = 3 || n mod 8 = 5 then App1(UMinus, (App2(Div, (App1(Sqrt, Num 2)), (Num 2))))
                            else App1(Cos, (simpl (App2(Div, e, (Num 4)))))
                          | _ -> App1(Cos, (simpl (App2(Div, e, (Num 4))))))
  | App2(Div,e,Num 3) ->  (match e with
                          | App0 Pi -> App2(Div, (Num 1), (Num 2))
                          | App2(Mult,App0 Pi,Num n) -> 
                            if n mod 6 = 1 || n mod 6 = 5 then App2(Div, (Num 1), (Num 2))
                            else if n mod 6 = 4 || n mod 6 = 2 then App1(UMinus, (App2(Div, (Num 1), (Num 2))))
                            else App1(Cos, (simpl (App2(Div, e, (Num 3)))))
                          | _ ->  App1(Cos, (simpl (App2(Div, e, (Num 3))))))
  | App2(Div,e,Num 2) ->  (match e with
                          | App0 Pi -> Num 0
                          | App2(Mult,App0 Pi,Num n) -> 
                            if n mod 4 = 0 then Num 1
                            else if n mod 4 = 2 then App1(UMinus, (Num 1))
                            else Num 0
                          | _ -> Num 0)
  | _ -> App1(Cos, (simpl e))

and simplPlus e1 e2 =
if e1 = e2 then simpl (App2(Mult, e1, (Num 2))) else 
  match e1, e2 with
  | Num 0, _ -> simpl e2
  | _, Num 0 -> simpl e1
  | Num n, Num m -> Num (n+m)
  | App2 (Div,n,d), App2(Div,m,d1) -> if d = d1 then simpl (App2(Div, (App2(Plus, n, m)), d)) else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Mult,Var s,n), App2 (Mult,Var s1,m) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Plus, n, m)))) else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Mult,Var s,n), App2 (Mult,m,Var s1) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Plus, n, m)))) else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Mult,n,Var s), App2 (Mult,Var s1,m) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Plus, n, m)))) else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Mult,n,Var s), App2 (Mult,m,Var s1) -> if s = s1 then App2(Mult ,(Var s), (simpl (App2(Plus, n, m)))) else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Expo,App1(Cos, e1),Num 2), App2(Expo,App1(Sin, e2),Num 2) -> if e1 = e2 then Num 1 else App2(Plus, (simpl e1) ,(simpl e2))
  | App2 (Expo,App1(Sin, e1),Num 2), App2(Expo,App1(Cos, e2),Num 2) -> if e1 = e2 then Num 1 else App2(Plus, (simpl e1) ,(simpl e2))
  | _ -> App2(Plus, (simpl e1) ,(simpl e2))

  and simplMinus e1 e2 =
  if e1 = e2 then Num 0 else 
    match e1, e2 with
    | Num 0, _ -> simpl e2
    | _, Num 0 -> simpl e1
    | Num n, Num m -> Num (n-m)
    | App2 (Div,n,d), App2(Div,m,d1) -> if d = d1 then simpl (App2(Div, (App2(Minus, n, m)), d)) else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Mult,Var s,n), App2 (Mult,Var s1,m) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Minus, n, m)))) else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Mult,Var s,n), App2 (Mult,m,Var s1) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Minus, n, m)))) else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Mult,n,Var s), App2 (Mult,Var s1,m) -> if s = s1 then App2(Mult, (Var s), (simpl (App2(Minus, n, m)))) else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Mult,n,Var s), App2 (Mult,m,Var s1) -> if s = s1 then App2(Mult ,(Var s), (simpl (App2(Minus, n, m)))) else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Expo,App1(Cos, e1),Num 2), App2(Expo,App1(Sin, e2),Num 2) -> if e1 = e2 then Num 1 else App2(Minus, (simpl e1) ,(simpl e2))
    | App2 (Expo,App1(Sin, e1),Num 2), App2(Expo,App1(Cos, e2),Num 2) -> if e1 = e2 then Num 1 else App2(Minus, (simpl e1) ,(simpl e2))
    | _ -> App2(Minus, (simpl e1) ,(simpl e2))

and simplMult e1 e2 = 
  match e1, e2 with
  | Num 0, _ -> Num 0
  | _, Num 0 -> Num 0
  | Num 1, _ -> simpl e2
  | _, Num 1 -> simpl e1
  | Num n, Num m -> Num (n*m)
  | App1(Sqrt,a), App1(Sqrt,a1) -> if a = a1 then simpl a else App2(Mult, (simpl e1), (simpl e2))
  | e, e1 -> if e = e1 then App2(Expo, (simpl e), (Num 2)) else App2(Mult, (simpl e1), (simpl e2))

and simplDiv e1 e2 =
  if e1 = e2 then Num 1 else
  match e1, e2 with 
  | App1(Sin,e), App1(Cos, e1) -> if e = e1 then App1(Tan, e) else App2(Div, (simpl e1), (simpl e2))
  | App2(Mult,Num a,b), Num c -> if a mod c = 0 then simpl (App2(Mult, (Num (a/c)), b)) else App2(Div, (simpl (App2(Mult ,(Num a), b))), (Num c))
  | App2(Mult,b,Num a), Num c -> if a mod c = 0 then simpl (App2(Mult,b, (Num (a/c)))) else App2(Div, (simpl (App2(Mult ,b,(Num a)))), (Num c))
  | Num n, Num m -> if n mod m = 0 then Num(n/m) else App2(Div, (Num n), (Num m))
  
  | _ -> App2(Div, (simpl e1), (simpl e2))
  

and simplExpo e1 e2 =
  match e1, e2 with
  | a, Num 1 -> simpl a
  | App1(Sqrt, e), Num 2 -> simpl e
  | a, App2(Div,Num 1,Num 2) -> simpl (App1(Sqrt, a))
  | _ -> App2(Expo, (simpl e1), (simpl e2))

let rec loop expr =
  let newExpr = simpl expr in
  if expr = newExpr then
    expr
  else
    loop newExpr

let rec subst e1 var e2 = match e1 with
|Var(name) -> if name = var then
                e2
              else
                e1
|App1(op, expr) -> App1(op, (subst expr var e2))
|App2(op, expr1, expr2) -> App2(op, (subst expr1 var e2), subst expr2 var e2)
| _ -> e1

let rec derive expr var = match expr with
| Var(name) -> if name = var then
                  Num(1)
                else
                  Num(0)
| App1(op, expr) -> (match expr with
                    | Var(name) -> if name <> var then
                                    derive (Num(1)) var
                                  else
                                    deriveApp1 expr var op
                    | _ ->  deriveApp1 expr var op)
| App2(op, expr1, expr2) -> deriveApp2 expr1 expr2 var op
| _ -> Num(0)

and deriveApp1 expr var = match expr with
| Var(name) -> (if name = var then
                  function
                  | Sqrt -> App2(Div, Num(1) ,App2(Mult, Num(2), App1(Sqrt,expr)))
                  | Exp -> App1(Exp, expr)
                  | Log -> App2(Div, Num(1), expr)
                  | Sin -> App1(Cos, expr)
                  | Cos -> App1(UMinus, App1(Sin, expr))
                  | Tan -> App2(Div, Num(1), App2(Expo, expr, Num(2)))
                  | ASin -> App2(Div, Num(1), App1(Sqrt, App2(Minus, Num(1), App2(Expo, expr, Num(2)))))
                  | ACos -> App1(UMinus, App2(Div, Num(1), App1(Sqrt, App2(Minus, Num(1), App2(Expo, expr, Num(2))))))
                  | ATan -> App2(Div, Num(1), App2(Plus, Num(1), App2(Expo, expr, Num(2))))
                  | UMinus -> App1(UMinus, (derive expr var))
                else
                  function
                  | _ -> (Num 0))
| _ -> function
        | _ -> (Num 0)

and deriveApp2 expr1 expr2 var = function
| Plus -> App2(Plus, (derive expr1 var), (derive expr2 var))
| Minus -> App2(Minus, (derive expr1 var), (derive expr2 var))
| Mult -> App2(Plus, App2(Mult, expr1, (derive expr2 var)), App2(Mult, expr2, (derive expr1 var)))
| Div ->  App2(Div, App2(Minus, App2(Mult, expr1, (derive expr2 var)), App2(Mult, expr2, (derive expr1 var))), App2(Expo, expr2, Num(2)))
| Expo -> match expr2 with 
	|Num(x) -> App2(Mult, expr2, App2(Expo, expr1, Num(x-1)))
	|_ -> App2(Mult, expr2, App2(Expo, expr1, App2(Minus, expr2, Num(1))))


let
  rec eval e =
  match e with
  | Num n -> Float.of_int n
  | Var s -> failwith "Variable"
  | App0 o0 -> (match o0 with 
                | Pi -> Float.pi
                | E -> Float.exp 1.)
  | App1 (o1,expr) -> evalApp1 expr o1
  | App2 (o2,e1,e2) -> evalApp2 e1 e2 o2

                    
and evalApp1 expr = function
  | Sqrt -> Float.sqrt (eval expr)
  | Exp -> Float.exp (eval expr)
  | Log -> Float.log (eval expr)
  | Sin -> Float.sin (eval expr)
  | Cos -> Float.cos (eval expr)
  | Tan -> Float.tan (eval expr)
  | ASin -> Float.asin (eval expr)
  | ACos -> Float.acos (eval expr)
  | ATan -> Float.atan (eval expr)
  | UMinus -> Float.neg (eval expr)


and evalApp2 e1 e2 = function
  | Plus -> (eval e1) +. (eval e2)
  | Mult -> (eval e1) *. (eval e2)
  | Minus -> (eval e1) -. (eval e2)
  | Div -> (eval e1) /. (eval e2)
  | Expo -> Float.pow (eval e1) (eval e2)  
  
  
  
(* Light : quelques fonctions pour aider à écrire de la syntaxe
    abstraite dans le code OCaml de manière plus légère. Par exemple:

    let expr = Light.(Var "x" + pi * sqrt (Num 3))
*)

module Light = struct

let pi = App0(Pi)
let e = App0(E)

let app1 o expr = App1(o,expr)
let sqrt = app1 Sqrt
let exp = app1 Exp
let log = app1 Log
let sin = app1 Sin
let cos = app1 Cos
let tan = app1 Tan
let asin = app1 ASin
let acos = app1 ACos
let atan = app1 ATan
let opp = app1 UMinus

let (+) e1 e2 = App2(Plus,e1,e2)
let (-) e1 e2 = App2(Minus,e1,e2)
let ( * ) e1 e2 = App2(Mult,e1,e2)
let (/) e1 e2 = App2(Div,e1,e2)
let (^) e1 e2 = App2(Expo,e1,e2)

end

(* Affichage *)

let str0 = function
  | Pi -> "pi"
  | E -> "e"

let str1 = function
  | Sqrt -> "sqrt"
  | Exp -> "exp"
  | Log -> "log"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | ASin -> "asin"
  | ACos -> "acos"
  | ATan -> "atan"
  | UMinus -> "-"

let str2 = function
  | Plus -> "+"
  | Mult -> "*"
  | Minus -> "-"
  | Div -> "/"
  | Expo -> "^"

let paren str = "(" ^ str ^ ")"

let rec to_string = function
  | Num n -> string_of_int n
  | Var v -> v
  | App0 o -> str0 o
  | App1(o,e) -> str1 o ^ paren(to_string e)
  | App2(o,e1,e2) -> paren (to_string e1 ^ str2 o ^ to_string e2)