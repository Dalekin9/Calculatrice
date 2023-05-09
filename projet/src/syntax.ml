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

let rec subst e1 var e2 = match e1 with
|Var(name) -> if name == var then
                e2
              else
                e1
|App1(op, expr) -> App1(op, (subst expr var e2))
|App2(op, expr1, expr2) -> App2(op, (subst expr1 var e2), subst expr2 var e2)
| _ -> e1

let rec derive expr var = match expr with
| Var(name) -> if name == var then
                  Num(0)
                else
                  expr
| App1(op, expr) -> (match expr with
                   | Var(name) -> if name <> var then
                                    derive (Num(1)) var
                                  else
                                    deriveApp1 expr var op
                   | _ ->  deriveApp1 expr var op)
| App2(op, expr1, expr2) -> deriveApp2 expr1 expr2 var op
| _ -> Num(0)

and deriveApp1 expr var = match expr with
| Var(name) -> (if name == var then
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
| Expo -> App2(Mult, expr2, App2(Expo, expr1, App2(Minus, expr2, Num(1))))


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
