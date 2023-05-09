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
