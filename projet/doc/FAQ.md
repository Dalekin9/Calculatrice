Quelques questions-réponses sur le projet PFA
=============================================

## Au secours, je n'y connais rien en math !

N'exagérons rien, il s'agit de math de terminale. De plus
tous les formules nécessaires sont dans le formulaire
[doc/Formulaire.pdf](Formulaire.pdf), en particulier à la 4eme page pour
les calculs de dérivés et intégrales.

## Ou puis-je trouver des exemples d'exercices type Bac ?

Les annales du bac ne sont pas trop durs à trouver en ligne, voir par exemple:

- http://www.sujetdebac.fr
- http://www.maths-france.fr/terminales

Par exemple le sujet de métropole pour le bac S 2019:
https://maths-france.fr/wp-content/uploads/2019-france-metropolitaine-specifique_enonce.pdf
Son exercice 1 étudie la fonction `7/2-1/2(exp(x)+exp(-x)`.

## A quel moment peut-on faire des approximations ?

Seulement lors des commandes `eval` (qui calcule un résultat de type
float à partir d'une expression) et lors du tracé d'une courbe via
`plot`. Pour toutes les autres commandes, il s'agit de calcul formel,
donc sans approximations. En particulier, on veut des dérivées
symboliques, et pas de `(f(x)-f(x+d))/d` pour `d` petit. Quant aux
intégrales, soit l'expression à intégrer correspond à une situation
connue (genre `f'/f`), soit vous avez le droit de dire "je ne sais pas"
ou de laisser l'intégrale sous sa forme initiale. Cas particulier
possible: pour une commande comme `eval(integr(...))` ou
`plot(integr(...))`, vous pouvez éventuellement essayer un calcul
approché de l'intégrale, p.ex. via une méthode des rectangles, mais ce
n'est pas obligatoire (= bonus :-).

## Comment je calcule une valeur approchée de pi en OCaml ?

Depuis OCaml 4.07, on peut utiliser `Float.pi`. Avant cela, la "ruse"
habituelle était d'utiliser une des fonctions trigonométrique inverse,
par exemple `2.*.acos(0.)` ou `4.*.atan(1.)`.

## Qu'est-ce que c'est que cette constante e ?

`e = exp(1)` et donc réciproquement, on peut aussi écrire `exp(x) = e^x`.

## Mais alors, il peut y avoir plusieurs manière d'écrire une même expression ?

Oui. Voir aussi `sqrt(x) = x^(1/2)`. Vous pouvez essayer de simplifier ces
différentes formes en une seule lors de la commande `simpl` (on parle de
normalisation ou de canonisation).

## Comment faire plot sur des bornes non-entières ?

Ceci est facultatif, mais vous pouvez accepter et gérer des choses comme
`plot(...,x,-1/2,1/2,-3/4,3/4)` par exemple.

## Le parser fourni ne permet pas de saisir de grand entiers

Effectivement, le type `Syntax.nums` vaut `int` dans le code fourni,
et ne peut donc dépasser le `max_int` d'OCaml (voir aussi le
`int_of_string` présent dans `lexer.mll`). Ceci dit, on ne saisit
normalement pas tout les jours des entiers aussi gros, on va plutôt
les écrire sous une forme courte, par exemple `2^100`.
Et un "gros" nombre comme 12345678901234567890 peut être
découpé en parties ne dépassant pas `max_int` et réassemblé en tant
que `expr`, par exemple `1234567890*10^10+1234567890`. 
Facultatif : vous pouvez adapter `lexer.mll` et `parser.mly` pour
automatiser ce genre de découpe.
