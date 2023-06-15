Projet de PFA : Calcul Formel
=============================

## Installation

Pour compiler les fichiers sources fournis, en plus d'OCaml nous vous recommandons
fortement l'usage des outils suivants: `make`, `menhir`, `dune`, `ocamlfind`.

Sur une Debian (ou Ubuntu >= 20.4), ces outils peuvent s'installer simplement via:
`sudo apt install make ocaml menhir dune ocaml-findlib`.

Pour les autres systèmes, `make` est disponible sur toute plateforme raisonnable de
développement, et si les autres outils ne sont pas disponibles directement vous
pourrez les installer via [opam](http://opam.ocaml.org/), un gestionnaire de paquets OCaml.

Eviter fortement Ubuntu 18.04 (ou plus ancien), qui ne propose pas
l'outil `dune` (ou pire, un binaire du même nom mais qui n'a rien à
voir avec OCaml).

## Utilisation

Une fois installé les outils nécessaires (voir la section précédente), vous pouvez
compiler en lançant `make` dans le répertoire actuel `projet`, ce qui lancera `dune build`
avec les bons arguments (voir le fichier [Makefile](Makefile) pour plus de détails).

Ensuite, le petit script fourni `run` facilite ensuite le lancement du
binaire obtenu (`dune exec` avec les bons arguments). Pour l'instant,
le code principal lit une expression algébrique sur l'entrée standard,
la transforme en donnée OCaml de type `Syntax.expr`, puis la réaffiche
(voir [src/calc.ml](src/calc.ml)). Par exemple:

```sh
% ./run
x+pi*sqrt(3)
(x+(pi*sqrt(3)))
```

