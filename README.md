# Transpiler C vers PlusCal

  Ce projet est un transpiler permettant de convertir un programme écrit en C vers une spécification **PlusCal**,
  utilisé avec TLA+ pour spécifier et vérifier des modèles. Le processus génère également un fichier de configuration associé.

## Installation et Compilation

  Pour installer et compiler le transpiler, assurez-vous que vous avez `dune` et `frama-c` installés sur votre machine.

## Compilation
  ```bash
  dune build
   ```

  ```bash
  dune install
  ```

## Utilisation

  Pour convertir un fichier `test.c` (un exemple est fourni à la racine du repo) C vers PlusCal,
  utilisez la commande suivante avec Frama-C :

  ```bash
  frama-c -pluscal test.c
  ```

  Les fichiers suivants seront générés :
      test.tla : La spécification en PlusCal.
      test.cfg : Le fichier de configuration.

  ⚠️ Il faudra écrire les Invariants et Propriétés à la main en dessous de la traduction PlusCal, dans des prédicats respectifs,
  `Inv` et `Prop`, par exemple pour vérifier que la variable `x` du fichier `test.tla` contient la valeur 3 après éxecution,
  et que la variable globale `a` contient la valeur 2, on écrira :
  ```
    Prop == /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], x_ptr_main[0]) = 3)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], z_ptr_main[0]) = 5)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], b_ptr_main[0]) = 6)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], c_ptr_main[0]) = 2)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], i_ptr_main[0]) = 10)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], d_ptr_main[0]) = 60)
            /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], j_ptr_main[0]) = 10)
            /\ <>[](mem /= <<>> /\ load(mem, a_ptr_glob) = 2)
  ```

## Options

  - `-debug-dump` : permet de générer un fichier `.dump` contenant des informations de debug sur la traduction
  - `-block-main` : permet d'ajouter une instruction qui bloque l'exécution du programme avant la fin de la fonction
                    d'entrée, afin d'écrire des propriétés et invariants sur les variables du programme, avant qu'elles
                    ne soient pop de la stack

## Opérations non supportées

  Pour le moment, le transpiler ne supporte pas les :
      - Tableaux
      - Enum
      - Define

  Mais ça ne saurait tarder :)
