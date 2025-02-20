# Transpiler C vers PlusCal

  Ce projet est un transpiler permettant de convertir un programme écrit en C vers une spécification **PlusCal**,
  utilisé avec TLA+ pour spécifier et vérifier des modèles. Le processus génère également un fichier de configuration associé.

## Installation et Compilation

  Pour installer et compiler le transpiler, assurez-vous que vous avez `dune` et `frama-c` installés sur votre machine.

## Compilation
  Placez vous dans le dossier `src/` et exécutez les commandes suivantes :

  ```bash
  dune build
   ```

  ```bash
  dune install
  ```

## Options

  - `-debug-dump` : permet de générer un fichier `.dump` contenant des informations de debug sur la traduction
  - `-check-main` : permet d'ajouter un label `"Check"` avant la fin de la fonction d'entrée,
                    afin d'écrire des propriétés et invariants sur les variables du programme,
                    avant qu'elles ne soient retirées de la stack

## Utilisation

  Pour convertir un fichier `test.c` (un exemple est fourni dans le dossier `tests/`) C vers PlusCal,
  avec l'ajout d'un label `"Check"` avant la fin de la fonction d'entrée,
  utilisez la commande suivante avec Frama-C :

  ```bash
  frama-c -pluscal -check-main test.c
  ```

  Les fichiers suivants seront générés :
      test.tla : La spécification en PlusCal.
      test.cfg : Le fichier de configuration.

  ⚠️ Il faudra écrire les Invariants et Propriétés à la main en dessous de la traduction PlusCal, dans des prédicats respectifs,
  `Inv` et `Prop`, par exemple pour vérifier que les variables du fichier `test.c` contiennent les valeurs attendues après éxecution,
   on écrira :
  ```
    Inv ==  /\ (pc[1] = "Check" => load(my_stack[1], x_ptr_main[1]) = 3)
            /\ (pc[1] = "Check" => load(my_stack[1], z_ptr_main[1]) = 5)
            /\ (pc[1] = "Check" => load(my_stack[1], b_ptr_main[1]) = 6)
            /\ (pc[1] = "Check" => load(my_stack[1], c_ptr_main[1]) = 2)
            /\ (pc[1] = "Check" => load(my_stack[1], i_ptr_main[1]) = 10)
            /\ (pc[1] = "Check" => load(my_stack[1], d_ptr_main[1]) = 60)
            /\ (pc[1] = "Check" => load(my_stack[1], j_ptr_main[1]) = 10)
            /\ (pc[1] = "Check" => load(my_stack[1], k_ptr_main[1]) = 2)
            /\ (pc[1] = "Check" => load(my_stack[1], t_ptr_main[1]) = 3)
            /\ (pc[1] = "Check" => load(my_stack[1], my_color_ptr_main[1]) = BLUE)
            /\ (pc[1] = "Check" => load(my_stack[1], error1_ptr_main[1]) = [name |-> "test", id |-> 2])
            /\ (pc[1] = "Check" => load(my_stack[1], simple_arr_ptr_main[1]) = <<12,2,3,4,5>>)
            /\ (pc[1] = "Check" => load(mem, a_ptr_glob) = 2)
            /\ (pc[1] = "Check" => load(mem, glob_arr_ptr_glob) = <<UNDEF,UNDEF,UNDEF,UNDEF,UNDEF,1,UNDEF,UNDEF,UNDEF,UNDEF>>)
            /\ (pc[1] = "Check" => load(mem, error_ptr_glob) = [name |-> "test glob error", id |-> 1])
  ```
  Par exemple, `(pc[1] = "Check" => load(my_stack[1], x_ptr_main[1]) = 3)` vérifie que la valeur de la variable `x` est de 3,
  juste avant le retour de `main`, symbolisé par le label `"Check"`.
  Les `[1]` présents sur les invariants, proviennent du fait que l'on vérifie les valeurs sur le processus d'ID `1`, on peut
  ainsi vérifier les valeurs de variables de processus différents.
    ⚠️ le processus d'ID `0` sert à initialisé les variables globales et n'est pas utilisé pour traduire le code C.

## Opérations non supportées

  Pour le moment, le transpiler ne supporte pas les :
      - Define

  Mais ça ne saurait tarder :)
