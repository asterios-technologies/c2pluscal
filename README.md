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
      test.dump : Un fichier de log qui peut être ignoré.

  ⚠️ Le fichier .dump n'est pas conçu pour être utilisé dans le processus de modélisation ou de vérification.

  ⚠️ Le fichier généré contient un code PlusCal, une fois traduit, il faut déplacer la partie `load` qui contiendra une erreur
  en dessous du prédicat `Init`.

  ⚠️ Si vous voulez vérifier des valeurs avant la fin de la fonction `main`, il faudra rajouter un blocage du programme,
   à la main, avec un `await FALSE`.

  ⚠️ Il faudra écrire les Invariants et Propriétés à la main en dessous de la traduction PlusCal, dans des prédicats respectifs,
  `Inv` et `Prop`, par exemple pour vérifier que la variable `x` du fichier `test.tla` contient la valeur 3 après éxecution,
  et que la variable globale `a` contient la valeur 2, on écrira :
  ```
    Prop == /\ <>[](my_stack[0] /= <<>> /\ load(my_stack[0], x_ptr_main[0]) = 3)
            /\ <>[](mem /= <<>> /\ load(mem, a_ptr_glob) = 2)
  ```

## Opérations non supportées

  Pour le moment, le transpiler ne supporte pas les :
      - Structures
      - Tableaux
      - Enum
      - Boucles
      - Define

  Mais ça ne saurait tarder :)
