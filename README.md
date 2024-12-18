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
      test.out : Un fichier de log qui peut être ignoré.

  ⚠️ Le fichier .out n'est pas conçu pour être utilisé dans le processus de modélisation ou de vérification.

## Opérations non supportées

  Pour le moment, le transpiler ne supporte pas les :
      - Structures
      - Tableaux
      - Enum
      - Boucles
      - Define

  Mais ça ne saurait tarder :)
