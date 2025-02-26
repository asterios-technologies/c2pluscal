Utilisation de stack comme un mini compilateur (initialisé avec une procédure pour ne pas avoir de soucis de déclaration qui se fait trop tôt dans init)
Macro et procedure prédefini pour gérer les choses
Retour de procédure gérés par une stack ret
load dans define psq on doit avoir une valeur peut pas se faire en pluscal
Process pour initialiser variables globales
Process "main" sert à appeler la fonction d'entrée du programme, qui est la dernière fonction définie dedans, à écrire à la main si volonté de faire autrement
Variable représentés par des pointeurs vers leur valeur dans la stack
Un pointeur est un record avec une loc (stack ou mem (variable globale)), un fp (frame pointer), un offs (par rapport au fp)

Début des procedure, on déclare toutes les variables comme UNDEF sur la stack, et à la fin on les pop
Macro pour push array sur la stack à la bonne taille

Reste des règles de trad dans pc_gen.ml (set -> store, etc)

Inspired by https://hal.science/hal-01314832/document

