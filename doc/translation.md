<!-- Utilisation de stack comme un mini compilateur (initialisé avec une procédure pour ne pas avoir de soucis de déclaration qui se fait trop tôt dans init)
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

Inspired by https://hal.science/hal-01314832/document -->

# 🔄 C to PlusCal Translation

This document explains how the **C2PlusCal Transpiler** translates C code into PlusCal.

For more in-depth details, refer to the original paper which inspired this work:
[`C2TLA+ by Amira METHNI`](https://hal.science/hal-01314832/document).

---

## 🏗️ Translation Approach

The translation process follows a stack-based approach, functioning similarly to a minimal compiler.

### 🔹 Stack-Based Execution Model

- The transpiler uses a **stack** to manage function calls and variable scope.
- The stack is initialized with a dedicated procedure to prevent premature declarations during initialization.
- **Predefined macros and procedures** handle essential operations, such as function calls and variable management.
  See **Macros Explanation** section.
- Function returns, not handled in PlusCal procedures, are managed by pushing the return value on a **return stack (`ret`)**,

### 📌 Memory Representation

- Variables are stored as **pointers** referencing their value in the stack.
- A pointer is represented as a **record** with:
  - `loc`: Memory location (`stack` or `mem` for global variables).
  - `fp`: Frame pointer (stack frame reference).
  - `offs`: Offset relative to the frame pointer.

### 🚀 Program Initialization

- A dedicated **process** initializes global variables.
- A separate **"main" process** is used to call the entry function of the program.
  - By default, the last defined function in the file is considered the entry point.
  - If a different function should be used, it must be explicitly written.

### 🔄 Translation Rules

- `load` operations are placed inside `define` statements because PlusCal requires an explicit value.
- Complex translation rules are implemented in `pc_gen.ml`.

### ⚙️ Macros Explanation

- At the **start** of a procedure, all local variables are declared as `UNDEF` on the stack.
- At the **end** of the procedure, local variables are **popped** from the stack.
- A macro ensures that arrays are **pushed** onto the stack with the correct size.

The transpiler uses macros to facilitate certain operations in PlusCal, ensuring a correct translation of C semantics.

- `load(stk, ptr)`: Loads a pointer from a given stack (`mem` or `stack`).
- `push(my_stack, val)`: Pushes a value onto the top of a sequence (used as a stack).
- `pop(my_stack)`: Pops the top value from a sequence.
- `decl(val, ptr)`: Declares a pointer on a stack, initializing its offset correctly and assigning it the value `val`.
- `store(val, ptr)`: Updates the value of a pointer.
- `attr_return(ret, ptr)`: Stores the top of the `ret` stack (used as a return stack for procedures) into a pointer, then pops `ret`.
- `init_array(size, arr_ptr)`: Initializes an array on the stack by adding a sequence of `UNDEF` values of the correct size. This is achieved using a global variable `tmpInitArray`, which serves as a loop index to fill the sequence.
- `stacks_init()`: Initializes the `my_stack` and `ret` stacks.


---

These translation principles enable the C2PlusCal transpiler to generate a PlusCal model while
preserving the semantics of the original C program.


