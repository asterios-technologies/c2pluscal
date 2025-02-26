# C to PlusCal Transpiler

🚀 **A transpiler that converts C programs into PlusCal specifications**
📌 *Language:* OCaml
🔧 *Status:* Active development

## 📥 Installation

Before starting, make sure you have **Frama-C** and **Dune** installed.

### 🔨 Compilation

In the `src/` directory, run:

```bash
dune build
dune install
```

## 🛠️ Options

- **`-debug-dump`**: Generates a `.dump` file containing debug information.
- **`-check-label "<funanme>""`**: Adds a `"Check_funname"` label before the end of specified functions,
                                   allowing properties or invariants to be inserted on local variables
                                   before they are removed from the stack.
- **`-expect "<file.expect>"`**: Generates invariants in the `.cfg` file with the expecting
                                 value of variables of the program, written in the`.expect` file

## 🚀 Usage

To transpile a file `test.c` into PlusCal with a `"Check"` label before the end of `main`:

```bash
frama-c -pluscal -check "main" test.c
```

🔹 This generates:
- **`test.tla`**: The PlusCal specification.
- **`test.cfg`**: The configuration file.

⚠️ **Manual additions required**:
      **Invariants** and **Properties** must be written after the translation, under the `Inv` and `Prop` predicates.
      They can be automatically generated with the `-expect` option.


📄 **Full documentation available in [`doc/`](./doc/)**.
