# C2PlusCal Transpiler

![Language](https://img.shields.io/badge/Language-OCaml-f18d03)
![Libraries](https://img.shields.io/badge/Libraries-FramaC-ff4001)
![Open Source](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

❓ **A transpiler that converts C programs into PlusCal specifications**
📌 *Language:* OCaml
🔧 *Status:* Active development

Based on: [`C2TLA+ by Amira METHNI`](https://hal.science/hal-01314832/document)

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
- **`-check-label <funanme>`**:
    Adds a `"Check_funname"` label before the end of specified functions, allowing
    properties or invariants to be inserted on local variables before they are removed from the stack.
    Can also be used with multiple functions : **`-check-label <fun1>,<fun2>`**
- **`-expect <file.expect>`**: Generates invariants in the `.cfg` file with the expecting
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

## Results

**Should be tested** on a certified micro-kernel, used in safety-critical airborne systems scheduler,
to strengthen development processes and identify bugs earlier than would be possible with traditional testing campaigns.

## License

Developed as part of a five-month internship at Asterios Technologies, subsidiary of Safran Electronics and Defense.
(LICENSE ?)

📄 **Full documentation available in [`doc/`](./doc/)**.
