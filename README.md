# C2PlusCal Transpiler

![Language](https://img.shields.io/badge/Language-OCaml-f18d03)
![Libraries](https://img.shields.io/badge/Libraries-FramaC-ff4001)
![Open Source](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

❓ **A transpiler that converts C programs into PlusCal specifications**
📌 *Language:* OCaml
🔧 *Status:* Active development

C2PlusCal translates C programs to [PlusCal][1] formal specifications.
PlusCal is an algorithm language created by [Leslie Lamport][2].
It is a formalism designed to make using formal methods easier.
PlusCal algorithm is automatically translated to a TLA+ model
that can be checked with TLA+ formal verification techniques.

The transpiler is developed as a [Frama-C][3] plugin.
Based on: [`C2TLA+ by Amira METHNI`][4]

## 📥 Installation

Before starting, make sure you have **[Frama-C][3]** and **[Dune][5]** installed.

### 🔨 Compilation

From the root directory, run:

```bash
dune build
dune install
```

This installs **C2PlusCal** as a Frama-C plugin.
You can check that by listing all available plugins of `Frama-C` by running:

```bash
frama-c -plugins
```

## 🚀 Plugin Usage

To transpile a file e.g. `tests/basic/test_array.c` into PlusCal:

```bash
frama-c -pluscal tests/basic/test_array.c
```

or by using ``dune``

```bash
dune exec -- frama-c -pluscal tests/basic/test_array.c
```

🔹 This generates:
- **`test.tla`**: The PlusCal specification.
- **`test.cfg`**: The configuration file.

⚠️ Some knowledge of PlusCal syntax are necessary to understand the PlusCal
generated code. Please refer to the section `TLA+ References` for a list of 
resources.

⚠️ **Manual additions required**:
      **Invariants** and **Properties** must be written after the translation,
      under the `Inv` and `Prop` predicates.
      They can be automatically generated with the `-expect` option.

`test.tla` contains the algorithm of your translated C program.
This algorithm is put into a TLA+ module. From this module, you can run the
PlusCal translator to generate a TLA+ model that can be checked by TLC model checker.
Please refer to this [page][5] for more information about using TLA+ tools.


### 🛠️ Plugin Options

- **`-debug-dump`**: Generates a `.dump` file containing debug information.
- **`-check-label <fun_name>`**:
    Adds a `"Check_fun_name"` label before the end of specified functions,
    allowing properties or invariants to be inserted on local variables before
    they are removed from the stack.
    Can also be used with multiple functions : **`-check-label <fun1>,<fun2>`**
- **`-expect <file.expect>`**: Generates invariants in the `.cfg` file with the
    expecting value of variables of the program, written in the`.expect` file

### How to model check the generated PlusCal algorithm ?

Once your `.tla`/`.cfg` files are generated, you can call PlusCal Translator 
(in your favorite editor) to generate TLA+ specification.

Then, you can call TLC model checker to check the specification.


⚠️  C2PlusCal make use of `Bitwise.tla` module (available in 
[CommunityModules repository][7]) which is not a standard TLA+ module.
If your C code make use of bitwise operations, you need to add this module. 
In this case, refer to steps described in the section `How to use it` of 
[CommunityModules repository][7].
Otherwise, you can just remove it from the list of imported modules.


## Results

**Should be tested** on a certified micro-kernel, used in safety-critical
airborne systems scheduler, to strengthen development processes and identify
bugs earlier than would be possible with traditional testing campaigns.

## TLA+ References

If you want to start with TLA+, the above references are basic points to get
started with it: 

- [The TLA+ Home Page][1] by Leslie Lamport's website.
- [Learn TLA+][8] by Hillel Wayne.


## Running tests

A set of tests are under `tests\basic` directory.

To run the tests, use:

```bash
dune build @ptests
```

This will check that translation succeeds. 
More precisely, it checks if there are differences between the current run
and oracles from previous runs. 
Oracles (with Dune files) are generated are generated with `frama-c-ptests`.

## Known Bugs

Some function calls with arguments may result in invalid PlusCal model that
raises this error `Parameters <XYZ> redefined at line <X>, column` this due to 
a bug in handling function arguments.

## License

Developed as part of a five-month internship at Asterios Technologies,
subsidiary of Safran Electronics and Defense.(LICENSE ?)

📄 **Full documentation available in [`doc/`](./doc/)**.


[1]: https://lamport.azurewebsites.net/tla/tla.html
[2]: https://fr.wikipedia.org/wiki/Leslie_Lamport
[3]: https://frama-c.com
[4]: https://hal.science/hal-01314832/document
[5]: https://dune.build/
[6]: https://lamport.azurewebsites.net/tla/tools.html
[7]: https://github.com/tlaplus/CommunityModules
[8]: https://learntla.com