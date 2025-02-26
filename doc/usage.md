# 🚀 Usage Guide

This guide explains how to use the **C to PlusCal Transpiler** to convert C programs into **PlusCal** specifications.

---

## 📌 Basic Usage

To transpile a C program (`test.c`) into PlusCal, run:

```bash
frama-c -pluscal test.c
```

This generates two output files:
- `test.tla`: The PlusCal specification.
- `test.cfg`: The configuration file.

---

## 🔍 Available Options

The transpiler provides additional options to enhance the conversion process:

### 🔹 `-debug-dump`
Generates a `.dump` file containing debug information about the translation.

```bash
frama-c -pluscal -debug-dump test.c
```

### 🔹 `-check-label <function_name>`
Adds a `"Check_<function_name>"` label just before the end of the specified function.
This allows writing properties and invariants on local variables before they are removed from the stack.

Example:

```bash
frama-c -pluscal -check-label "main" test.c
```

### 🔹 `-expect <file>`
Generates invariants in the `.cfg` file with the expecting value of variables of the program, written in the`.expect` file

Example:

```bash
frama-c -pluscal -expect "test.expect" test.c
```

---

## 📄 Writing Invariants and Properties

You can also manually write **invariants** and **properties** in the corresponding sections.

For example, to verify that the variable `x` in `main` has a value of `3` before the function returns:

```tla
Inv ==  /\ (pc[1] = "Check_main" => load(my_stack[1], x_ptr_main[1]) = 3)
```

📌 **Note:**
- `[1]` refers to the process ID, allowing multi-process verification.
- Process `0` is used for global initialization and does not translate C functions.

Other type of **invariants** and **properties** can be written depending on the program.

---

## 🏁 Next Steps

- **Installation Guide:** See [`install.md`](./install.md)
- **Learn about the translation process:** See [`doc/`](./architecture.md/)
