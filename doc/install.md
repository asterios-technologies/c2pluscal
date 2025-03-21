# 🛠️ Installation Guide

This guide explains how to install and set up the **C2PlusCal Transpiler**.

---

## 📌 Prerequisites

Before installing, ensure that you have the following dependencies installed on your system:

- **OCaml** (recommended version: `>= 4.12`)
- **Dune** (build system for OCaml)
- **Frama-C** (for C code analysis)

### 📦 Installing Dependencies

#### 🔹 Install OCaml and Dune

Using `opam` (recommended OCaml package manager):

```bash
opam install ocaml dune
```

#### 🔹 Install Frama-C

To install Frama-C:

```bash
opam install frama-c
```

Check the installation:

```bash
frama-c -version
```

---

## 🔨 Building the Transpiler

Once dependencies are installed, navigate to the `src/` directory and compile the project:

```bash
dune build
```

If the build succeeds, you can install the transpiler:

```bash
dune install
```

---

## ✅ Verifying the Installation

To ensure everything is correctly set up, run:

```bash
frama-c -pluscal-h
```

If the transpiler is correctly installed, you should see its options listed.

---

## 🏁 Other Docs

- **Overview** See [`overview.md`](./overview.md)
- **Learn about the translation process:**: See [`translation.md`](./translation.md)
- **See the limitations of the tool:** See [`limitations.md`](./limitations.md)
