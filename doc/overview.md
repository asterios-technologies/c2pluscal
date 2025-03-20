# 🚀 Overview Guide

This guide gives an overview of the **C2PlusCal Transpiler** goals and structures.

---

## 🎯 Goals

The **C2PlusCal Transpiler** converts C code into PlusCal while preserving the program's semantics.
The goal is to verify properties and invariants directly on an equivalent representation of the source code to ensure its correctness.

It can be used for bug detection or refinement towards a TLA+ model that the source code should adhere to.

The transpiler utilizes **Frama-C** to extract the AST and preprocess the C code before applying the translation.

Based on: [`C2TLA+ by Amira METHNI`](https://hal.science/hal-01314832/document)

---

## 📂 Structure

- **`pc_gen`** → Translation pass
- **`pc`** → IR definitions
- **`pc_utils`** → Utility functions for `pc_gen` and dumping
- **`pc_dump`** → Handles `.tla` dumping
- **`pc_debug`** → Optional debug file generation

- **`config_dump`** → Handles `.cfg` dumping
- **`invariants_utils`** → Utilities for parsing configuration invariants

- **`main`** → Entry point of the algorithm
- **`module`** → Manages module imports
- **`options`** → Declares plugin options

📄 **For code documentation, see the HTML documentation section.**

---

## 🏁 Other Docs

- **Installation Guide:** See [`install.md`](./install.md)
- **Learn about the translation process:** See [`translation.md`](./translation.md)
- **See the limitations of the tool:** See [`limitations.md`](./limitations.md)
