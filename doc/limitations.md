
# ⚠️ Limitations

This document outlines the current limitations of the **C2PlusCal Transpiler**.
Some of these features are planned for future updates.

---

## 🛑 Unsupported Features

Currently, the transpiler does not support:
- `#define` macros
- `typedef`
- `sizeof`
- `throw`
- `switch` statements
- `continue` statements
- Multi-threading (only a single process is supported, but multi-process support is planned)
- Generation of TypeOK verification

## 🔍 Known Issues

The following issues may affect the correctness of the translation:

- Address of a struct field or an array member is not fully supported.
- Indexing with a variable (e.g., `arr[a]`) may behave unexpectedly because PlusCal sequences are starting from `1`.
- Complex pointer dereferencing expressions such as `(*arr[i]->field)` may lead to incorrect translations.

---

These limitations will be progressively addressed in future versions of the transpiler.

