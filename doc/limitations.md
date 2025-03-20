
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

The following known issues may affect the correctness of the translation:

- Missing Frama-C labels in some `if` or `loop` statement, because of the way we skip `blocks` in `pc_gen.visit_stmt_aux`
  -> Possible solution : keep a trace of all statements we visited to retrieve their labels and skip them when visiting,
     instead of just skipping whole `blocks` contained in `if` and `loop` statements.
- Frama-C labels may cause some duplicate
  -> Possible solution : add `_line_n`, with `n` the line in the C code, to make labels unique.
- Binop of pointers may be incorrect when pointer are of form `[ptr |-> ..., ref |-> ...]`.
  -> See `pc_dump.dump_pc_binop_ptr`

---

These limitations will be progressively addressed in future versions of the transpiler.

