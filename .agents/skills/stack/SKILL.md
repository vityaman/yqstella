---
name: stack
description: How to build and test the project.
---

# Stack Skill

How to build:

```bash
hpack && stack build
```

How to test:

```bash
hpack && stack test
```

How to test with a filter:

```bash
hpack && stack test --ta "-p <substring> --size-cutoff 100000"
# Examples:
hpack && stack test --ta "-p structural-patterns --size-cutoff 100000"
hpack && stack test --ta "-p Useful --size-cutoff 100000"
```

How to accept golden tests:

```bash
hpack && stack test --ta "-p <substring> --accept --size-cutoff 100000"
# Examples:
hpack && stack test --ta "-p structural-patterns --size-cutoff 100000 --accept"
```

How to run linters:

See "Check Haskell Formatting" and "Check Haskell Style"
at `.github/workflows/haskell.yml` .
