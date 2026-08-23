---
name: stack
description: How to build and test the project.
---

# Stack Skill

Refresh project configuration:

```bash
hpack
```

How to build:

```bash
stack build
```

How to test:

```bash
stack test
```

How to run:

```bash
stack run -- --help
stack run < test/golden/core/example-intro/input.yqst
```

How to test with a filter:

```bash
stack test --ta "-p <substring> --size-cutoff 100000 --hide-successes"
# Examples:
stack test --ta "-p structural-patterns --size-cutoff 100000 --hide-successes"
stack test --ta "-p Useful --size-cutoff 100000 --hide-successes"
```

How to accept golden tests:

```bash
stack test --ta "-p <substring> --accept --size-cutoff 100000 --hide-successes"
# Examples:
stack test --ta "-p structural-patterns --size-cutoff 100000 --hide-successes --accept"
```

How to run style checkers: see `.github/workflows/haskell.yml`.
To auto-format with Ormolu, use `--mode inplace` instead of
`--mode check` for the same file lists.
