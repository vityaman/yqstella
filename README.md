# YQStella

![Stella](./docs/stella.webp)

> Unpleasant telegrams always arrive without delay
>
> -- Emmanuel Herman, 📖 Soviet writer

Yandex Query [Stella](https://fizruk.github.io/stella/) is
an experimental [YQL](https://ytsaurus.tech/docs/en/yql/)
frontend to learn type systems during the course by
[Nikolay Kudasov](https://github.com/fizruk).

YQStella type annotation strictly follows the Stella specification,
because it is needed in order to submit it as a coursework, but
operational semantics is not aligned, because some features are
unsupported by the YQL (e.g. recursion, references, errors) and
because YQL transformation pipeline conforms to other rules.

## Goal

There are following questions under this project:

1. How to create a bidirectional type checker for a programming
   language with a non-trivial type system?

2. Is it possible to map YQL representation to a different than
   SQL language with a typical functional language syntax?

3. Can it be more convinient to query data using a typical
   functional language syntax than using the SQL?

4. Is it possible to create a highly maintainable functional
   language frontend, while preserving type annotation recovery,
   good source locations, compiler as a library and so on, that
   production languages have?

## Extensions

| Extension                           | Status         |
| ----------------------------------- | -------------- |
| `core`                              | ✅ `READY`     |
| `#structural-patterns`              | ✅ `READY`     |
| `#let-bindings`                     | ✅ `READY`     |
| `#let-patterns`                     | ✅ `READY`     |
| `#let-many-bindings`                | ⏳ `TBD`       |
| `#letrec-bindings`                  | ⏳ `TBD`       |
| `#fixpoint-combinator`              | 🧪 `TYPECHECK` |
| `#nested-function-declarations`     | ✅ `READY`     |
| `#type-aliases`                     | ✅ `READY`     |
| `#natural-literals`                 | ✅ `READY`     |
| `#predecessor`                      | ⏳ `TBD`       |
| `#arithmetic-operators`             | ✅ `READY`     |
| `#comparison-operators`             | ✅ `READY`     |
| `#logical-operators`                | ✅ `READY`     |
| `#nullary-functions`                | ✅ `READY`     |
| `#multiparameter-functions`         | ✅ `READY`     |
| `#curried-multiparameter-functions` | ⏳ `TBD`       |
| `#unit-type`                        | ✅ `READY`     |
| `#pairs`                            | ✅ `READY`     |
| `#tuples`                           | ✅ `READY`     |
| `#records`                          | ✅ `READY`     |
| `#sum-types`                        | ✅ `READY`     |
| `#variants`                         | ✅ `READY`     |
| `#nullary-variant-labels`           | ✅ `READY`     |
| `#lists`                            | ✅ `READY`     |
| `#type-ascriptions`                 | ✅ `READY`     |
| `#pattern-ascriptions`              | ⏳ `TBD`       |
| `#sequencing`                       | ⏳ `TBD`       |
| `#references`                       | ⏳ `TBD`       |
| `#exception-type-declaration`       | ⏳ `TBD`       |
| `#open-variant-exceptions`          | ⏳ `TBD`       |
| `#exceptions`                       | ⏳ `TBD`       |
| `#panic`                            | ⏳ `TBD`       |
| `#structural-subtyping`             | ⏳ `TBD`       |
| `#top-type`                         | ⏳ `TBD`       |
| `#bottom-type`                      | ⏳ `TBD`       |
| `#type-cast`                        | ⏳ `TBD`       |
| `#universal-types`                  | ⏳ `TBD`       |
| `#general-recursion`                | ⏳ `TBD`       |

## YQL Correspondence

A program entrypoint is the `main` function and its arguments
are mapped to query parameters.

The query result is a single row with a single `result` column
of the `main` function return type.

Here is a mapping from YQStella to YQL types:

| **YQStella**          | **YQL**                   |
| --------------------- | ------------------------- |
| `Bool`                | `Bool`                    |
| `Nat`                 | `Uint64`                  |
| `Unit`                | `Void`                    |
| `{T...}`              | `Tuple<T...>`             |
| `{(a : T)...}`        | `Struct<(a: T)...>`       |
| `T + U`               | `Variant<inl: T, inr: U>` |
| `<\| (l : T0)... \|>` | `Variant<(l: T)...>`      |
| `<\| l... \|>`        | `Variant<(l: Unit)>`      |

## Getting Started

See the [Haskell GitHub Workflow](./.github/workflows/haskell.yml)
for the instructions how to build the CLI tool. Also note, that in
case you are the Fizruk, consider checking `--help` option and
pay attention to `--fizruk` flag that made especially for a
difftesting.

A tiny VSCode extension with a syntax highlighting is also available,
but it is maintained completely by an LLM.

You also need to have the [Docker](https://www.docker.com/) installed to run
golden tests, where YQStella type checker is differentially tested against
the Stella reference implementation and is executed on the tool called
[Minirun](https://github.com/ydb-platform/ydb/tree/main/yql/essentials/tools/minirun)
(with pure YQL provider).

## References

- [Types and Programming Languages by Benjamin C. Pierce](https://www.amazon.com/Types-Programming-Languages-MIT-Press/dp/0262162091)

- [YTsaurus, YQL Documentation](https://ytsaurus.tech/docs/en/yql/)

- [YDB, YQL Essentials Source Code](https://github.com/ydb-platform/ydb/tree/main/yql/essentials)

- [LUC MARANGET, Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/warn.pdf)
