# Glacier's Gleeunit

This is a fork of [Gleeunit](https://hex.pm/packages/gleeunit) that allows
Gleeunit to be both called:

1. with one or many given Gleam test modules instead of running all of them.
2. as a library function with a list of test modules instead of just
   via `gleam test` CLI.

The combination of both allows unit tests to be executed from glacier which
figures out which test modules to run.

As a side effect you can run single test modules with `glacier_gleeunit` from
the CLI.

**This is a dependency of [Glacier](https://hex.pm/packages/glacier).**

**DO NOT install `gleeunit` and `glacier_gleeunit` side by side in the same project.**

- `glacier_gleeunit` is meant as a straightforward replacement for `gleeunit`.
- `glacier_gleeunit` tries to stay up to date with `gleeunit` upstream,
   back-porting any changes made there.
- As a consequence `glacier_gleeunit` uses the same Gleam module files and thus
  the modules would collide if both are installed.

## Usage

Remove `gleeunit` and add this package to your Gleam project.

```shell
gleam remove gleeunit
gleam add glacier_gleeunit --dev
```

* * *

For further usage instructions, see [gleeunit's README.md](./README.md).
