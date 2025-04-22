# Changelog

## v1.3.1001 - 2025-04-22

- Merge in upstream gleeunit 1.3.0.

## v1.2.1001 - 2024-06-20

- Merge in upstream gleeunit 1.2.0.

## v1.1.2001 - 2024-04-11

- Merge in upstream gleeunit 1.1.2.
- Cleanups

## v0.11.40 - 2024-04-11

- Remove now unused dep gleam/erlang.

## v0.11.30 - 2024-04-11

- Updates for Gleam 1.0 to be used by Glacier 0.9 or later.

## v0.11.10 - 2023-03-05

- Merge with gleeunit main.

## v0.11.0 - 2023-03-03

- Dependency upgrade and code fixes to be in line with 0.27.0.

## v0.10.12-glacier - 2023-01-31

- Attempt to fix Deno testing on Glacier.

## v0.10.1 - 2023-01-23

- Fixed a bug where unicode characters would be printed incorrectly.

## v0.10.3-glacier - 2023-01-19

- Make hack a bit more generic

## v0.10.2-glacier - 2023-01-19

- Fixed some bugs around erlang that I missed.

## v0.10.1-glacier - 2023-01-19

- Fixed more bugs around argv that should not exist and files that are not valid gleam test modules.

## v0.10.0-glacier - 2023-01-19

- Merged upstream gleeunit 0.10.0.
- Fixed running on deno with help of <https://github.com/brettkolodny> 💜.

## v0.9.0-glacier - 2023-01-08

- Fork of `Gleeunit` 0.9.0:
  - This is a fork of [*Gleeunit*](https://hex.pm/packages/gleeunit) that allows it to
    be called as a library/function with a list of test modules instead of
    just via CLI.
  - This fork is being used primarily as a test runner target by
    <https://hex.pm/packages/glacier>.
  - I will attempt to keep this in sync with `lpil/gleeunit`.
