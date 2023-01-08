# Changelog

## v0.9.0 glacier_gleeunit - 2023-01-08

- Fork of Gleeunit 0.9.0:
  - This is a fork of [*Gleeunit*](https://hex.pm/gleeunit) that allows it to
    be called as a library/function with a list of test modules instead of
    just via CLI.
  - This fork is being used primarily as a test runner target by
    <https://hexdocs.pm/glacier>.
  - I will attempt to keep this up to date with <https://hexdocs.pm/gleeunit>.

## v0.9.0 - 2023-01-08

- The JavaScript runner now also supports the Deno runtime.

## v0.8.0 - 2022-12-17

- `should.be_ok` and `should.be_error` now return the contained value when the
  assertion passes.

## v0.7.2 - 2022-11-19

- Update for Gleam v0.25.0.

## v0.7.1 - 2022-11-11

- Fixed a bug where project names containing numbers would not run correctly on
  JavaScript.

## v0.7.0 - 2022-09-24

- Line numbers are printed on JS for assertions.

## v0.6.2 - 2022-07-15

- Fixed a bug where assertions in JavaScript tests could fail to report an
  error due to them being async.

## v0.6.1 - 2022-01-26

- Fixed a bug where failed tests on the JavaScript target would crash the `main`
  function.
- Fixed a bug where tests on the JavaScript target could succeed regardless of
  return value.

## v0.6.0 - 2022-01-09

- Added support for OTP versions below 23.
- Added support for running tests on the JavaScript target.

## v0.5.0 - 2021-12-05

- Updated for Gleam v0.18.0.
- Added `gleeunit/should` module containing assertions.

## v0.4.0 - 2021-11-01

- Slightly improved failure format.

## v0.3.0 - 2021-10-31

- Fixed Hex package which was missing some files.

## v0.2.0 - 2021-10-31

- `gleeunit.discover_and_run_tests` removed.
- `gleeunit.main` added.

## v0.1.0 - 2021-10-31

- Initial release.
