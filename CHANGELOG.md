# Changelog

## v1.6.1 - 2025-07-23

- Fixed a bug where the number of skipped tests could be omitted.
- Fixed a bug where the wrong exit status code could be used.

## v1.6.0 - 2025-06-29

- Improved formatting of unexpected errors.
- Default EUnit timeout increased to 60 seconds.

## v1.5.1 - 2025-06-11

- Fixed a bug where Erlang modules in subdirectories within `test` could cause
  the test runner to fail.

## v1.5.0 - 2025-06-11

- Improved the format for unexpected errors on JavaScript.

## v1.4.0 - 2025-04-24

- Added support for `assert`.
- The console output format has been improved.
- The `gleeunit/should` module has been soft-deprecated in favour of Gleam's
  `assert`. In future releases this module will emit a warning when used.

## v1.3.1 - 2025-04-24

- Fixed printing of `let assert` crashes.

## v1.3.0 - 2025-02-02

- Fixed deprecation warnings with the Gleam standard library v0.53.0 or later.

## v1.2.0 - 2024-06-20

- The Gleam standard library version requirement has been relaxed to 
  `>= 0.33.0 and < 2.0.0`.

## v1.1.2 - 2024-03-30

- Added `Option` assertions: `should.be_some` and `should.be_none`

## v1.0.2 - 2023-12-19

- Added Gleam v0.33.0 version requirement.

## v1.0.1 - 2023-12-19

- Updated for Gleam v0.33.0.

## v1.0.0 - 2023-08-06

- Updated for Gleam v0.32.0.

## v0.11.0 - 2023-08-06

- Updated for Gleam v0.30.0.

## v0.10.1 - 2023-01-23

- Fixed a bug where unicode characters would be printed incorrectly.

## v0.10.0 - 2023-01-19

- The output for failed assertions when targeting Erlang has been improved.

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
