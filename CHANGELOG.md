# Changelog

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
