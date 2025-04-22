import gleam/function
import gleam/io
@target(javascript)
import gleam/list
@target(javascript)
import gleam/string

/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name ending in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  detect_all_test_modules()
  |> run_suite(halts_on_error: True)
}

@target(javascript)
fn detect_all_test_modules() -> List(String) {
  find_files(".gleam", in: "test")
}

@target(erlang)
fn detect_all_test_modules() -> List(String) {
  find_files("**/*.{erl,gleam}", in: "test")
  |> list.map(fn(test_module_file_name: String) {
    "test/" <> test_module_file_name
  })
}

@target(erlang)
fn run_suite(
  test_module_files: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil {
  let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

  let result =
    test_module_files
    |> list.map(fn(test_module_file: String) {
      let assert Ok(#(_test_prefix, test_module_file)) =
        string.split_once(test_module_file, "test/")
      test_module_file
    })
    |> list.map(gleam_to_erlang_module_name)
    |> list.map(dangerously_convert_string_to_atom(_, Utf8))
    |> run_eunit(options)

  let code = case result {
    Ok(_) -> 0
    Error(_) -> 1
  }

  case halts_on_error, code {
    True, code -> halt(code)
    False, 0 -> Nil
    False, _ -> Nil
  }
}

@target(javascript)
fn run_suite(
  test_modules: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil {
  find_matching_test_module_files(test_modules)
  |> run_suite_ffi(halts_on_error)
}

@target(javascript)
@external(javascript, "./gleeunit_ffi.mjs", "main")
fn run_suite_ffi(
  test_modules test_modules: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil

/// Similar to `main()` but meant to be called as a function not from cli:
/// - Allows to specificity a list of test modules
/// - Allows customization of the halt behavior
///
pub fn run(
  test_module_files: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil {
  test_module_files
  |> find_matching_test_module_files
  |> run_suite(halts_on_error)
}

fn find_matching_test_module_files(test_module_files) {
  test_module_files
  |> list.filter(fn(module_name) {
    module_name
    |> string.ends_with(".gleam")
    == True
  })
  |> list.filter(fn(module_name) {
    let absolute_module_file_name = get_cwd() <> "/" <> module_name

    file_exists(absolute_module_file_name)
    |> function.tap(fn(exists) {
      case exists {
        True -> Nil
        False ->
          io.println_error(
            "Error: Could not find " <> absolute_module_file_name,
          )
      }
    })
  })
}

@external(erlang, "gleeunit_glacier_ffi", "get_cwd_as_binary")
@external(javascript, "./gleeunit_ffi.mjs", "cwd")
fn get_cwd() -> String

@external(erlang, "filelib", "is_regular")
@external(javascript, "./gleeunit_ffi.mjs", "file_exists")
fn file_exists(absolute_file_name absolute_file_name: String) -> Bool

// Vanilla gleeunit below, mostly

// We do not need this from gleeunit anymore as `run()` covers that.
//
// @target(javascript)
// @external(javascript, "./gleeunit_ffi.mjs", "main")
// fn do_main() -> Nil

@target(erlang)
import gleam/list
@target(erlang)
import gleam/string

@target(erlang)
@external(erlang, "erlang", "halt")
fn halt(a: Int) -> Nil

@target(erlang)
fn gleam_to_erlang_module_name(path: String) -> String {
  path
  |> string.replace(".gleam", "")
  |> string.replace(".erl", "")
  |> string.replace("/", "@")
}

@external(erlang, "gleeunit_ffi", "find_files")
@external(javascript, "./gleeunit_ffi.mjs", "find_ext_files_recursive_in")
fn find_files(matching matching: String, in in: String) -> List(String)

@target(erlang)
type Atom

@target(erlang)
type Encoding {
  Utf8
}

@target(erlang)
@external(erlang, "erlang", "binary_to_atom")
fn dangerously_convert_string_to_atom(a: String, b: Encoding) -> Atom

@target(erlang)
type ReportModuleName {
  GleeunitProgress
}

@target(erlang)
type GleeunitProgressOption {
  Colored(Bool)
}

@target(erlang)
type EunitOption {
  Verbose
  NoTty
  Report(#(ReportModuleName, List(GleeunitProgressOption)))
}

@target(erlang)
@external(erlang, "gleeunit_ffi", "run_eunit")
fn run_eunit(a: List(Atom), b: List(EunitOption)) -> Result(Nil, a)
