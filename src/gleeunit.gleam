import gleam/io
import gleam/list
import gleam/function
import gleam/string

/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  detect_all_test_modules()
  |> run_suite(halts_on_error: True)
}

/// Similar to `main()` but meant to be called as a function not from cli:
/// - Allows to specificy a list of test modules
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
    |> string.ends_with(".gleam") == True
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

if erlang {
  import gleam/dynamic.{Dynamic}
  import gleam/int
  import gleam/result
  import gleam/string

  fn run_suite(
    test_module_files: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

    let result =
      test_module_files
      |> list.map(fn(test_module_file: String) {
        assert Ok(#(_test_prefix, test_module_file)) =
          string.split_once(test_module_file, "test/")
        test_module_file
      })
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> result.unwrap(Error(dynamic.from(Nil)))

    let exit_code = case result {
      Ok(_) -> 0
      Error(_) -> 1
    }

    case halts_on_error, exit_code {
      True, exit_code -> halt(exit_code)
      False, 0 -> Nil
      False, 1 -> Nil
      False, unhandled_exit_code -> {
        "Unexpected Error Code: " <> int.to_string(unhandled_exit_code)
        |> io.println_error
        Nil
      }
    }
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn gleam_to_erlang_module_name(path: String) -> String {
    path
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  external fn find_files(matching: String, in: String) -> List(String) =
    "gleeunit_ffi" "find_files"

  external type Atom

  type Encoding {
    Utf8
  }

  external fn dangerously_convert_string_to_atom(String, Encoding) -> Atom =
    "erlang" "binary_to_atom"

  type ReportModuleName {
    GleeunitProgress
  }

  type GleeunitProgressOption {
    Colored(Bool)
  }

  type EunitOption {
    Verbose
    NoTty
    Report(#(ReportModuleName, List(GleeunitProgressOption)))
  }

  external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
    "eunit" "test"

  fn detect_all_test_modules() -> List(String) {
    find_files("**/*.{erl,gleam}", in: "test")
    |> list.map(fn(test_module_file_name: String) {
      "test/" <> test_module_file_name
    })
  }

  external fn get_cwd() -> String =
    "glacier_ffi" "get_cwd_as_binary"

  external fn file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"
}

if javascript {
  fn detect_all_test_modules() -> List(String) {
    find_files(".gleam", in: "test")
  }

  fn run_suite(
    test_modules: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    find_matching_test_module_files(test_modules)
    |> run_suite_ffi(halts_on_error)
  }

  external fn run_suite_ffi(
    test_modules: List(String),
    halts_on_error: Bool,
  ) -> Nil =
    "./gleeunit_ffi.mjs" "main"

  external fn get_cwd() -> String =
    "./gleeunit_ffi.mjs" "cwd"

  external fn file_exists(absolute_file_name: String) -> Bool =
    "./gleeunit_ffi.mjs" "file_exists"

  external fn find_files(ext: String, in: String) -> List(String) =
    "./gleeunit_ffi.mjs" "find_ext_files_recursive_in"
}
