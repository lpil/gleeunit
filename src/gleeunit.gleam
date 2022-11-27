/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  run_tests()
  |> halt()
}

pub fn run_tests() -> Result(Nil, Nil) {
  do_run_tests()
}

pub fn halt(result: Result(Nil, Nil)) -> Nil {
  let code = case result {
    Ok(_) -> 0
    Error(_) -> 1
  }
  do_halt(code)
}

if javascript {
  external fn do_run_tests() -> Result(Nil, Nil) =
    "./gleeunit_ffi.mjs" "run_tests"

  external fn do_halt(Int) -> Nil =
    "./gleeunit_ffi.mjs" "halt"
}

if erlang {
  import gleam/list
  import gleam/result
  import gleam/string
  import gleam/dynamic.{Dynamic}

  fn do_run_tests() -> Result(Nil, Nil) {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.map(gleam_to_erlang_module_name)
    |> list.map(dangerously_convert_string_to_atom(_, Utf8))
    |> run_eunit(options)
    |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
    |> result.unwrap(Error(dynamic.from(Nil)))
    |> result.nil_error()
    |> result.map(fn(_) { Nil })
  }

  external fn do_halt(Int) -> Nil =
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
}
