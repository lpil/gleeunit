if javascript {
  /// Find and run all test functions for the current project using Erlang's EUnit
  /// test framework.
  ///
  /// Any Erlang or Gleam function in the `test` directory with a name editing in
  /// `_test` is considered a test function and will be run.
  ///
  /// If running on JavaScript tests will be run with a custom test runner.
  ///
  pub fn main() -> Nil {
    do_main()
  }

  external fn do_main() -> Nil =
    "./gleeunit_ffi.mjs" "main"
}

if erlang {
  import gleam/list
  import gleam/result
  import gleam/string
  import gleam/io
  import gleam/dynamic.{Dynamic}

  pub fn main() -> Nil {
    do_main([Colored(True)])
  }

  pub fn main_with(opts: List(GleeunitProgressOption)) -> Nil {
    do_main(opts)
  }

  fn do_main(opts: List(GleeunitProgressOption)) -> Nil {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, opts))]
    let coverage = list.contains(opts, Coverage(True))

    case coverage {
      True -> {
        do_cover_start()
        find_files(matching: "**/*.{erl,gleam}", in: "src")
        |> list.map(gleam_to_erlang_module_name)
        |> list.map(dangerously_convert_string_to_atom(_, Utf8))
        |> list.each(recompile_for_coverage)
      }
      False -> Nil
    }

    let result =
      find_files(matching: "**/*.{erl,gleam}", in: "test")
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> result.unwrap(Error(dynamic.from(Nil)))

    case coverage {
      True -> do_cover_stop()
      False -> Nil
    }

    let code = case result {
      Ok(_) -> 0
      Error(_) -> 1
    }
    halt(code)
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn gleam_to_erlang_module_name(path: String) -> String {
    path
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  fn recompile_for_coverage(module) -> Atom {
    case do_cover_recompile_module(module) {
      Ok(_) -> {
        module
      }
      Error(err) -> {
        io.debug(#("cannot compile coverage", err))
        module
      }
    }
  }

  external fn do_cover_start() -> Nil =
    "gleeunit_ffi" "start_coverage"

  external fn do_cover_stop() -> Nil =
    "gleeunit_ffi" "stop_coverage"

  external fn do_cover_recompile_module(module: Atom) -> Result(Nil, String) =
    "gleeunit_ffi" "compile_coverage"

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

  pub type GleeunitProgressOption {
    Colored(Bool)
    Profile(Bool)
    Coverage(Bool)
  }

  type EunitOption {
    Verbose
    NoTty
    Report(#(ReportModuleName, List(GleeunitProgressOption)))
  }

  external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
    "eunit" "test"
}
