import gleeunit

pub fn main() {
  case start_args() {
    [] -> gleeunit.main()
    start_args -> gleeunit.run(start_args, halts_on_error: False)
  }
}

pub fn some_test() {
  let assert 2 = 1 + 1
}

pub fn some_other_test() {
  let assert 1 = 1 - 0
}

if erlang {
  import gleam/erlang

  fn start_args() -> List(String) {
    erlang.start_arguments()
  }
}

if javascript {
  import gleam/list
  import gleam/string

  fn start_args() -> List(String) {
    do_start_args()
    // This is a work around around a bug introduced in 0.26.0:
    |> list.filter(fn(arg) {
      arg != "--" && string.ends_with(arg, "/gleam.main.mjs") == False
    })
  }

  external fn do_start_args() -> List(String) =
    "./gleeunit_ffi.mjs" "start_args"
}
