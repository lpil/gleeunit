import gleeunit
import gleam/list
import gleam/string

pub fn main() {
  let start_args = start_args()

  let start_args =
    list.filter(
      start_args,
      fn(arg) {
        string.ends_with(arg, "glacier_gleeunit/gleam.main.mjs") == False && arg != "--"
      },
    )

  case start_args {
    [] -> gleeunit.main()
    start_args -> gleeunit.run(start_args, halts_on_error: False)
  }
}

pub fn some_test() {
  assert 2 = 1 + 1
}

pub fn some_other_test() {
  assert 1 = 1 - 0
}

if erlang {
  fn start_args() -> List(String) {
    erlang.start_arguments()
  }
}

if javascript {
  external fn start_args() -> List(String) =
    "./gleeunit_ffi.mjs" "start_args"
}
