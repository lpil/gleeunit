import gleam/io

pub fn run_todo() -> Nil {
  todo
}

pub fn run_todo_message(message: String) -> Nil {
  todo as message
}

pub fn run_assert(value: Bool) -> Nil {
  assert value
}
