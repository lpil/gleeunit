import gleeunit.{Colored, Coverage, Profile}

pub fn main() {
  gleeunit.main_with([Colored(True), Profile(True), Coverage(True)])
}

pub fn some_test() {
  let assert 2 = 1 + 1
}

pub fn some_other_test() {
  let assert 1 = 1 - 0
}
