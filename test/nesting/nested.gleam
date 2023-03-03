pub fn nested_1_test() {
  let assert 1 = 1
}

pub fn nested_2_test() {
  let assert 3 = 2 + 1
}

pub fn record_assert_test() {
  // assert Ok(_) = Error(#(1, "error", Nil))
  Nil
}

pub fn todo_test() {
  // todo("Hello, Joe!")
  Nil
}

pub fn inexhaustive_case_test() {
  // case Ok(123) {
  //   Ok(12) -> Nil
  //   Error(_) -> Nil
  // }
  Nil
}

pub fn should_equal_test() {
  // Ok(#(123, "ok"))
  // |> should.equal(Error(Nil))
  Nil
}

pub fn should_not_equal_test() {
  // Error(Nil)
  // |> should.not_equal(Error(Nil))
  Nil
}
