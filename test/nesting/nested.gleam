pub fn nested_1_test() {
  assert 1 = 1
}

pub fn nested_2_test() {
  assert 3 = 2 + 1
}

pub fn record_assert_test() {
  // assert Ok(_) = Error(#(1, "error", Nil))
  Nil
}
