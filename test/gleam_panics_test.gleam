import gleam/dynamic
import gleam/function
import gleeunit/internal/gleam_panic.{
  Assert, BinaryOperator, Expression, FunctionCall, LetAssert, Literal,
  OtherExpression, Panic, Todo, Unevaluated,
}

@external(erlang, "gleeunit_test_ffi", "rescue")
@external(javascript, "./gleeunit_test_ffi.mjs", "rescue")
fn rescue(f: fn() -> t) -> Result(t, dynamic.Dynamic)

pub fn panic_test() {
  panic
  let assert Error(e) = rescue(fn() { panic })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.kind == Panic
  assert e.function == "panic_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "`panic` expression evaluated."
}

pub fn panic_message_test() {
  todo
  let assert Error(e) = rescue(fn() { panic as "oh my!" })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.kind == Panic
  assert e.function == "panic_message_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "oh my!"
}

pub fn todo_test() {
  let assert Error(e) = rescue(fn() { todo })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.kind == Todo
  assert e.function == "todo_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message
    == "`todo` expression evaluated. This code has not yet been implemented."
}

pub fn todo_message_test() {
  let get_names = fn() { ["Lucy"] }
  assert get_names() == ["Lucy", "Nubi"]
  let assert Error(e) = rescue(fn() { todo as "oh my!" })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.kind == Todo
  assert e.function == "todo_message_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "oh my!"
}

pub fn let_assert_test() {
  let assert 1 = 2
  let assert Error(e) =
    rescue(fn() {
      let assert 0 = function.identity(123)
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.function == "let_assert_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "Pattern match failed, no pattern matched the value."
  let assert LetAssert(value:, start:, pattern_start:, pattern_end:) = e.kind
  assert value == dynamic.int(123)
  assert start > 1
  assert pattern_start == start + 11
  assert pattern_end == pattern_start + 1
}

pub fn let_assert_message_test() {
  let assert Error(e) =
    rescue(fn() {
      let assert 0 = function.identity(321) as "oh dear"
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "let_assert_message_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "oh dear"
  let assert LetAssert(value:, start:, pattern_start:, pattern_end:) = e.kind
  assert value == dynamic.int(321)
  assert start > 1
  assert pattern_start == start + 11
  assert pattern_end == pattern_start + 1
}

pub fn assert_expression_test() {
  let assert Error(e) =
    rescue(fn() {
      let x = function.identity(False)
      assert x
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "assert_expression_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "Assertion failed."
  let assert Assert(start:, expression_start:, expression_end:, kind:) = e.kind
  assert start > 1
  assert expression_start == start + 7
  assert expression_end == expression_start + 1
  let assert OtherExpression(expression:) = kind
  assert expression.start == expression_start
  assert expression.end == expression_end
  assert expression.kind == Expression(value: dynamic.bool(False))
}

pub fn assert_expression_message_test() {
  let assert Error(e) =
    rescue(fn() {
      let x = function.identity(False)
      assert x as "maybe?"
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "assert_expression_message_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "maybe?"
  let assert Assert(start:, expression_start:, expression_end:, kind:) = e.kind
  assert start > 1
  assert expression_start == start + 7
  assert expression_end == expression_start + 1
  let assert OtherExpression(expression:) = kind
  assert expression.start == expression_start
  assert expression.end == expression_end
  assert expression.kind == Expression(value: dynamic.bool(False))
}

pub fn assert_function_test() {
  let assert Error(e) =
    rescue(fn() {
      assert function.identity(False)
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "assert_function_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "Assertion failed."
  let assert Assert(start:, expression_start:, expression_end:, kind:) = e.kind
  assert start > 1
  assert expression_start == start + 7
  assert expression_end == expression_start + 24
  let assert FunctionCall(arguments: [expression]) = kind
  assert expression.start == expression_start + 18
  assert expression.end == expression_end - 1
  assert expression.kind == Literal(value: dynamic.bool(False))
}

pub fn assert_function_message_test() {
  let assert Error(e) =
    rescue(fn() {
      assert function.identity(False) as "oh!"
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "assert_function_message_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "oh!"
  let assert Assert(start:, expression_start:, expression_end:, kind:) = e.kind
  assert start > 1
  assert expression_start == start + 7
  assert expression_end == expression_start + 24
  let assert FunctionCall(arguments: [expression]) = kind
  assert expression.start == expression_start + 18
  assert expression.end == expression_end - 1
  assert expression.kind == Literal(value: dynamic.bool(False))
}

pub fn assert_binary_operator_test() {
  let assert Error(e) =
    rescue(fn() {
      let a = False
      assert a && function.identity(False)
    })
  let assert Ok(e) = gleam_panic.from_dynamic(e)
  assert e.file == "test/gleam_panics_test.gleam"
  assert e.function == "assert_binary_operator_test"
  assert e.module == "gleam_panics_test"
  assert e.line > 1
  assert e.message == "Assertion failed."
  let assert Assert(start:, expression_start:, expression_end:, kind:) = e.kind
  assert start > 1
  assert expression_start == start + 7
  assert expression_end == expression_start + 29
  let assert BinaryOperator(operator:, left:, right:) = kind
  assert operator == "&&"
  assert left.start == expression_start
  assert left.end == left.start + 1
  assert left.kind == Expression(dynamic.bool(False))
  assert right.start == left.end + 4
  assert right.end == right.start + 24
  assert right.end == expression_end
  assert right.kind == Unevaluated
}
