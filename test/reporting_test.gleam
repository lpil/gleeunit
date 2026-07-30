import gleam/dynamic
import gleeunit/internal/reporting
import testhelper

@external(erlang, "gleeunit_test_ffi", "rescue")
@external(javascript, "./gleeunit_test_ffi.mjs", "rescue")
fn rescue(f: fn() -> t) -> Result(t, dynamic.Dynamic)

fn discard(_string: String) -> Nil {
  Nil
}

pub fn todo_counted_as_todo_not_failure_test() {
  let state = reporting.new_state()
  let assert Error(error) = rescue(fn() { testhelper.run_todo() })
  let state =
    reporting.test_failed(state, "test_module", "my_test", error, discard)
  assert state.todos == 1
  assert state.failed == 0
  assert state.passed == 0
}

pub fn panic_still_counted_as_failure_test() {
  let state = reporting.new_state()
  let assert Error(error) = rescue(fn() { panic as "something broke" })
  let state =
    reporting.test_failed(state, "test_module", "my_test", error, discard)
  assert state.failed == 1
  assert state.todos == 0
  assert state.passed == 0
}

pub fn finished_returns_1_when_todos_present_test() {
  let state = reporting.State(passed: 5, failed: 0, skipped: 0, todos: 2)
  let exit_code = reporting.finished(state, discard)
  assert exit_code == 1
}

pub fn finished_returns_0_when_no_todos_or_failures_test() {
  let state = reporting.State(passed: 5, failed: 0, skipped: 0, todos: 0)
  let exit_code = reporting.finished(state, discard)
  assert exit_code == 0
}
