import gleam/bit_array
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/option.{type Option}
import gleam/result
import gleam/string
import gleeunit/internal/gleam_panic.{type GleamPanic}

pub type State {
  State(passed: Int, failed: Int, skipped: Int)
}

pub fn new_state() -> State {
  State(passed: 0, failed: 0, skipped: 0)
}

pub fn finished(state: State) -> Int {
  case state {
    State(passed: 0, failed: 0, skipped: 0) -> {
      io.println("\nNo tests found!")
      1
    }
    State(failed: 0, skipped: 0, ..) -> {
      let message = "\n" <> int.to_string(state.passed) <> " tests, no failures"
      io.println(green(message))
      0
    }
    State(skipped: 0, ..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, "
        <> int.to_string(state.failed)
        <> " failures"
      io.println(red(message))
      0
    }
    State(failed: 0, ..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, 0 failures, "
        <> int.to_string(state.skipped)
        <> " skipped"
      io.println(yellow(message))
      1
    }
    State(..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, "
        <> int.to_string(state.failed)
        <> " failures, "
        <> " skipped"
      io.println(red(message))
      1
    }
  }
}

pub fn test_passed(state: State) -> State {
  io.print(green("."))
  State(..state, passed: state.passed + 1)
}

pub fn test_failed(
  state: State,
  module: String,
  function: String,
  error: dynamic.Dynamic,
) -> State {
  let message = case gleam_panic.from_dynamic(error) {
    Ok(error) -> {
      let src = option.from_result(read_file(error.file))
      format_gleam_error(error, module, function, src)
    }
    Error(_) -> format_unknown(error)
  }

  io.print("\n" <> message)
  State(..state, failed: state.failed + 1)
}

fn format_unknown(error: dynamic.Dynamic) -> String {
  "\nAn unexpected error occurred:\n\n" <> string.inspect(error)
}

fn format_gleam_error(
  error: GleamPanic,
  module: String,
  function: String,
  src: Option(BitArray),
) -> String {
  let location = grey(error.file <> ":" <> int.to_string(error.line))

  case error.kind {
    gleam_panic.Panic -> {
      string.concat([
        bold(red("panic")) <> " " <> location <> "\n",
        blue(" test") <> ": " <> module <> "." <> function <> "\n",
        blue(" info") <> ": " <> error.message <> "\n",
      ])
    }

    gleam_panic.Todo -> {
      string.concat([
        bold(yellow("todo")) <> " " <> location <> "\n",
        blue(" test") <> ": " <> module <> "." <> function <> "\n",
        blue(" info") <> ": " <> error.message <> "\n",
      ])
    }

    gleam_panic.Assert(start:, expression_end:, kind:, ..) -> {
      string.concat([
        bold(red("assert")) <> " " <> location <> "\n",
        blue(" test") <> ": " <> module <> "." <> function <> "\n",
        code_snippet(src, start, expression_end),
        assert_info(kind),
        blue(" info") <> ": " <> error.message <> "\n",
      ])
    }

    // TODO: include the whole expression
    gleam_panic.LetAssert(start:, pattern_end:, value:, ..) -> {
      string.concat([
        bold(red("let assert")) <> " " <> location <> "\n",
        blue(" test") <> ": " <> module <> "." <> function <> "\n",
        code_snippet(src, start, pattern_end),
        blue("value") <> ": " <> string.inspect(value) <> "\n",
        blue(" info") <> ": " <> error.message <> "\n",
      ])
    }
  }
}

fn assert_info(kind: gleam_panic.AssertKind) -> String {
  case kind {
    gleam_panic.BinaryOperator(operator:, left:, right:) ->
      string.concat([assert_value(" left", left), assert_value("right", right)])

    gleam_panic.FunctionCall(arguments:) -> todo

    gleam_panic.OtherExpression(expression:) -> ""
  }
}

fn assert_value(name: String, value: gleam_panic.AssertedExpression) -> String {
  case value.kind {
    gleam_panic.Expression(value:) ->
      blue(name) <> ": " <> string.inspect(value) <> "\n"

    gleam_panic.Literal(..) | gleam_panic.Unevaluated -> ""
  }
}

fn code_snippet(src: Option(BitArray), start: Int, end: Int) -> String {
  {
    use src <- result.try(option.to_result(src, Nil))
    use snippet <- result.try(bit_array.slice(src, start, end - start))
    use snippet <- result.try(bit_array.to_string(snippet))
    let snippet = blue(" code") <> ": " <> snippet <> "\n"
    Ok(snippet)
  }
  |> result.unwrap("")
}

pub fn test_skipped(state: State, module: String, function: String) -> State {
  io.print("\n" <> module <> "." <> function <> yellow(" skipped"))
  State(..state, skipped: state.skipped + 1)
}

fn bold(text: String) -> String {
  "\u{001b}[1m" <> text <> "\u{001b}[22m"
}

fn blue(text: String) -> String {
  "\u{001b}[34m" <> text <> "\u{001b}[39m"
}

fn yellow(text: String) -> String {
  "\u{001b}[33m" <> text <> "\u{001b}[39m"
}

fn green(text: String) -> String {
  "\u{001b}[32m" <> text <> "\u{001b}[39m"
}

fn red(text: String) -> String {
  "\u{001b}[31m" <> text <> "\u{001b}[39m"
}

fn grey(text: String) -> String {
  "\u{001b}[90m" <> text <> "\u{001b}[39m"
}

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, dynamic.Dynamic) {
  case read_file_text(path) {
    Ok(text) -> Ok(bit_array.from_string(text))
    Error(e) -> Error(e)
  }
}

@external(javascript, "../../gleeunit_ffi.mjs", "read_file")
fn read_file_text(path: String) -> Result(String, dynamic.Dynamic)
