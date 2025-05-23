import gleam/dynamic

pub type GleamPanic {
  GleamPanic(
    module: String,
    function: String,
    line: Int,
    message: String,
    kind: PanicKind,
  )
}

pub type PanicKind {
  Todo
  Panic
  LetAssert(
    start: Int,
    pattern_start: Int,
    pattern_end: Int,
    value: dynamic.Dynamic,
  )
  Assert(
    start: Int,
    expression_start: Int,
    expression_end: Int,
    kind: AssertKind,
  )
}

pub type AssertKind {
  BinaryOperator(
    operator: String,
    left: AssertedExpression,
    right: AssertedExpression,
  )
  FunctionCall(arguments: List(AssertedExpression))
  OtherExpression(expression: AssertedExpression)
}

pub type AssertedExpression {
  AssertedExpression(start: Int, end: Int, kind: ExpressionKind)
}

pub type ExpressionKind {
  Literal(value: dynamic.Dynamic)
  Expression(value: dynamic.Dynamic)
  Unevaluated
}

@external(erlang, "gleam_panic_ffi", "from_dynamic")
@external(javascript, "./gleam_panic_ffi.mjs", "from_dynamic")
pub fn from_dynamic(data: dynamic.Dynamic) -> Result(GleamPanic, Nil)
