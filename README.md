# gleeunit

A simple test runner for Gleam, using EUnit on Erlang and a custom runner on JS.

[![Package Version](https://img.shields.io/hexpm/v/gleeunit)](https://hex.pm/packages/gleeunit)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleeunit/)


```sh
gleam add gleeunit@1 --dev
```
```gleam
// In test/yourapp_test.gleam
import gleeunit

pub fn main() {
  gleeunit.main()
}
```

Now any public function with a name ending in `_test` in the `test` directory
will be found and run as a test.

```gleam
pub fn some_function_test() {
  assert some_function() == "Hello!"
}
```

Run the tests by entering `gleam test` in the command line.

### Deno

If using the Deno JavaScript runtime, you will need to add the following to your
`gleam.toml`.

```toml
[javascript.deno]
allow_read = [
  "gleam.toml",
  "test",
  "build",
]
```

### Parallel test execution

If your tests are independent and don't share mutable state, you can run them
in parallel using EUnit's `inparallel` mode:

```gleam
// In test/yourapp_test.gleam
import gleeunit

pub fn main() {
  gleeunit.main_parallel()
}
```

To switch back to sequential execution, change `main_parallel()` to `main()`.

On JavaScript targets, `main_parallel()` falls back to sequential execution
since Node.js and Deno are single-threaded.

#### When parallel tests are safe

Tests can run in parallel when each test is fully isolated. For example:

- Each test creates its own in-memory database (no shared connections)
- No named processes are started (use unnamed processes or unique names)
- No shared ETS tables with `named_table` (unnamed ETS is fine)
- No writes to the file system

#### When parallel tests are not safe

Tests that share mutable state will produce flaky failures when parallelized:

- A shared database connection or shared database state between tests
- Named processes (e.g., `gen_server` registered with a fixed name)
- Shared named ETS tables modified by multiple tests
- Tests that read/write the same files
- Tests that depend on execution order
