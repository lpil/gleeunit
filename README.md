# gleeunit

A simple test runner for Gleam, using EUnit on Erlang.

Documentation is available on [HexDocs](https://hexdocs.pm/gleeunit/index.html).

## Usage

Add this package to your Gleam project.

```sh
gleam add gleeunit --dev
```

And then call the `gleeunit.main` function from your test main function.

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
