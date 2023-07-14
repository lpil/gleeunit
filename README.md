# glacier_gleeunit

**This is a fork of [*Gleeunit*](https://hex.pm/packages/gleeunit) that allows it to be called as a library/function with a list of test modules instead of just via CLI.**

**This is a dependency of [*Glacier*](https://hex.pm/packages/glacier).**

**DO NOT install *Gleeunit* and this fork side by side in the same project.**

* * *

Gleam bindings to the Erlang EUnit test framework.

A custom test runner is included for when compiled to JavaScript running on
either NodeJS or Deno.

Documentation is available on [HexDocs](https://hexdocs.pm/glacier_gleeunit/index.html).

## Usage

Add this package to your Gleam project.

```sh
gleam add glacier_gleeunit --dev
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
pub fn the_universe_test() {
  let assert 1 = 1
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
