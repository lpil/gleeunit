import { Ok, Error } from "./gleam.mjs";

// The maximum amount of time running all tests is expected to take.
// This is used to set the timeout for the delayed promise test, to ensure
// it runs after all other tests have had a chance to execute.
const MAX_TEST_TIME = 4000;

export function rescue(f) {
  try {
    return new Ok(f());
  } catch (e) {
    return new Error(e);
  }
}

export function promise_fail_test() {
  new Promise(() => {
    throw new Error("Promise panicked");
  });
}

export function delayed_promise_fail_test() {
  new Promise((_, reject) => {
    setTimeout(() => {
      reject(new Error("Promise panicked"));
    }, MAX_TEST_TIME);
  });
}
