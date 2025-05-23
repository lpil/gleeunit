import { Ok, Error } from "./gleam.mjs";

export function rescue(f) {
  try {
    return new Ok(f());
  } catch (e) {
    return new Error(e);
  }
}
