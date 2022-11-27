import { readdir, readFile } from "fs/promises";
import { join as joinPath } from "path";
import { Ok, Error } from "./gleam.mjs";

async function* gleamFiles(directory) {
  let dirents = await readdir(directory, { withFileTypes: true });
  for (let dirent of dirents) {
    let path = joinPath(directory, dirent.name);
    if (dirent.isDirectory()) {
      yield* gleamFiles(path);
    } else if (path.endsWith(".gleam")) {
      yield path;
    }
  }
}

async function readRootPackageName() {
  let toml = await readFile("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function run_tests() {
  let passes = 0;
  let failures = 0;

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  for await (let path of await gleamFiles("test")) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    let module = await import(joinPath(dist, js_path));
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) continue;
      try {
        await module[fnName]();
        process.stdout.write(`\u001b[32m.\u001b[0m`);
        passes++;
      } catch (error) {
        let moduleName = "\n" + js_path.slice(0, -4);
        let line = error.line ? `:${error.line}` : "";
        process.stdout.write(`\n❌ ${moduleName}.${fnName}${line}: ${error}\n`);
        failures++;
      }
    }
  }

  console.log(`
${passes + failures} tests, ${failures} failures`);
  return failures ? new Error(undefined) : new Ok(undefined);
}

export async function halt(result) {
  process.exit(result === new Ok(undefined) ? 0 : 1);
}

export function crash(message) {
  throw new Error(message);
}
