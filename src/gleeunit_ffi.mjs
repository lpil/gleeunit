import { readdir, readFile } from "fs/promises";
import { join as joinPath } from "path";

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
    let matches = line.match(/\s*name\s=\s"([a-z_]+)"/);
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main() {
  let passes = 0;
  let failures = 0;

  let packageName = await readRootPackageName();
  let dist = `../../${packageName}/dist/`;

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
        process.stdout.write(`\n❌ ${moduleName}.${fnName}: ${error}\n`);
        failures++;
      }
    }
  }

  console.log(`
${passes + failures} tests, ${failures} failures`);
  process.exit(failures ? 1 : 0);
}

export async function crash(message) {
  throw new Error(message);
}
