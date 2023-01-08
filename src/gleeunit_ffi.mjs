import * as NodeProcess from "node:process";
import * as Gleam from "./gleam.mjs";

// async function* gleamFiles(directory) {
//   for (let entry of await read_dir(directory)) {
//     let path = join_path(directory, entry);
//     if (path.endsWith(".gleam")) {
//       yield path;
//     } else {
//       try {
//         yield* gleamFiles(path);
//       } catch (error) {
//         // Could not read directory, assume it's a file
//       }
//     }
//   }
// }

async function readRootPackageName() {
  let toml = await read_file("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main(test_modules, halts_on_error) {
  let passes = 0;
  let failures = 0;
  const failureMsgs = [];

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  for await (let path of test_modules) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    let module = await import(join_path(dist, js_path));
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) {
        continue;
      }
      try {
        await module[fnName]();
        write(`\u001b[32m.\u001b[0m`);
        passes++;
      } catch (error) {
        write(`\u001b[31mF\u001b[0m`);
        let moduleName = js_path.slice(0, -4);
        let line = error.line ? `:${error.line}` : "";
        failures++;
        failureMsgs.push(`❌ ${failures}) ${moduleName}.${fnName}${line}\n${error}\n`);
      }
    }
  }

	write("\n");
  let result_info = `${passes + failures} tests, ${failures} failures`;
  if (failures == 0) {
    result_info = `\u001b[32m${result_info}\u001b[0m`;
  } else {
		write("Failures:\n\n");
		write(failureMsgs.join("\n") + "\n");
    result_info = `\u001b[31m${result_info}\u001b[0m`;
  }
  write(result_info + "\n");

  if (halts_on_error) {
    exit(failures ? 1 : 0);
  } else {
    exit(0);
  }
}

export function crash(message) {
  throw new Error(message);
}

function write(message) {
  if (globalThis.Deno) {
    Deno.stdout.writeSync(new TextEncoder().encode(message));
  } else {
    process.stdout.write(message);
  }
}

function exit(code) {
  if (globalThis.Deno) {
    Deno.exit(code);
  } else {
    process.exit(code);
  }
}

// async function read_dir(path) {
//   if (globalThis.Deno) {
//     let items = [];
//     for await (let item of Deno.readDir(path, { withFileTypes: true })) {
//       items.push(item.name);
//     }
//     return items;
//   } else {
//     let { readdir } = await import("fs/promises");
//     return readdir(path);
//   }
// }

function join_path(a, b) {
  if (a.endsWith("/")) return a + b;
  return a + "/" + b;
}

async function read_file(path) {
  if (globalThis.Deno) {
    return Deno.readTextFile(path);
  } else {
    let { readFile } = await import("fs/promises");
    let contents = await readFile(path);
    return contents.toString();
  }
}

export const file_exists = function (absolute_file_name) {
  if (fs.existsSync(absolute_file_name)) {
    return true;
  }
  return false;
};

export const cwd = function() {
	return NodeProcess.cwd();
};


export const find_ext_files_recursive_in = function (file_ext, directory) {
  // TODO: use gleamFiles() and read_dir() instead.
  const path = require("path");
	let files = [];
  const getFilesRecursively = (directory) => {
    const filesInDirectory = fs.readdirSync(directory);
    for (const file of filesInDirectory) {
      const absolute = path.join(directory, file);
      if (fs.statSync(absolute).isDirectory()) {
        getFilesRecursively(absolute);
      } else if (absolute.endsWith(file_ext)) {
        files.push(absolute);
      }
    }
  };
  getFilesRecursively(directory);

  return Gleam.List.fromArray(files);
};
