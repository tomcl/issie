// Command-line access to the Verilog input parser, for tests and quick checks:
//
//   node run_parser.mjs <file.v>
//
// Prints one JSON object to stdout:
//   {"Ok": <fixed AST matching VerilogTypes.VerilogInput>, "NewLinesIndex": [...]}
// on success, or
//   {"Err": <the parser's error object>}
// on a syntax error. Exit code is 0 either way; non-zero means the runner itself failed.
//
// This is the same parse used by the app (parser.js -> nearley grammar -> fix), so a file
// that parses here parses in the editor. Used by Tests/Issie.Tests/VerilogCompiler.fs to
// drive the F# semantic checks and SheetCreator under .NET.
import fs from "fs";
import path from "path";
import { createRequire } from "module";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);

// parser.js logs token diagnostics on every parse error; keep stdout clean for the JSON
const realLog = console.log;
console.log = () => {};
const parser = require(path.resolve(__dirname, "parser.js"));

const source = fs.readFileSync(process.argv[2], "utf8").toString();
const result = JSON.parse(parser.parseFromFile(source));
console.log = realLog;

if (result.Result && !result.Error) {
  const fixed = parser.fix(result.Result);
  console.log(JSON.stringify({ Ok: JSON.parse(fixed), NewLinesIndex: result.NewLinesIndex }));
} else {
  console.log(JSON.stringify({ Err: JSON.parse(result.Error) }));
}
