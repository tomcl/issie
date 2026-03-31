import fs from "fs";
import path from "path";
import { pathToFileURL, fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// replaced dynamic ESM import with createRequire to load CommonJS parser.js
const { createRequire } = await import('module');
const require = createRequire(import.meta.url);
const parserPath = path.resolve(__dirname, "parser.js");
const parser = require(parserPath);
const parseFromFile = parser.parseFromFile;
const fix = parser.fix ?? ((x) => x);
