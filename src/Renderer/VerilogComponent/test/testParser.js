import * as verilogGrammar from "../VerilogGrammar.js";
import nearley from "nearley";
import { parseFromFile, fix } from "../parser.js";

console.log("Starting parser tests");

import * as fs from 'fs';
import * as path from 'path';


function readFile(relPath) {
    return fs.readFileSync(relPath, { encoding: 'utf8' }); // zzzz....
}


function getLineContext(source, lineNum, colNum) {
    const lines = source.split('\n');
    const lineIndex = lineNum - 1;
    if (lineIndex < 0 || lineIndex >= lines.length) return '';
    
    const line = lines[lineIndex];
    const pointer = ' '.repeat(Math.max(0, colNum - 1)) + '^';
    return `    ${lineNum} | ${line}\n      | ${pointer}`;
}

function formatParseError(filename, source, parseResult) {
    if (!parseResult["Error"]) {
        console.log(`  [FAIL] ${filename}: Unknown error`);
        return;
    }
    
    try {
        const errorObj = JSON.parse(parseResult["Error"]);
        const line = errorObj.Line || '?';
        const col = errorObj.Col || '?';
        const message = errorObj.Message || 'Unknown parse error';
        
        console.log(`  [FAIL] ${filename}`);
        console.log(`    Location: Line ${line}, Column ${col}`);
        console.log(`    ${message}`);
        console.log(getLineContext(source, line, col));
    } catch (e) {
        console.log(`  [FAIL] ${filename}: ${parseResult["Error"]}`);
    }
}

const inputDir = './input/valid';
const dir = fs.opendirSync(inputDir);
let dirent;
let passed = 0, failed = 0;
while ((dirent = dir.readSync()) !== null) {
    const filePath = path.join(inputDir, dirent.name);
    const verilogInput = readFile(filePath);
    const parse = parseFromFile(verilogInput);
    const filename = path.join('./output/valid/', path.basename(dirent.name));
    const parseResult = JSON.parse(parse);
    if (parseResult["Result"]) {
        const ast = JSON.parse(parseResult["Result"]);
        fs.writeFileSync(filename, JSON.stringify(ast, (key, value) => {if (value !== null) return value; }, "\t"));
        console.log(`  [OK] ${dirent.name}`);
        passed++;
    } else {
        formatParseError(dirent.name, verilogInput, parseResult);
        failed++;
    }
}
dir.closeSync();
console.log(`\nResults: ${passed} passed, ${failed} failed`);

function runParser(inputFile, outputFile) {
    const verilogInput = readFile(inputFile);
    const basename = path.basename(inputFile);
    const parse = parseFromFile(verilogInput);
    const filename = outputFile;
    const parseResult = JSON.parse(parse);
    if (parseResult["Result"]) {
        const ast = JSON.parse(parseResult["Result"]);
        fs.writeFileSync(filename, JSON.stringify(ast, (key, value) => {if (value !== null) return value; }, "\t"));
        console.log(`  [OK] ${basename}`);
        return true;
    } else {
        formatParseError(basename, verilogInput, parseResult);
        return false;
    }
}