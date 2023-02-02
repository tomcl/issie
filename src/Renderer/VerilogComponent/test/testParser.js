import * as verilogGrammar from "../VerilogGrammar.js";
import nearley from "nearley";
import { parseFromFile, fix } from "../parser.js";

console.log("Starting parser tests");

import * as fs from 'fs';
import * as path from 'path';


function readFile(relPath) {
    return fs.readFileSync(relPath, { encoding: 'utf8' }); // zzzz....
}


const inputDir = './input/valid';
const dir = fs.opendirSync(inputDir);
let dirent;
while ((dirent = dir.readSync()) !== null) {
    const verilogInput = readFile(path.join(inputDir, dirent.name));
    console.log(dirent.name);
    const parse = parseFromFile(verilogInput);
    const filename = path.join('./output/valid/', path.basename(dirent.name));
    //console.log(parse);
    const ast = JSON.parse(JSON.parse(parse)["Result"]);
    fs.writeFileSync(filename, JSON.stringify(ast, (key, value) => {if (value !== null) return value; }, "\t"));
}
dir.closeSync()

function runParser(inputFile, outputFile) {
    const verilogInput = readFile(inputFile);
    console.log(path.basename(inputFile));
    const parse = parseFromFile(verilogInput);
    const filename = outputFile;
    //console.log(parse);
    const ast = JSON.parse(JSON.parse(parse)["Result"]);
    fs.writeFileSync(filename, JSON.stringify(ast, (key, value) => {if (value !== null) return value; }, "\t"));
}