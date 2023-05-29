// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

import fs from "fs";
import path from "path";
import { dotnet } from "./dotnet.js";

const projectPath = process.argv[2];

console.log("project path: ", projectPath);

const { setModuleImports, getAssemblyExports, getConfig } = await dotnet
  .withDiagnosticTracing(false)
  .create();

setModuleImports("main.mjs", {
  fs: {
    readFileSync: (path) => fs.readFileSync(path, "utf8"),

    listJSONFilesSync: (folderPath) => fs.readdirSync(folderPath, "utf8")
      .map((filePath) => path.join(folderPath, filePath))
      .filter(filePath => fs.lstatSync(filePath).isFile())
      .filter(filePath => filePath.endsWith(".dgmNew")),

    listRAMFilesSync: (folderPath) => fs.readdirSync(folderPath, "utf8")
      .map((filePath) => path.join(folderPath, filePath))
      .filter(filePath => fs.lstatSync(filePath).isFile())
      .filter(filePath => filePath.endsWith(".ram")),

    readJSONFilesSync: (folderPath) => fs.readdirSync(folderPath, "utf8")
      .map((filePath) => path.join(folderPath, filePath))
      .filter(filePath => fs.lstatSync(filePath).isFile())
      .filter(filePath => filePath.endsWith(".dgmNew"))
      .map(filePath => fs.readFileSync(filePath, "utf8")),

    readRAMFilesSync: (folderPath) => fs.readdirSync(folderPath, "utf8")
      .map((filePath) => path.join(folderPath, filePath))
      .filter(filePath => fs.lstatSync(filePath).isFile())
      .filter(filePath => filePath.endsWith(".ram"))
      .map(filePath => fs.readFileSync(filePath, "utf8")),
  },
});

const config = getConfig();
const exports = await getAssemblyExports(config.mainAssemblyName);

exports.MyClass.Benchmark(projectPath);

await dotnet.run();