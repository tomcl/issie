#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";

const json2Components = (json) => {
  let result = json;
  // remove all quotes except for uuid
  result = result.replaceAll(/"([\d\w-]{1,35})"/g, "$1");
  result = result.replaceAll(/,/g, ";");
  result = result.replaceAll(/:/g, " = ");
  // null -> None
  result = result.replaceAll(/null/g, "None");

  // wrap into Some
  result = result.replaceAll(
    /{Input1 = \s*\[(\d+);\s*(\d+)\]}/g,
    "Input1($1, Some $2)"
  );
  result = result.replaceAll(
    /{Constant1 = \s*\[(\d+);\s*(\d+);\s*(\d+)\]}/g,
    'Constant1($1, $2, "$3")'
  );
  result = result.replaceAll(/SymbolInfo = ({.*?}})/gm, "SymbolInfo = Some $1");
  result = result.replaceAll(
    /ReversedInputPorts = (\w+)/g,
    "ReversedInputPorts = Some $1"
  );
  result = result.replaceAll(/PortNumber = (\d+)/g, "PortNumber = Some $1");

  // convert JavaScript map to .NET map
  result = result.replaceAll(
    /PortOrientation = {(.*?)}/g,
    "PortOrientation = Map [ $1 ]"
  );
  result = result.replaceAll(/PortOrder = {(.*?)}/g, "PortOrder = Map [ $1 ]");

  result = result.replaceAll(
    /PortType = (Input|Output)/g,
    "PortType = PortType.$1"
  );

  result = result.replaceAll(/Label = (\w+)/g, 'Label = "$1"');
  result = result.replaceAll(/{(\w+) = (\d+)}/g, "$1 ($2)");

  result = result.replaceAll(
    /("[\d\w-]{36}") = (Left|Right|Top|Bottom)/g,
    "($1, $2)"
  );
  result = result.replaceAll(
    /(Left|Right|Top|Bottom) = (\[("[\d\w-]{36}";?\s*)*\])/g,
    "($1, $2)"
  );

  return result;
};

const json2Connections = (json) => {
  let result = json;
  // remove all quotes except for uuid
  result = result.replaceAll(/"([\d\w-]{1,35})"/g, "$1");
  result = result.replaceAll(/,/g, ";");
  result = result.replaceAll(/:/g, " = ");
  // null -> None
  result = result.replaceAll(/null/g, "None");

  // wrap into Some
  result = result.replaceAll(/PortNumber = (\d+)/g, "PortNumber = Some $1");

  result = result.replaceAll(
    /PortType = (Input|Output)/g,
    "PortType = PortType.$1"
  );

  // convert to Tuple
  result = result.replaceAll(
    /\[(\d+.\d+);(\d+.\d+);(false|true)\];?/g,
    "$1, $2, $3;"
  );

  return result;
};

const json2CanvasState = (json) => {
  const parsedData =
    JSON.parse(json).NewCanvasWithFileWaveSheetInfoAndNewConns[0];

  // extract CanvasState from SavedInfo
  const components = json2Components(JSON.stringify(parsedData[0]));
  const connections = json2Connections(JSON.stringify(parsedData[1]));

  return `(${components},${connections})`;
};

const main = async () => {
  const filePath = process.argv[2];
  const baseName = path.basename(filePath, path.extname(filePath));
  const content = fs.readFileSync(filePath, "utf8");
  const canvasState = json2CanvasState(content);
  console.log(`(${canvasState}, "${baseName}")`);
};

main();
