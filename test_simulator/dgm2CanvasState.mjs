#!/usr/bin/env node

import readline from "node:readline";

const json2fs = (json) => {
  let result = json;
  // remove all quotes except for uuid
  result = result.replaceAll(/"([\d\w-]{1,35})"/g, "$1");
  result = result.replaceAll(/,/g, ";");
  result = result.replaceAll(/:/g, " = ");
  // add explicit list type
  // result = result.replaceAll(/\[\]/g, "list []");
  // null -> None
  result = result.replaceAll(/null/g, "None");

  // wrap into Some
  result = result.replaceAll(
    /{Input1 = \s*\[(\d+);\s*(\d+)\]}/g,
    "Input1($1, Some $2)"
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
  result = result.replaceAll(
    /\[(\d+.\d+);(\d+.\d+);(false|true)\];?/g,
    "$1, $2, $3;"
  );

  return result;
};

const main = async () => {
  let canvasState = "";

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  });

  for await (const line of rl) {
    canvasState += line;
  }

  const parsedData =
    JSON.parse(canvasState).NewCanvasWithFileWaveSheetInfoAndNewConns[0];
  // extract CanvasState from SavedInfo
  const components = json2fs(JSON.stringify(parsedData[0]));
  const connections = json2fs(JSON.stringify(parsedData[1]));

  console.log(`module TestCases

open CommonTypes
open SimulatorTypes

let register: CanvasState = (
${components},
${connections}
)`);
};

main();
