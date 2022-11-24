#!/usr/bin/env node
let readline = require("readline");

let main = async () => {
  let canvasState = "";

  let rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  });

  for await (const line of rl) {
    canvasState += line;
  }
  canvasState = canvasState.replaceAll(/\n/g, "");
  canvasState = canvasState.replaceAll(/undefined/g, "None");
  let uuid = /([0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12})/g;
  canvasState = canvasState.replaceAll(uuid, '"$1"');
  let tag =
    /(Type|Label|InputPorts|PortNumber|PortType|HostId|OutputPorts|X =|H = |W = |Y =|SymbolInfo|LabelRotation|LabelBoundingBox|STransform|flipped|ReversedInputPorts|PortOrientation|PortOrder|HScale|VScale|Source|Target|Vertices)/g;
  canvasState = canvasState.replaceAll(tag, " ; $1");
  canvasState = canvasState.replaceAll(
    /Label = ([0-9a-zA-Z\-]*)/g,
    'Label = "$1"'
  );

  canvasState = canvasState.replaceAll(
    /PortType = (Input|Output)/g,
    "PortType = PortType.$1"
  );
  canvasState = canvasState.replaceAll(
    /ReversedInputPorts = (\w+)/g,
    "ReversedInputPorts = Some $1"
  );
  // add Some to PortNumber
  canvasState = canvasState.replaceAll(
    /PortNumber = (\d+)/g,
    "PortNumber = Some $1"
  );
  // map to Map
  canvasState = canvasState.replaceAll(/map \[\(/g, "Map [(");
  // remove double space
  canvasState = canvasState.replaceAll(/\s\s/g, " ");
  canvasState = canvasState.replaceAll(/{\s*;/g, "{");
  canvasState = canvasState.replaceAll(/\s*;/g, ";");
  // remove SymbolInfo
  canvasState = canvasState.replaceAll(
    /SymbolInfo = ({ .*? } })/g,
    "SymbolInfo = Some $1"
  );

  console.log(canvasState);
};

main();
