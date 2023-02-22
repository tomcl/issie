#!/usr/bin/env node
import * as t from "npm:io-ts"
import Ajv from "npm:ajv"
import { PathReporter } from "npm:/io-ts/lib/PathReporter.js"
import { isLeft } from "npm:/fp-ts/lib/Either.js"

import { SavedInfo } from "./CommonTypes.ts"

import { existsSync, readdirSync, readFileSync } from "node:fs"
import { join } from "node:path"
import { fromPredicate } from "../../.cache/deno/npm/registry.npmjs.org/fp-ts/2.13.1/lib/FromEither.d.ts"

const folderPath = Deno.args[0]

if (!existsSync(folderPath)) Deno.exit(1)
const files = readdirSync(folderPath)
// console.debug(files);

const filePath = join(folderPath, files[0])
if (!existsSync(filePath)) Deno.exit(1)
const contents = readFileSync(filePath, "utf8")
// console.debug(contents);

const parsedJson: unknown = JSON.parse(contents)
console.debug("parsedJson", parsedJson)

const decoded = SavedInfo.decode(parsedJson)

if (isLeft(decoded)) {
  throw Error(`Could not validate data: ${PathReporter.report(decoded).join("\n")}`)
  // e.g.: Could not validate data: Invalid value "foo" supplied to : { userId: number, name: string }/userId: number
}

type SavedInfoT = t.TypeOf<typeof SavedInfo> // compile-time type
const decodedSavedInfo: SavedInfoT = decoded.right // now safely the correct type

console.log("decoded user id:", decodedSavedInfo)
