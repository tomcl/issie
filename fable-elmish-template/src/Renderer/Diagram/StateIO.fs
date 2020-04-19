module StateIO

open Fable.SimpleJson

type RecordType = {
    stringMember: string
    intMember: int
}

let data: RecordType = { stringMember = "The string"; intMember = 123 }

// serialize record into JSON
let json = SimpleJson.fromObjectLiteral data
printfn "%A" json
