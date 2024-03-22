namespace Fable.SimpleJson

open Fable.Parsimmon

#nowarn "40"

module Parser =

    let digits = 
        Parsimmon.digit 
        |> Parsimmon.atLeastOneOrMany
        |> Parsimmon.concat

    let jint = digits |> Parsimmon.map float

    let negJint = 
        Parsimmon.seq2 (Parsimmon.str "-") jint 
        |> Parsimmon.map (fun (sign, number) -> -number)

    let jfloat =
        let floatWithComma =
            Parsimmon.regex "-?(0|[1-9][0-9]*)?[.][0-9]+([eE][+-]?[0-9]+)?"
        let floatWithoutComma =
            Parsimmon.regex "-?[1-9][0-9]*[eE][+-]?[0-9]+"

        [ floatWithComma; floatWithoutComma ]
        |> Parsimmon.choose
        |> Parsimmon.map float

    let jnumber = 
        [jfloat; jint; negJint] 
        |> Parsimmon.choose
        |> Parsimmon.map JNumber

    let jbool = 
        [ Parsimmon.stringReturn "true" (JBool true)
          Parsimmon.stringReturn "false" (JBool false)]
        |> Parsimmon.choose

    let jnull = Parsimmon.stringReturn "null" JNull

    let stringLiteral =
        let escape =  
            Parsimmon.oneOf "\"\\/bfnrt"
            |> Parsimmon.map(function
                | "b" -> "\b"
                | "f" -> "\u000C"
                | "n" -> "\n"
                | "r" -> "\r"
                | "t" -> "\t"
                | c   -> c) // every other char is mapped to itself

        let escapedCharSnippet = 
            Parsimmon.seq2 (Parsimmon.str "\\") escape
            |> Parsimmon.map snd

        let normalCharSnippet = Parsimmon.satisfy (fun c -> c <> "\"" && c <> "\\")

        let anyCharSnippet = 
            normalCharSnippet
            |> Parsimmon.orTry escapedCharSnippet
            |> Parsimmon.many
            |> Parsimmon.concat

        anyCharSnippet
        |> Parsimmon.between (Parsimmon.str "\"") (Parsimmon.str "\"")
        
    let jstring = stringLiteral.map JString

    let withWhitespace p = 
        Parsimmon.between (Parsimmon.optionalWhitespace) (Parsimmon.optionalWhitespace) p
        
    let jvalue = 
        [ jnull; jbool; jnumber; jstring ]
        |> List.map withWhitespace
        |> Parsimmon.choose
    
    let comma = withWhitespace (Parsimmon.str ",")
 
    let rec json = Parsimmon.ofLazy <| fun () ->
        
        let leftBracket = withWhitespace (Parsimmon.str "[")
        let rightBracket = withWhitespace (Parsimmon.str "]")
                    
        let arrayValue = Parsimmon.seperateBy comma json
        
        let jarray = 
            arrayValue
            |> Parsimmon.between leftBracket rightBracket
            |> Parsimmon.map (List.ofArray >> JArray)

        let leftBrace = withWhitespace (Parsimmon.str "{")
        let rightBrace = withWhitespace (Parsimmon.str "}")
            
        let keyValues = 
            Parsimmon.seq3 
                (withWhitespace stringLiteral)
                (withWhitespace (Parsimmon.str ":"))
                (withWhitespace json)
            |> Parsimmon.map (fun (key, _ , value) -> key,value)
            |> Parsimmon.seperateBy comma

        let jobject = 
            keyValues
            |> Parsimmon.between leftBrace rightBrace
            |> Parsimmon.map (List.ofArray >> Map.ofList >> JObject)

        [jvalue; jarray; jobject]
        |> Parsimmon.choose
        
    let jsonParser = withWhitespace json
