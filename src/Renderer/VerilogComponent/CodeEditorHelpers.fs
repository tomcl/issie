module CodeEditorHelpers

open VerilogTypes
open ErrorCheck
open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Fulma
open System


type PrismCore =
    abstract highlight : string * obj -> string

[<ImportAll("./prism.js")>]
let Prism: PrismCore = jsNative

[<Emit("Prism.languages.verilog")>]
let language : obj = jsNative


[<Emit("clipboard.writeText($0,'selection')")>]
let copyToClipboard (text:string) : unit = jsNative

[<Emit("import {clipboard} from \"electron\"")>]
let importClipboard : unit = jsNative

importClipboard







/// Returns the overlay which contains all the errors    
let getErrorDiv errorList : ReactElement =
    
    
    let getUnderLineElement marginLeft _line message = 
        [
            span [Style [Display DisplayOptions.InlineBlock; MarginLeft marginLeft; PointerEvents "stroke"]] []
            span [Class "error"; Style [PointerEvents "auto"; FontSize 16; Color "rgb(255,0,0)"; Background "rgba(255,0,0,0)"]] [str (_line)] 
            span [Class "hide"] [str message]                                //204 
        ]


    /// Given a list of errors on a specific line, returns a react element with the correct underlines and on-hover messages 
    let getErrorLine errorLineList =
        let sortedErrors = List.sortBy (fun e -> e.Col) errorLineList
        let linechildren = 
            sortedErrors
            |> List.indexed
            |> List.collect (fun (index,err) ->
                let prevErrorEnd = if index = 0 then 0.0 else (float (sortedErrors[index-1].Col+sortedErrors[index-1].Length-1))*8.8
                let spaces = sprintf "%fpx" ((float (err.Col-1))*8.8 - prevErrorEnd)
                let _line = ("", [1..err.Length]) ||> List.fold (fun s v -> s+"-")
                getUnderLineElement spaces _line err.Message
            )
        
        [p [] linechildren]

    /// Returns a map which maps line number to list of errors (type ErrorInfo) on that line
    let getLineToErrorsMap sortedErrorList = 
        
        let emptyMap = Map.empty<int,ErrorInfo list>
        
        (emptyMap, sortedErrorList)
        ||> List.fold (fun state err ->
                match Map.tryFind err.Line state with
                | Some found -> Map.add err.Line (List.append found [err]) state
                | None -> Map.add err.Line [err] state
            )
    
    
    
    let sortedByLineErrorList = List.sortBy (fun err -> err.Line) errorList
    
    let lineToErrorsMap = getLineToErrorsMap sortedByLineErrorList
    
    let childrenElements =
        match List.tryLast sortedByLineErrorList with
        | Some lastError ->
            [1..lastError.Line]
            |> List.collect (fun line ->
                match Map.tryFind line lineToErrorsMap with
                | Some errors -> getErrorLine errors
                | None -> [br []]
                )
        | None -> []

    div [
        Style [Position PositionOptions.Absolute ; 
            Display DisplayOptions.Block; 
            Width "100%"; Height "100%"; 
            CSSProp.Top "8px"; CSSProp.Left "0"; CSSProp.Right "0"; CSSProp.Bottom "0";
            BackgroundColor "rgba(0,0,0,0)";
            FontWeight "bold";
            Color "Red"; 
            ZIndex "2" ;
            PointerEvents "none";
            WhiteSpace WhiteSpaceOptions.PreLine]
    ] childrenElements


///////////

let getSyntaxErrorInfo error = 
    if (String.exists (fun ch -> ch = ';') error.Message && not (String.exists (fun ch->ch='.') error.Message))
        then {error with ExtraErrors = Some [|{Text= "Your previous line is not terminated with a semicolon (;)"; Copy= false}|]}
    elif (String.exists (fun ch -> ch = '\'') error.Message)
        then {error with ExtraErrors = Some [|{Text= "Numbers must be of format: <size>'<radix><value>\n  e.g. 16'h3fa5;"; Copy= false}|]}
    else {error with ExtraErrors = Some [|{Text= error.Message; Copy= false}|]}



//////////



       

let getErrorTable (errorList: ErrorInfo list) =
    

    let getSuggestionLine suggestions = 
        let buttons = 
            suggestions
            |> Seq.toList
            |> List.collect (fun suggestion ->
                [
                    span [Style [WhiteSpace WhiteSpaceOptions.Pre]] [str "    "]
                    Button.button [
                        Button.OnClick (fun _ -> 
                            copyToClipboard suggestion)
                        Button.Option.Size ISize.IsSmall
                        ] [str suggestion]
                ]
            )

        let line = 
            List.append
                [span [Style [Color "Red"; FontStyle "Italic"; VerticalAlign "Middle";]] [str "\tDo you mean:"]]
                buttons
        
        td [Style [WhiteSpace WhiteSpaceOptions.Pre ]] line

        
        

    
    let getErrorTableLine index (extraMessage:ExtraErrorInfo) line : ReactElement list =
        let copyable = extraMessage.Copy
        let text = extraMessage.Text
        let showLine = if index=0 then "  Line "+(string line) else ""
        if copyable then
            let suggestions = text.Split([|"|"|],StringSplitOptions.RemoveEmptyEntries)
            [
                tr [] [
                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str showLine]
                    getSuggestionLine suggestions
                ]
            ]
        else
            [
                tr [] [
                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str showLine]
                    td [Style [Color "Black"; WhiteSpace WhiteSpaceOptions.PreWrap]] [str text]
                ]
            ]

    let getErrorTableLines error = 
        let line = error.Line   
        // let message = 
        match isNullOrUndefined error.ExtraErrors with
        |true -> null
        |false ->
            let tLine = 
                (Option.get error.ExtraErrors)
                |> Array.toList
                |> List.indexed
                |> List.collect (fun (index,mess) -> getErrorTableLine index mess line)
            
            tbody [] tLine
    
    
    
    let tableFormat =
        [
        colgroup [] [
            col [Style [Width "20%";]]
            col [Style [Width "80%"; WhiteSpace WhiteSpaceOptions.PreLine]]
            ]
        thead [] [
            tr [] [
                th [Style [WhiteSpace WhiteSpaceOptions.Pre]] [str "  Line"]
                th [] [str "Message"]
            ]
        ]
        ]

    
    let tableLines =
        errorList
        |> List.sortBy (fun err -> err.Line)
        |> List.collect (fun err -> [getErrorTableLines err])
    
    let tableChildren = List.append tableFormat tableLines
    if List.length tableLines <> 0 then 
        table 
            [Style 
                [ 
                FontSize "16px"; 
                TableLayout "Fixed"; 
                Width "100%";
                BorderRight "groove";
                BorderLeft "groove"]]
            tableChildren
    else
        table [] []



let getLineCounterDiv linesNo =
    let childrenElements=
        [1..linesNo+1]
        |> List.collect (fun no ->
            [p [] [str (sprintf "%i" no)]]
        )
    
    div [
        Style [Position PositionOptions.Absolute ; 
            Display DisplayOptions.Block; 
            Width "48px"; Height "100%"; 
            CSSProp.Top "0px"; CSSProp.Left "-56px"; CSSProp.Right "0"; CSSProp.Bottom "0";
            BackgroundColor "rgba(0,0,0,0)";
            Color "#7f7f7f"; 
            ZIndex "2" ;
            PointerEvents "none";
            TextAlign TextAlignOptions.Right;
            WhiteSpace WhiteSpaceOptions.PreLine]
    ] childrenElements


let infoHoverableElement = 
    let example =
        "\tTHIS IS AN EXAMPLE OF A VALID VERILOG FILE
----------------------------------------------------------
module decoder(instr,n,z,c,mux1sel,aluF,jump);
\tinput [15:0] instr;
\tinput n,z,c;
\toutput mux1sel,jump;
\toutput [2:0] aluF;
\twire cond = n|z|c;

\tassign mux1sel = instr[15]&cond | instr[14];
\tassign j = instr[8]&(n|c);
\tassign aluF = intr[10:8];
endmodule"

    div [
        Style [Position PositionOptions.Absolute ; 
            Display DisplayOptions.Block; 
            Width "100%"; Height "100%"; 
            CSSProp.Top "-52px"; CSSProp.Left "104px"; CSSProp.Right "0"; CSSProp.Bottom "0";
            BackgroundColor "rgba(0,0,0,0)";
            Color "#7f7f7f"; 
            ZIndex "2" ;
            PointerEvents "none";
            TextDecoration "underline";
            TextDecorationColor "rgb(0,0,255)";
            TextAlign TextAlignOptions.Left;
            WhiteSpace WhiteSpaceOptions.PreWrap]
        ]
        [p [Class "info"] 
        [
            span [Class "error"; Style [PointerEvents "auto"; FontSize 16; Color "rgb(0,0,255)"; Background "rgba(255,0,0,0)"]] [str ("example")] 
            span [Class "hide"] [str example]
        ]
    ]