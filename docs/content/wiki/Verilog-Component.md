## Introduction

The creation of Custom Components written in Verilog is a much different process than the one followed for other ISSIE components. The code is designed in a way such that it can be used for other purposes in the future as well, with very few alterations (e.g. an Assembly language to RAM contents editor and compiler).

The creation of the Verilog Component was separated into 5 parts. The most challenging parts and potential improvements will be analyzed for each part. The 5 parts are:


- [Code Editor](#code-editor)
- [Grammar and Parser](#grammar-and-parser)
- [F# representation of the AST](#f-representation-of-the-ast)
- [Error Checking and UI](#error-checking-and-ui)
- [AST to ISSIE Sheet](#ast-to-issie-sheet)


## Code Editor

The first step was setting up a code editor. In order to follow the creation procedure of other components, a `Verilog` category was added in the Catalogue, with elements:
- `New Verilog Component`
- List of already created Verilog components  

When the user clicks on `New Verilog Component`, a `Verilog Popup` appears which has the code editor inside. Above the code editor, the component name input field is located, which in this case is set to `Disabled` and equal to the Verilog module name specified by the user. 

The Code Editor used is [react-simple-code-editor](https://github.com/satya164/react-simple-code-editor), a simple editor which provides only syntax highlighting. In order to bind it with F#, Fable provides some react Helpers and specifically the `Fable.React.Helpers.ofImport` function which allows the instantiation of imported React components, and is used as follows:

```fsharp
let inline codeEditor 
  (props : CodeEditorProps list)
  (elems : ReactElement list)
    : ReactElement =
      ofImport "default" "react-simple-code-editor" (keyValueList CaseRules.LowerFirst props) elems

```

The props need to be defined as an F# Type, matching the names and characteristics of the React element props. 

```fsharp
type CodeEditorProps = 
    | Placeholder of string
    | Value of string
    | OnValueChange of (string -> unit)
    | Highlight of (string -> obj)
    | TabSize of int
    | InsertSpaces of bool
    | IgnoreTabKey of bool
    | Padding of int
    | TextAreaId of string
    | TextAreaClassName of string
    | PreClassName of string
```

So now, by using the `codeEditor` function we can create a code editor react element (similar to creating a `div [] []`). In ISSIE, it is used as follows:

```fsharp
codeEditor 
  [
    CodeEditorProps.Placeholder ("Start Writing your Verilog Code here..."); 
    CodeEditorProps.Value ((sprintf "%s" this.state.code)); 
    OnValueChange (fun txt -> 
        (this.setState (fun s p -> {s with code=txt}))
        props.Dispatch <| SetPopupDialogCode (Some txt)
        props.Compile {props.DialogData with VerilogCode=Some txt}
    )             
    Highlight (fun code -> Prism.highlight(code,language));
  ]
  []
```

### Syntax Highlighting

`react-simple-code-editor` suggests the use of `PrismJS` for Syntax Highlighting, which supports multiple languages, including Verilog. Setting up PrismJS in F# is easy and really simplifies the highlighting task because of the pre-existing Verilog Grammar-tokenizer-highlighter: 

```fsharp
type PrismCore =
    abstract highlight : string * obj -> string

[<ImportAll("../VerilogComponent/prism.js")>]
let Prism: PrismCore = jsNative

[<Emit("Prism.languages.verilog")>]
let language : obj = jsNative
```

### React Stateful Component  

A problem that arises by using the code editor is that `OnValueChange` (i.e. whenever the user changes the code) the whole editor is re-rendered. This means that the cursor is moved to the end of the code. Of course, this is undesirable behavior, because the user cannot properly type his code. 

To solve the aforementioned problem, the code editor is wrapped inside a react stateful component, which has the ability to keep track of its state (cursor position), giving the expected outcome of a normal code editor whenever it is re-rendered.

```fsharp
type CERSCProps =
    { CurrentCode : string
      ReplaceCode : string Option
      Dispatch : (Msg -> unit)
      DialogData: PopupDialogData
      Compile: (PopupDialogData -> Unit)}
type CERSCState = { code: string; }

type CodeEditorReactStatefulComponent (props) =
    inherit Component<CERSCProps, CERSCState> (props)
    
    do base.setInitState({ code = "module NAME();\n  // Write your IO Port Declarations here\n  \n  \n  \n  // Write your Assignments here\n  \n  \n  \nendmodule" })

    override this.componentDidUpdate (prevProps,prevState) =
        match (props.ReplaceCode <> None && prevProps.ReplaceCode = None) with
        |true -> 
            this.setState(fun s _-> {s with code = Option.get props.ReplaceCode} )
            props.Dispatch <| SetPopupDialogCode (props.ReplaceCode)
            props.Compile {props.DialogData with VerilogCode=props.ReplaceCode}
        |false -> ()

    override this.render () =
            codeEditor [
                    CodeEditorProps.Placeholder ("Start Writing your Verilog Code here..."); 
                    CodeEditorProps.Value ((sprintf "%s" this.state.code)); 
                    OnValueChange (fun txt -> 
                        (this.setState (fun s p -> {s with code=txt}))
                        props.Dispatch <| SetPopupDialogCode (Some txt)
                        props.Compile {props.DialogData with VerilogCode=Some txt}
                    )             
                    Highlight (fun code -> Prism.highlight(code,language));]
                    []
```

## Grammar, Parser and Lexer

Once the code is complete, it needs to be represented in a format that F# understands. This is done by parsing the input string to generate the Abstract Syntax Tree (AST). The parsing is done using the `NearleyJS` parser which turns the input string into a JSON object.

NearleyJS is very easy to use as the user only needs to specify the grammar rules for his input. The process is as follows:
- Write grammar rules in a `.ne` file
  - Example:
    ```javascript
    MODULE_ITEM
    -> INPUT_DECL _ {%function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}
    | OUTPUT_DECL _ {%function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}
    | PARAMETER_DECL _ {%function(d,l, reject) {return {Type: "item", ItemType: "parameter_decl", IODecl: null, ParamDecl: d[0], Statement: null, Location: l};} %}
    | STATEMENT _ {%function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, ParamDecl: null, Statement: d[0], Location: l};} %}
    ```
  - For full grammar see [verilogGrammar.ne](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/VerilogGrammar.ne)
- Compile the `.ne` file to a `.js` file using the Nearley Compiler
  - Run: "npx nearleyc VerilogGrammar.ne -o VerilogGrammar.js"
- Parse the string using the `parse` function provided by Nearley specifying the grammar rules `.js` file
- The `parse` function returns a `JSON object` as defined in the `.ne` file

For good syntax errors (and better performance) Moo, the lexer supported and recommended by Nearley is used. Read about Moo on the [Nearley website](https://nearley.js.org/docs/tokenizers) and the [Moo repository](https://github.com/no-context/moo). The lexer is also defined in [verilogGrammar.ne](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/VerilogGrammar.ne).

### Grammar and parser gotchas:
- The order the tokens are defined in the lexer matters! Moo will always return the first match, so the order of the tokens determines the priority of the tokens.
 - For example the following lexers:
   ```javascript
   const lexer = moo.compile({
     lt: '<',
     sll: '<<'})
   ```
   for the input string '<<' returns two `lt` tokens instead of a single `sll`. This is why the multiple character operators are above the single character ones.
- Keywords should be defined as Moo keywords, and then the Verilog keywords won't be parsed as identifiers
- In the parser to get the string value of a token, use `<token>.value`, to get the location from the start of the input, use `<token>.offset`
- Do not use the standard Nearley 3 parameter postprocessor function, as the location parameter will only give the offset in the stream of tokens and not the stream of characters, so always use the above mentioned offset field in the tokens, then pass these up in the AST as necessary.
- Check the grammar for ambiguity! The _ and __ optional and obligatory whitespace characters can cause ambiguity when they are placed at the end of nested structures. For example:
  ```js
  ALWAYS_CONSTRUCT -> %always STATEMENT _ 
  STATEMENT -> BLOCKINGASSIGNMENT %semicolon _ 
  ```
  will cause ambiguities when there are multiple whitespace characters at the end of a statement. Make sure there is always a single parse for the inputs. If there are multiple parses, some of them might be incorrect. If the whitespace ambiguity is present in a lot of the production rules, there can be so many parses that the entire app crashes, so keep an eye out for this.


## F# representation of the AST

After parsing the code, we have a JSON object (JSON = JavaScript Object Notation). However, in order to use the JSON in F#, we need to transform it into an F# record. Before that, the record needs to be defined explicitly.

Following the format of the JSON-AST produced by Nearley, the result is of `type VerilogInput` where:

```fsharp
//////////////////////// Verilog Input Record   ///////////////////////////
type IdentifierT = {Name: string; Location: int}

type ModuleNameT = {Type : string; Name : IdentifierT}

type NumberT = {Type: string; NumberType: string; Bits: string option; Base: string option; UnsignedNumber: string option; AllNumber: string option; Location: int }

type RangeT = {Type: string; Start: string; End: string; Location: int}

type IOItemT = {Type: string; DeclarationType: string; Range : RangeT option; Variables: IdentifierT array; Location: int}

type ParameterT = {Type: string; Identifier: IdentifierT; RHS: NumberT}
type ParameterItemT = {Type: string; DeclarationType: string; Parameter : ParameterT;}

type PrimaryT = {Type: string; PrimaryType: string; BitsStart: string option; BitsEnd: string option; Primary: IdentifierT}

type ExpressionT = {Type: string; Operator: string option; Head: ExpressionT option; Tail: ExpressionT option; Unary: UnaryT option}
    and UnaryT = {Type: string; Primary: PrimaryT option; Number: NumberT option; Expression: ExpressionT option}

type AssignmentLHST = {Type: string; PrimaryType: string; BitsStart: string option; BitsEnd: string option; Primary: IdentifierT; VariableBitSelect: ExpressionT option}
type AssignmentT = {Type: string; LHS: AssignmentLHST; RHS: ExpressionT}

type ContinuousAssignT = {Type: string; StatementType: string; Assignment : AssignmentT; Location: int} // need to add seq block, option statement array

type DeclarationT = {Type: string; DeclarationType: string; Range: RangeT option; Variables: IdentifierT array; Location: int;}

type NonBlockingAssignT = {Assignment: AssignmentT}

type BlockingAssignT = {Operator: string; Assignment: AssignmentT}

type SeqBlockT = {Type: string; Statements: StatementT array; Location: int}

and StatementT = {Type: string; StatementType: string; NonBlockingAssign: NonBlockingAssignT option; BlockingAssign: BlockingAssignT option; SeqBlock: SeqBlockT option; Conditional: ConditionalT option; CaseStatement: CaseStatementT option; Location: int}

and IfStatementT = {Type: string; Condition: ExpressionT; Statement: StatementT; Location: int}

and ConditionalT = {Type: string; IfStatement: IfStatementT; ElseStatement: StatementT option; Location: int} 

and CaseItemT = {Type: string; Expressions: NumberT array; Statement: StatementT}

and CaseStatementT = {Type: string; Expression: ExpressionT; CaseItems: CaseItemT array; Default: StatementT option; Location: int}

type AlwaysConstructT = {Type: string; AlwaysType: string; Statement: StatementT; ClkLoc: int; Location: int}

type NamedPortConnectionT = {Type: string; PortId: IdentifierT; Primary: PrimaryT}

type ModuleInstantiationT = {Type: string; Module: IdentifierT; Identifier: IdentifierT; Connections: NamedPortConnectionT array}

type ItemT = {Type: string; ItemType: string; IODecl: IOItemT option; Decl: DeclarationT option; ParamDecl: ParameterItemT option; Statement: ContinuousAssignT option; AlwaysConstruct: AlwaysConstructT option; ModuleInstantiation: ModuleInstantiationT option; Location: int}

type ModuleItemsT = {Type : string; ItemList : ItemT array}

type ModuleT = {Type : string; ModuleName : IdentifierT; PortList : string array; Locations: string array; ModuleItems : ModuleItemsT; EndLocation: int;}

type VerilogInput = { Type:string; Module: ModuleT; }
```

For the conversion from JSON to F# Record, the library `Fable.SimpleJson` provided by `Fable` will be used. `Fable.SimpleJson` can automatically parse a JSON object to an F# Record (given that the record matches the JSON object keys). In other words, with a single line of code (see below) the original Verilog Code in now in a format F# can understand and manipulate. 
```fsharp
let parsedAST = nearleyResult |> Json.parseAs<VerilogInput>
```

Note that the AST records contain many optional fields. For example to represent different types of module items, every possibility has its own optional field. This logic could be better represented as discriminated unions. However, the SimpleJson library does not support this out of the box. To get the AST into a DU structure in the future (as this would make the compiler more maintainable), one could try to write a special convert function, or look into using different libraries (at the time of writing this doc, there are no libraries that support this). Some solutions are mentioned [here](https://stackoverflow.com/questions/58748946/deserialization-in-f-vs-c-sharp)

## Interacting with the AST
As the AST contains recursive structures, such as the various statements, a general `ASTNode` DU is added, so that recursive functions can easily iterate through the entire AST. The ASTNode definition can be seen in [VerilogAST.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/VerilogAST.fs)

As iterating through the AST has to be done in multiple places both in error handling and sheet creation, a higher-order `foldAST` function was written. This function takes in a folder function, a state of any type and an ASTNode and executes the given function on each node of the AST. The `foldAST` function can be called on any ASTNode. For the definition of `foldAST` see [VerilogAST.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/VerilogAST.fs).

Example usage of `foldAST`:
```fsharp
let getExpressions expressions node =
  match node with
  | Expression expr -> expressions @ [expr]
  | _ -> expressions
let expressions = foldAST getExpressions [] (VerilogInput ast) // returns all expressions in the AST
```
Note: the function always traverses the entire tree (or subtree), so try to only use it when the entire tree must be traversed.

## Error Checking and UI

In order to follow the ISSIE standards (user-friendly and easy to use), the Code Editor should, upon error, display some analytical error messages, in a way which makes it easy and clear how to solve the errors. There are two types of Errors: (i) Syntax Errors, and (ii) Semantic Errors.

- Syntax Errors:
  - Missing semicolon
  - non-closed parenthesis
  - etc.
- Semantic Errors:
  - Big-endian width format
  - Out of width range output port assignment
  - Use of undefined variables
  - etc.
  
Once the check is complete, all errors are of type `ErrorInfo`, giving all together an `ErrorInfo list`, where:
```fsharp
type ErrorInfo = {Line:int; Col:int; Length: int; Message: string; ExtraErrors: ExtraErrorInfo array option}

type ExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}

type ReplaceType =
    |IODeclaration
    |Assignment
    |Variable of string
    |NoReplace
```
  
### Syntax Errors

Syntax Errors are detected by NearleyJS, as in the case of a syntax error NearleyJS won't be able to parse the code. Nearley provides a self-explanatory error message in which the line and column of the error, the unexpected token, and the tokens that would be accepted as correct based on the given
grammar rules are stated explicitly. 

```js

Syntax error at line 1 col 19:

  module decoder(asd.);
                    ^
Unexpected dot token: ".". Instead, I was expecting to see one of the following:

A ws token based on:
    _$ebnf$1 → _$ebnf$1 ● %ws
    _ →  ● _$ebnf$1
    LIST_OF_PORTS → PORT ● _ %comma _ LIST_OF_PORTS
    MODULE → _ %module __ NAME_OF_MODULE _ %lparen _ ● LIST_OF_PORTS _ %rparen _ %semicolon _ MODULE_ITEMS %endmodule _
    PROGRAM →  ● MODULE
A rparen token based on:
    MODULE → _ %module __ NAME_OF_MODULE _ %lparen _ LIST_OF_PORTS _ ● %rparen _ %semicolon _ MODULE_ITEMS %endmodule _
    PROGRAM →  ● MODULE
A comma token based on:
    LIST_OF_PORTS → PORT _ ● %comma _ LIST_OF_PORTS
    MODULE → _ %module __ NAME_OF_MODULE _ %lparen _ ● LIST_OF_PORTS _ %rparen _ %semicolon _ MODULE_ITEMS %endmodule _
    PROGRAM →  ● MODULE
```


However, because the error message analyses all the grammar rules,
this is a very good error explanation only for a programmer with knowledge on parsers, and thus, not for all users. To solve this issue, the `parse` function (see [parser.js](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/parser.js)), is coded such that given an error message, it returns only the line, the column, the unexpected token, and a list of all the expected tokens (i.e. type `ErrorInfo`).

```js
//For the previous error:
Unexpected token "."
Expected: ")",","
```
Specifically, the NearleyJS parser returns a JSON object of type `ParserOutput` which either contains the produced AST in the `Result` field, or the syntax error in the `Error` field. The `newLinesIndex` is an `int array` used for the `errorDiv` and `errorTable` (see [User Interface section](#user-interface)) in order to determine the position of each assignment,variable,etc. in the code editor
```fs
type ParserOutput = {Result: string option; Error: ErrorInfo option; NewLinesIndex: int array option}
```
By using a lexer along with the parser, the tokens given by the parser are very useful, they don't just report the next character expected but the next token, hence there is no real need to do any tedious postprocessing in the `parseFromFile` function, one only needs to convert the token names into strings that are meaningful to the user, for example convert `rparen` to "\)" .


### Semantic Errors

In order to check for Semantic Errors, the code must be syntactically correct, otherwise the AST (used for the semantic error check) won't be generated. Assuming no syntax errors, the `getSemanticErrors` function (see [ErrorCheck.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/ErrorCheck.fs)) will traverse through the AST and run all the semantic checks:

```fsharp
// begin with empty list and add errors to it
[] (type:ErrorInfo)
// all ports are declared as input/output
|> portCheck ast linesLocations 
// all ports declared as IO are defined in the module header
|> checkIODeclarations ast portWidthDeclarationMap portLocationMap linesLocations notUniquePortDeclarations 
// correct port width declaration (e.g. [1:4] -> invalid)
|> checkIOWidthDeclarations ast linesLocations 
// Checks one-by-one all wire and output port assignments for:
// 1) LHS Name and Width
// 2) RHS Names
// 3) RHS Width of inputs/wires
// 4) Width LHS = Width RHS
|> checkWiresAndAssignments ast portMap portSizeMap portWidthDeclarationMap inputSizeMap inputNameList linesLocations wireNameList wireSizeMap wireLocationMap
// checks whether all output ports have been assined a value
|> checkAllOutputsAssigned ast portMap portSizeMap linesLocations
// filter out possible double Errors
|> List.distinct 
```

Regarding the `ExtraErrorInfo` field of `ErrorInfo`, in the case were a suggestion can be made (e.g. port not defined as input/output -> Do you mean: "input {portname}" , "output {portname}"), it will contain the suggestion. Otherwise, if there are no suggestions, the `ExtraErrorInfo` field will contain the same message of the `Message` field of `ErrorInfo`. This is because the ExtraErrorInfo is the source of information for the `errorTable` (see below)     



### User Interface

After running the two error checking functions, all errors are in an `ErrorInfo list`. All these errors need to be displayed in an easy-to-understand way to the user, helping both experts and begginers to solve their errors. There are two ways to display the errors to the user:
1. `errorDiv`
2. `errorTable` 

### ErrorDiv

`errorDiv` is a transparent overlay above the code editor where the errors get converted into red dashed lines which on-hover display the error message. The process to create the `errorDiv` is:
- Sort the error by line and by column
- Per error:
  - find the location of the error using `line`,`col`, and `newLinesIndex`
  - create a line `span` of length equal to the variable,keyword,etc. which needs to be underlined
  - Set the on-hover message equal to the string of `ErrorInfo.Message`

```fsharp
let getUnderLineElement marginLeft _line message = 
    [
      span [Style [Display DisplayOptions.InlineBlock; MarginLeft marginLeft; PointerEvents "stroke"]] []
      span [Class "error"; Style [PointerEvents "auto"; FontSize 16; Color "rgb(255,0,0)"; Background "rgba(255,0,0,0)"]] [str (_line)] 
      span [Class "hide"] [str message]                                 
    ]
```

![errorDiv](https://user-images.githubusercontent.com/59971511/180603703-817b3c45-f08c-40fa-a82e-35d9fa317062.PNG)


For full code, see [CodeEditorHelpers.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/CodeEditorHelpers.fs)


### ErrorTable

`errorTable` is a table that analyses all the errors and gives suggestions to fix the errors. It appears on the RHS, next to the code editor, once the user clicks the "More Info" button. The messages appearing on the table originate from the `ExtraErrorInfo` field. 

In the case of a suggestion, the suggestion appears as a button next to "Do you mean:". Once the user clicks on a suggestion, the text of the suggestion will be added in the correct place in the code editor.     
![errorTable](https://user-images.githubusercontent.com/59971511/180603705-22670bc9-ea45-4f82-9c0b-c322bbf208d4.PNG)


For full code, see [CodeEditorHelpers.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/VerilogComponent/CodeEditorHelpers.fs) and `addButton` function in [CatalogueView.fs](https://github.com/tomcl/issie/tree/master/src/Renderer/UI/CatalogueView.fs)


## AST to ISSIE Sheet

Verilog code and its AST can be turned into an ISSIE sheet if and only if there are no errors in it. This ensures that the sheet will be valid and won't cause any bugs breaking the application. In other words, this is the reason why the error checking is so extensive and the supported Verilog limited. 

The sheet creation depends on the creation of a `Circuit` list where each `Circuit` represents one output or wire assignment. Its type is:
```fsharp
type Circuit = {
    Comps: Component list;
    Conns: Connection list;
    Out: Port;
    OutWidth: int
}
``` 

Initially, all input ports, output ports and wires (wire labels) are extracted from the AST. For each port an input or output port component is created. For combinational variables and output port, a wire label is created which is then connected to the output port component in the case of output ports. For clocked variables instead of the wire label, a register is generated. These can be used multiple times in circuits but must be added only once in the final sheet. For this reason, they are extracted in the beginning, stored in a map for reference while building the sheet, and will be added at the end. The map is of type `Map<string, Component>` so that the characteristics of each input or wire label component can be accessed faster.

Initially, every variable and output port has a circuit. Combinational variables are set to a circuit of constant zero, whereas clocked variables are connected to the corresponding flip flop output (meaning that by default they keep their previous value). Then, the `CompileModule` function recursively traverses the entire tree. At every assignment the circuit corresponding to the LHS variable is updated according to the RHS expression (see [Circuit Creation section](#circuit-creation-from-assignment)). If-else statements and case statements translate to Mux2-s for each variable.

once the `CompileModule` function returns, every port and variable has its final circuit. These final circuits are then connected to the associated wire label, register and/or output port components. As explained before, a `Circuit` is basically a list of `Components` (Component list) and `Connections` (Connection list), which is also known in ISSIE as `CanvasState` - a type that characterizes each sheet. We concatenate all CanvasStates (concatenate components and connections separately) and this yields a ***final CanvasState*** which includes all the logic coded in Verilog except for the input port and wire components, which were not included in the circuit creation (see below how). We simply add all the input port and wire components to the `Component list` of the ***final CanvasState*** and the sheet has been successfully produced. The final canvas state needs some slight modifications (done by `fixCanvasState`), such as
 - give a unique name to each component
 - make sure components do not overlap
 - remove connections between consecutive wire labels and make them have the same name so that they form a single net


### Circuit creation from assignment

No matter how complex an assignment is, it is eventually a list of components connected together, starting from an input or a wire or a constant, and ending in an output or a wire. The circuit is built using the following 6 recursive functions:
- buildExpressionCircuit
- buildUnaryCircuit
- buildUnaryListCircuit (for concatenations)
- buildConditionalCircuit
- buildShiftCircuit
- buildReductionAndLogicalCircuit

The code starts from the deepest node in the assignment tree (input, wire, constant, register) creating the circuit for the input port for example:
```fsharp
{
Comps = []; //input port component will be added at the end
Conns = [];
Out = inputComp.OutputPorts[0];
Width = inputComp.Width;
}
```
and then traverses the tree upwards, adding components and connections to that initial circuit, updating the `Out` and `Width` fields accordingly at each level.

## Adding new features
Adding new features the following steps are needed:
1. Update the AST as necessary
    - looking at the official grammar might help with this
    - add in the new records / modify the existing records to handle the new feature
2. Update lexer if there are any new tokens necessary
3. Update the parser with the new production rules
    - This is straightforward, copy and potentially simplify the grammar found in the most recent version of SystemVerilog standard, available [here](https://ieeexplore.ieee.org/document/8299595)
    - Keep an eye out for ambiguities, use the offset property of tokens to get location information necessary for error messages
4. Update error handling
    - Add a new DU case to the ASTNode type if necessary
    - Look through error handling functions to see if anything needs to be updated
    - For most features, the folder function passed into the foldAST would have to be updated: either by adding a new case there or by modifying the existing cases in the match statement
    - Add further error handling functions to handle the errors related to the new features
5. Modify sheet creation
    - Either add a new case in the CompileModule function or modify an existing case
    - Possibly modify how some variables are initialised - for example arrays would be initialised to RAMs instead of registers or wire labels
6. Test!
    - To generate reference outputs with Icarus Verilog, first install Icarus Verilog. Install the lates version from [here](https://bleyer.org/icarus/) for Windows. For Unix based systems, follow the installation guide found [here](https://github.com/steveicarus/iverilog).
    - To test new features, add malformed inputs for semantic error check and add correct inputs for codegen tests
    - Semantic error checks: add the input to `src/Renderer/VerilogComponent/test/input/semantic`, following the existing testcases
      - Insert the error message in the Issie IDE to see if the error is as expected
      - If the error is correct, run the testcases, then save the file generated at `src/Renderer/VerilogComponent/test/output/semantic/<testcase>.json` to `src/Renderer/VerilogComponent/test/ref/semantic/<testcase>.json`
      - Now you can run the tests by clicking Sheet > Verilog > Run Verilog tests
    - Hardware generation checks: add the input to `src/Renderer/VerilogComponent/test/codegen`, following the JSON format of the other testcases
      - Include a list of inputs, containing input name, width and values; list of outputs describing output name and width; whether the component is clocked; the name of the module
        - The order of the inputs must match the order in which they are declared in in the testcase
      - Now you can generate the driver modules by clicking Sheet > Verilog > Generate driver modules
      - Then click Sheet > Verilog > Icarus compile testcases, to get the binaries of the tests
      - Click Sheet > Verilog > Icarus run testcases to generate the reference outputs
      - To run all the compiler tests, click Sheet > Verilog > Run Verilog tests. Check the browser console (dev tools) for the test results
    - If the tests fail, try hard restarting the app and see if that helps
    - Currently *39/39* semantic error tests pass and *61/63* hardware generation test pass
7. Update this wiki as necessary




    