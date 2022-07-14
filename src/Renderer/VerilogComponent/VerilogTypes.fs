
module VerilogTypes

open Fable.React.Props

//////////////////////// Code Editor Types /////////////////////////////////

type State = 
    | Code of string

type CodeEditorProps = 
    | Placeholder of string
    // | Value of (string -> State)
    | Value of string
    // | OnValueChange of (string -> obj)
    | OnValueChange of (string -> unit)
    | Highlight of (string -> obj) //highlight (string => string | React.Node)
    | TabSize of int
    | InsertSpaces of bool
    | IgnoreTabKey of bool
    | Padding of int
    | TextAreaId of string
    | TextAreaClassName of string
    | PreClassName of string


//////////////////////// Verilog Input Record   ///////////////////////////
type IdentifierT = {Name: string; Location: int}

type ModuleNameT = {Type : string; Name : IdentifierT}

type NumberT = {Type: string; NumberType: string; Bits: string option; Base: string option; UnsignedNumber: string option; AllNumber: string option; Location: int }

type RangeT = {Type: string; Start: string; End: string; Location: int}

type IOItemT = {Type: string; DeclarationType: string; Range : RangeT option; Variables: IdentifierT array}

type ParameterT = {Type: string; Identifier: IdentifierT; RHS: NumberT}
type ParameterItemT = {Type: string; DeclarationType: string; Parameter : ParameterT;}

type PrimaryT = {Type: string; PrimaryType: string; BitsStart: string option; BitsEnd: string option; Primary: IdentifierT}

type ExpressionT = {Type: string; Operator: string option; Head: ExpressionT option; Tail: ExpressionT option; Unary: UnaryT option}
    and UnaryT = {Type: string; Primary: PrimaryT option; Number: NumberT option; Expression: ExpressionT option}

type AssignmentLHST = {Type: string; PrimaryType: string; BitsStart: string option; BitsEnd: string option; Primary: IdentifierT}    
type AssignmentT = {Type: string; LHS: AssignmentLHST; RHS: ExpressionT}

type StatementItemT = {Type: string; StatementType: string; Assignment : AssignmentT;}

type ItemT = {Type: string; ItemType: string; IODecl: IOItemT option; ParamDecl: ParameterItemT option; Statement: StatementItemT option; Location: int}

type ModuleItemsT = {Type : string; ItemList : ItemT array}

type ModuleT = {Type : string; ModuleName : IdentifierT; PortList : string array; Locations: string array; ModuleItems : ModuleItemsT}

type VerilogInput = { Type:string; Module: ModuleT; }


////////////////////////////////////////////////////////////////////////////

type PortAssignmentError =
    |Unassigned
    |DoubleAssignment 

type ReplaceType =
    |IODeclaration
    |Assignment
    |Variable of string
    |NoReplace

type OneUnary = {Name:string;Size:int;Parenthesis:OneUnary list option}

type ExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}

type ErrorInfo = {Line:int; Col:int; Length: int; Message: string; ExtraErrors: ExtraErrorInfo array option}

type ParserOutput = {Result: string option; Error: ErrorInfo option; NewLinesIndex: int array option}
