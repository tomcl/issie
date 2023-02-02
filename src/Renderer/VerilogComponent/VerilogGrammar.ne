# To compile the .ne file to .js file:
#   1. Install the nearley compiler
#   2. Run: "npx nearleyc VerilogGrammar.ne -o VerilogGrammar.js"

##############################################  SOURCE TEXT  ##################################################
@{%
const moo = require("moo");

const lexer = moo.compile({
    nand: '~&',
    nor: '~|',
    sll: '<<',
    srl: '>>',
    sra: '>>>',
    xor_xnor: ["^","~^","^~"],
    gte: '>=',
    lte: '<=',
    lor: '||',
    land: '&&',
    eq: '==',
    neq: '!=',
    //hexBase: "'h",
    //binaryBase: "'b",
    decimalBase: "'d",
    lparen: '(',
    rparen: ')',
    semicolon: ';',
    comma: ',',
    lbracket: '[',
    rbracket: ']',
    at: '@',
    op_assign: '=',
    colon: ':',
    question: '?',
    or: '|',
    and: '&',
    not: '~',
    lbrace: '{',
    rbrace: '}',
    lt: '<',
    gt: '>',
    plus: '+',
    minus: '-',
    lnot: '!',
    mult: '*',
    dot: '.',
    binary: /\'b[0-1]+/,
    unsigned_number: /[0-9]+/,
    all_numeric: /\'h[0-9a-fA-F]+/,
    IDENTIFIER: {match: /[a-zA-Z][a-zA-Z_0-9]*/, type: moo.keywords({
        keywords: ["alias","always", "always_comb", "always_ff", "and","assert","assign","assume","automatic","before","begin","bind","bins","binsof","bit","break","buf","bufif0","bufif1","byte","case","casex","casez","cell","chandle","class","clocking","cmos","config","const","constraint","context","continue","cover","covergroup","coverpoint","cross","deassign","default","defparam","design","disable","dist","do","edge","else","end","endcase","endclass","endclocking","endconfig","endfunction","endgenerate","endgroup","endinterface","endmodule","endpackage","endprimitive","endprogram","endproperty","endsequence","endspecify","endtable","endtask","enum","event","expect","export","extends","extern","final","first_match","for","force","foreach","forever","fork","forkjoin","function","generate","genvar","highz0","highz1","if","iff","ifnone","ignore_bins","illegal_bins","import","incdir","include","initial","inout","input","inside","instance","int","integer","interface","intersect","join","join_any","join_none","large","liblist","library","local","localparam","logic","longint","macromodule","matches","medium","modport","module","nand","negedge","new","nmos","nor","noshowcancelled","not","notif0","notif1","null","or","output","package","packed","parameter","pmos","posedge","primitive","priority","program","property","protected","pull0","pull1","pulldown","pullup","pulsestyle_ondetect","pulsestyle_onevent","pure","rand","randc","randcase","randsequence","rcmos","real","realtime","ref","reg","release","repeat","return","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared","sequence","shortint","shortreal","showcancelled","signed","small","solve","specify","specparam","static","string","strong0","strong1","struct","super","supply0","supply1","table","tagged","task","this","throughout","time","timeprecision","timeunit","tran","tranif0","tranif1","tri","tri0","tri1","triand","trior","trireg","type","typedef","union","unique","unsigned","use","uwire","var","vectored","virtual","void","wait","wait_order","wand","weak0","weak1","while","wildcard","wire","with","within","wor","xnor","xor"],
        module: "module",
        endmodule: "endmodule",
        input: 'input',
        output: 'output',
        //parameter: 'parameter',
        assign: 'assign',
        bit: 'bit',
        always_comb: 'always_comb',
        always_ff: 'always_ff',
        posedge: 'posedge',
        begin: 'begin',
        end: 'end',
        t_if: 'if',
        t_else: 'else',
        t_case: 'case',
        t_endcase: 'endcase',
        t_default: 'default'
      })},
    ws: {match: /[\s]/, lineBreaks: true},

    EVERYTHING: {match: /./, lineBreaks: true}

});
%}

# Pass your lexer object using the @lexer option:
@lexer lexer

PROGRAM -> MODULE {%function(d) {return {Type: "program", Module: d[0]};} %}
# _ for optional whitespace, __ for obligatory whitespace
MODULE 
    -> _ %module __ NAME_OF_MODULE _ %lparen _ LIST_OF_PORTS _ %rparen _ %semicolon _ MODULE_ITEMS %endmodule _ {%function(d) { return {Type: "module_old", ModuleName: d[3], PortList: d[7], ModuleItems: d[13], EndLocation: d[14].offset}; } %}
    | _ %module __ NAME_OF_MODULE _ %lparen _ (IO_ITEMS _ {%function(d){return d[0];}%}):? %rparen _ %semicolon _ NON_PORT_MODULE_ITEMS %endmodule _ {%function(d) {return {Type: "module_new", ModuleName: d[3], IOItems: d[7], ModuleItems: d[12], EndLocation: d[13].offset};} %}

NAME_OF_MODULE -> IDENTIFIER {% id %}
 
LIST_OF_PORTS
    -> PORT _ %comma _ LIST_OF_PORTS {%function(d, l, reject) {return {Type: "port_list", Head: d[0], Tail: d[4], Location: d[0].Location};} %}
    | PORT {% function(d,l,reject) {return {Type: "port_list", Head: d[0], Tail: null, Location: d[0].Location};}  %}

PORT -> IDENTIFIER {%function(d) {return {Type: "port", Port: d[0], Location: d[0].Location};} %}

MODULE_ITEMS -> MODULE_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

NON_PORT_MODULE_ITEMS -> NON_PORT_MODULE_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

MODULE_ITEM
    -> INPUT_DECL _ %semicolon _  {%function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], Decl: null, Statement: null, Location: d[0].Location};} %}
    | OUTPUT_DECL _ %semicolon _ {%function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], Decl: null, Statement: null, Location: d[0].Location};} %}
    | NON_PORT_MODULE_ITEM {% id %} #?

NON_PORT_MODULE_ITEM
   -> CONTINUOUS_ASSIGNMENT _ {%function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, Decl: null, Statement: d[0], AlwaysConstruct: null, Location: d[0].Location};} %}
    | ALWAYS_CONSTRUCT {%function(d,l, reject) {return {Type: "item", ItemType: "always_construct", IODecl: null, Decl: null, Statement: null, AlwaysConstruct: d[0], Location: d[0].Location};} %}
    | REG_DECLARATION _ {%function(d,l, reject) {return {Type: "item", ItemType: "logic_decl", IODecl: null, Decl: d[0], Statement: null, AlwaysConstruct: null,Location: d[0].Location};} %}
    | MODULE_INSTANTIATION_STATEMENT _ {%function(d,l, reject) { return {Type: "item", ItemType: "module_instantiation", IODecl: null, Decl: null, Statement: null, AlwaysConstruct: null, ModuleInstantiation: d[0], Location: d[0].Module.Location};} %}
    #| "logic" __ EVERYTHING {%function(d,l, reject) {return {Type: "WIRE-DECL", ItemType: d[0], IODecl: null, Decl: null, Statement: null, AlwaysConstruct: null, Location: l};} %}
    #| "reg" __ EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, Decl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}
    #| "always" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, Decl: null, ParamDecl: null, Statement: null, Location: l};} %}
    #| "always_comb" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null, Location: l};} %}
    #| "initial" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, Decl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}
    #| "case" EVERYTHING {%function(d,l, reject) {return {Type: "NO-CASE", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}

IO_ITEMS 
    -> IO_ITEM _ %comma _ IO_ITEMS {%function(d) {return {Type: "io_items", Head: d[0], Tail: d[4]};} %}
    | IO_ITEM {%function(d) {return {Type: "io_items", Head: d[0], Tail: null};} %}

IO_ITEM
    -> INPUT_DECL  {%function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: d[0].Location};} %}
    | OUTPUT_DECL  {%function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: d[0].Location};} %}

#STATEMENTS -> STATEMENT_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

#STATEMENT_ITEM -> 
    #STATEMENT _ {%function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, ParamDecl: null, Statement: d[0], Location: l};} %}
############################################    DECLARATIONS    ###############################################


INPUT_DECL -> input __ (%bit _ ) (RANGE _ {%(d) => {return d[0]}%}):? LIST_OF_VARIABLES  {%function(d) {
    return {Type: "declaration", DeclarationType: "input", Range: d[3], Variables: d[4], Location: d[0].Location};} %}

OUTPUT_DECL -> output __ (%bit _ ) (RANGE _ {%(d) => {return d[0]}%}):? LIST_OF_VARIABLES {%function(d) {
    return {Type: "declaration", DeclarationType: "output", Range: d[3], Variables: d[4], Location: d[0].Location};} %}

LIST_OF_VARIABLES
    -> NAME_OF_VARIABLE _ %comma _ LIST_OF_VARIABLES {%function(d) {return {Type: "variable_list", Head: d[0], Tail: d[4]};} %}
    | NAME_OF_VARIABLE {% function(d) {return {Type: "variable_list", Head: d[0], Tail: null};}  %}


LIST_OF_VARIABLES2
    -> IDENTIFIER _ %comma _ LIST_OF_VARIABLES2 {%function(d) {return [d[0]].concat(d[4]) ;} %}
    | IDENTIFIER {% function(d) {return [d[0]];}  %}

NAME_OF_VARIABLE -> IDENTIFIER {%function(d) {return {Type: "variable", Name: d[0], Location: d[0].Location};} %}

RANGE -> %lbracket _ UNSIGNED_NUMBER _ %colon _ UNSIGNED_NUMBER _ %rbracket {%function(d,l,reject) {return {Type: "range", Start: d[2], End: d[6], Location: d[0].offset};} %}

#### reg declaration #####
REG_DECLARATION 
    -> %bit __  (RANGE _ {%(d,l,r) => {return d[0]}%}):? LIST_OF_VARIABLES2 _ %semicolon {% (d,l,r) => {
        return {Type: "declaration", DeclarationType: "logic", Range: d[2], Variables: d[3], Location: d[0].offset};} %}
######################################     BEHAVIORAL STATEMENTS    #############################################

### PROCEDURAL BLOCKS AND ASSIGNMENTS

#initial_construct -> "initial" statement_or_null #maybe dont need it
ALWAYS_CONSTRUCT
    -> %always_comb __ STATEMENT {%function(d) {
        return {Type: "always_construct", AlwaysType: d[0].value, Statement: d[2], ClkLoc: 0, Location: d[0].offset};} %}
    | %always_ff _ %at _ %lparen _ %posedge __ "clk" _ %rparen _ STATEMENT {%function(d) {
        return {Type: "always_construct", AlwaysType: d[0].value, Statement: d[12], ClkLoc: d[8].offset, Location: d[0].offset};} %}

#ALWAYS_KEYWORD -> "always" {% id %} | "always_comb" {% id %} | "always_latch" {% id %} | "always_ff" {% id %} #remove latch and maybe basic
#final_construct -> "final" function_statement # maybe dont need it

BLOCKING_ASSIGNMENT ->
    # VARIABLE_LVALUE "=" EXPRESSION {%function(d) {return {Type: "blocking_assignment", LHS: d[0]}, RHS: d[2];} %}# dont need delay DELAY_OR_EVENT_CONTROL
    #| NONRANGE_VARIABLE_LVALUE "=" DYNAMIC_ARRAY_NEW #probs dont need it
    # | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_variable_identifier #figure out what this is
    #select = class_new
    OPERATOR_ASSIGNMENT {%function(d) { return {Assignment: {Type: "blocking_assignment", Operator: d[0].Operator, Assignment: d[0].Assignment}, Location:d[0].Location};} %}

OPERATOR_ASSIGNMENT -> VARIABLE_LVALUE _ ASSIGNMENT_OPERATOR _ EXPRESSION
    {%function(d) { return {Type: "operator_assignment", Operator: d[2].Operator, Assignment: {Type: "assign", LHS: d[0], RHS: d[4]}, Location: d[2].Location};} %}

ASSIGNMENT_OPERATOR ->
    %op_assign {% (d)=>{return {Operator: d[0].value, Location: d[0].offset}} %}

NONBLOCKING_ASSIGNMENT ->
    VARIABLE_LVALUE _ %lte _ EXPRESSION #don't need delay or event control [ delay_or_event_control ]
    {%function(d) {return {Assignment: {Type: "nonblocking_assignment", Assignment: {Type: "assign", LHS: d[0], RHS: d[4]}}, Location: d[2].offset};} %}

#PROCEDURAL_CONTINUOUS_ASSIGNMENT ->
    #"assign" VARIABLE_ASSIGNMENT
    #| deassign variable_lvalue
    #| force variable_assignment
    #| force net_assignment
    #| release variable_lvalue
    #| release net_lvalue

#VARIABLE_ASSIGNMENT -> VARIABLE_LVALUE "=" EXPRESSION

VARIABLE_LVALUE -> 
    L_VALUE {% id %}
    | VARIABLE_BITSELECT_L_VALUE {% id %}

#need multiple statements not just one
SEQ_BLOCK
    -> %begin __ SEQ_BLOCK_STMTS %end __ {% function(d) {return{Type: "seq_block", Statements: d[2], Location: d[0].offset}; } %}# not sure if this is correct

SEQ_BLOCK_STMTS -> STATEMENT:+ {% function(d) {return d[0]}%}

CONDITIONAL_STATEMENT ->
    # IF ELSE_IF:* ELSE:? {%function(d) {if_statements=[d[0]].concat(d[1]); return {Type: "cond_stmt", IfStatements: if_statements, ElseStatement: d[2]}}%}
    IF ELSE:? {% function(d) {return {Type: "cond_stmt", IfStatement: d[0], ElseStatement: d[1], Location: d[0].Location};} %}

STATEMENT -> COMPLETE_STATEMENT {%id%}
        | INCOMPLETE_CONDITIONAL_STATEMENT {%id%}

COMPLETE_STATEMENT 
    -> COMPLETE_CONDITIONAL_STATEMENT  {%id%}
    | NONBLOCKING_ASSIGNMENT _ %semicolon _ 
        {%function(d,l,reject) {
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "<=";
            return {Type: "statement", StatementType: "nonblocking_assignment", NonBlockingAssign: assignment, BlockingAssign: null, SeqBlock: null, Conditional: null, CaseStatement: null, Location: d[0].Location};
        } %}
    | BLOCKING_ASSIGNMENT _ %semicolon _ 
        {%function(d,l,reject){
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "=";
            return {Type: "statement", StatementType: "blocking_assignment", NonBlockingAssign: null, BlockingAssign: assignment, SeqBlock: null, Conditional: null,  CaseStatement: null, Location: d[0].Location};
        } %}
    | SEQ_BLOCK {%function(d,l,reject) {return {Type: "statement", StatementType: "seq_block", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: d[0], Conditional: null,  CaseStatement: null, Location: d[0].Location};} %} #change to statements?
    | CASE_STATEMENT {%function(d,l,reject) {return {Type: "statement", StatementType: "case_stmt", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: null,  CaseStatement: d[0], Location: d[0].Location};}%}

COMPLETE_CONDITIONAL_STATEMENT 
    -> %t_if _ %lparen _ EXPRESSION _ %rparen _ COMPLETE_STATEMENT  %t_else __ COMPLETE_STATEMENT {% function(d) {
        let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
        let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: d[11], Location: d[0].offset};
        return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
        } %}

INCOMPLETE_CONDITIONAL_STATEMENT 
    -> %t_if _ %lparen _ EXPRESSION _ %rparen _ STATEMENT {% function(d) {
        let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
        let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: null, Location: d[0].offset};
        return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
        } %}
    | %t_if _ %lparen _ EXPRESSION _ %rparen _ COMPLETE_STATEMENT  %t_else __ INCOMPLETE_CONDITIONAL_STATEMENT {% function(d) {
        let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
        let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: d[11], Location: d[0].offset};
        return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
    } %}

# this might be ambiguous grammar? I want to have it this way because code gen should be easier maybe
IF -> %t_if _ %lparen _ EXPRESSION _ %rparen _ STATEMENT {% function(d) {return {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset}; } %}
ELSE_IF -> %t_else __ %t_if _ %lparen _ EXPRESSION _ %rparen _ STATEMENT {% function(d) {return {Condition: d[6], Statement: d[10]}; } %}
ELSE -> %t_else __ STATEMENT {% function(d) {return d[2]; } %}

STATEMENT2
    -> NONBLOCKING_ASSIGNMENT _ %semicolon _ 
        {%function(d,l,reject) {
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "<=";
            return {Type: "statement", StatementType: "nonblocking_assignment", NonBlockingAssign: assignment, BlockingAssign: null, SeqBlock: null, Conditional: null, CaseStatement: null, Location: d[0].Location};
        } %}
    | BLOCKING_ASSIGNMENT _ %semicolon _ 
        {%function(d,l,reject){
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "=";
            return {Type: "statement", StatementType: "blocking_assignment", NonBlockingAssign: null, BlockingAssign: assignment, SeqBlock: null, Conditional: null,  CaseStatement: null, Location: d[0].Location};
        } %}
    | SEQ_BLOCK {%function(d,l,reject) {return {Type: "statement", StatementType: "seq_block", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: d[0], Conditional: null,  CaseStatement: null, Location: d[0].Location};} %} #change to statements?
    | CONDITIONAL_STATEMENT {%function(d,l,reject) {return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: d[0],  CaseStatement: null, Location: d[0].Location};}%}
    | CASE_STATEMENT {%function(d,l,reject) {return {Type: "statement", StatementType: "case_stmt", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: null,  CaseStatement: d[0], Location: d[0].Location};}%}

################# CASE STATEMENTS ###################
CASE_STATEMENT # probs only care about first one
    -> %t_case _ %lparen _ EXPRESSION _ %rparen _ (CASE_ITEM {% id %}):+  DEFAULT:? %t_endcase __ {%function(d) { 
        return {Type: "case_stmt", Expression: d[4], CaseItems: d[8], Default: d[9], Location: d[0].offset};}%}
    #| CASE_KEYWORD (case_expression ) "matches" case_pattern_item { case_pattern_item } endcase
    #|  "case" ( case_expression ) "inside"
    #   case_inside_item { case_inside_item } endcase


# for clarity default should be last
DEFAULT
    -> %t_default _ %colon _ STATEMENT {% function(d){return d[4];}%}
CASE_ITEM
    -> NUMBER  _ (%comma _ NUMBER _ {%function(d){return d[2];}%}):*  %colon _ STATEMENT
        {% function(d) {expr = [d[0]].concat(d[2]); return {Type: "case_item", Expressions: expr, Statement: d[5]};}%}
    #| default [ : ] statement_or_null

# case_pattern_item ::=
#     pattern [ &&& expression ] : statement_or_null
#     | default [ : ] statement_or_null
# case_inside_item ::=
#     open_range_list : statement_or_null
#     | default [ : ] statement_or_null



# EDGE_IDENTIFIER -> "posedge" | "negedge" | "edge"

# EVENT_EXPRESSION
#     -> EDGE_IDENTIFIER:? EXPRESSION ("iff" __ EXPRESSION):?
#     #| sequence_instance [ iff expression ] # check what this is!
#     | EVENT_EXPRESSION _ "or" _ EVENT_EXPRESSION
#     | EVENT_EXPRESSION _ "," _ EVENT_EXPRESSION
#     | "(" _ EVENT_EXPRESSION _ ")"

# PROCEDURAL_TIMING_CONTROL
#     -> "@" IDENTIFIER  #hierarchical_event_identifier
#     | "@" _ "(" EVENT_EXPRESSION ")"
#     | "@" _ "*"
#     | "@" _ "(" _ "*" _ ")"
#     #| "@" ps_or_hierarchical_sequence_identifier


CONTINUOUS_ASSIGNMENT
    -> assign __ ASSIGNMENT _ %semicolon {%function(d) {return {Type: "statement", StatementType: "assign", Assignment: d[2], Location: d[0].Location};} %}
    | logic __ WIRE_ASSIGNMENT _ %semicolon {%function(d) {return {Type: "statement", StatementType: "wire", Assignment: d[2], Location: d[0].Location};} %}

ASSIGNMENT -> L_VALUE _ %op_assign _ EXPRESSION {%function(d) {return {Type: "assign", LHS: d[0], RHS: d[4], Location:d[0].Primary.Location};} %}

WIRE_ASSIGNMENT -> WIRE_L_VALUE _ %op_assign _ EXPRESSION {%function(d) {return {Type: "bit", LHS: d[0], RHS: d[4], Location:d[0].Primary.Location };} %} 

###########################################      MODULE INSTANTIATION STATEMENTS    #############################
MODULE_INSTANTIATION_STATEMENT -> IDENTIFIER __ IDENTIFIER _ %lparen _ LIST_OF_PORT_CONNECTIONS _ %rparen _ %semicolon {%(d) => {return {Type: "module_instantiation", Module: d[0], Identifier: d[2], Connections: d[6]}}%}

LIST_OF_PORT_CONNECTIONS ->
        NAMED_PORT_CONNECTION  (_ %comma _ NAMED_PORT_CONNECTION {%(d)=> {return d[3];}%}):* _ {%(d) => {return [d[0]].concat(d[1]);}%}

NAMED_PORT_CONNECTION ->
        %dot _ IDENTIFIER _ %lparen _ MODULE_INSTANTIATION_PRIMARY _ %rparen {% (d) => {return {Type: "named_port_connection", PortId: d[2], Primary: d[6]}}%}

MODULE_INSTANTIATION_PRIMARY
    -> IDENTIFIER {%function(d) {return {Type: "primary", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket UNSIGNED_NUMBER %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket UNSIGNED_NUMBER %colon UNSIGNED_NUMBER %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};} %}
###########################################      EXPRESSIONS      ###############################################

WIRE_L_VALUE
    -> IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | %lbracket UNSIGNED_NUMBER %colon UNSIGNED_NUMBER %rbracket _ IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[1], BitsEnd: d[3], Primary: d[6]};} %}

L_VALUE
    -> IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket UNSIGNED_NUMBER %rbracket {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket UNSIGNED_NUMBER %colon UNSIGNED_NUMBER %rbracket {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};} %}

VARIABLE_BITSELECT_L_VALUE
    -> IDENTIFIER _ %lbracket EXPRESSION %rbracket {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: null, BitsEnd: null, Primary: d[0], VariableBitSelect: d[3], Width: 1};} %}
    #| IDENTIFIER _ %lbracket EXPRESSION _ %minus _ %colon UNSIGNED_NUMBER %rbracket {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: null, BitsEnd: null, Primary: d[0], VariableBitSelect: d[3], Width: parseInt(d[8].value)};} %} 


EXPRESSION -> CONDITIONAL {% id %}

CONDITIONAL
    -> LOGICAL_OR _ %question _ CONDITIONAL_RESULT {%function(d) {return {Type: "conditional_cond", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}
    | LOGICAL_OR {% id %}

CONDITIONAL_RESULT
    -> LOGICAL_OR _ %colon _ LOGICAL_OR {%function(d) {return {Type: "conditional_result", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}

LOGICAL_OR
    -> LOGICAL_OR _ %lor _ LOGICAL_AND {%function(d) {return {Type: "logical_OR", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}
    | LOGICAL_AND {% id %}

LOGICAL_AND
    -> LOGICAL_AND _ %land _ BITWISE_OR {%function(d) {return {Type: "logical_AND", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}  
    | BITWISE_OR {% id %}

BITWISE_OR 
    -> BITWISE_OR _ %or _ BITWISE_XOR {%function(d) {return {Type: "bitwise_OR", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}
    | BITWISE_XOR {% id %}

BITWISE_XOR  
    -> BITWISE_XOR _ XOR_XNOR_OPERATOR _ BITWISE_AND {%function(d) {return {Type: "bitwise_XOR", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | BITWISE_AND {% id %}

BITWISE_AND 
    -> BITWISE_AND _ %and _ LOGICAL_SHIFT {%function(d) {return {Type: "bitwise_AND", Operator:d[2].value, Head: d[0], Tail: d[4]};} %}
    | EQUALITY {% id %}

# here put case equality, logical equality, comparison
EQUALITY
    -> EQUALITY _ EQUALITY_OPERATOR _ COMPARISON {%function(d) {return {Type: "equality", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | COMPARISON {% id %}

COMPARISON
    -> COMPARISON _ RELATIONAL_OPERATOR _ LOGICAL_SHIFT {%function(d) {return {Type: "comparison", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | LOGICAL_SHIFT {% id %}

LOGICAL_SHIFT
    -> LOGICAL_SHIFT _ SHIFT_OPERATOR _ UNSIGNED_REDUCTED {%function(d) {return {Type: "SHIFT", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | LOGICAL_SHIFT _ SHIFT_OPERATOR _ ADDITIVE {%function(d) {return {Type: "SHIFT", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | ADDITIVE {% id %}

ADDITIVE
    -> ADDITIVE _ ADDITIVE_OPERATOR _ MULTIPLICATIVE {%function(d) {return {Type: "additive", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | MULTIPLICATIVE {% id %}

MULTIPLICATIVE
    -> MULTIPLICATIVE _ MULTIPLICATION_OPERATOR _ REDUCTION_OR_NEGATION {%function(d) {return {Type: "multiplicative", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | REDUCTION_OR_NEGATION {% id %}

REDUCTION_OR_NEGATION
    -> %lparen _ UNARY_OPERATOR _ UNARY _ %rparen {%function(d) {return {Type: "reduction", Operator:d[2], Unary: d[4]};} %}
    | %not _ UNARY {%function(d) {return {Type: "negation", Operator: "~", Unary: d[2]};} %}
    | UNARY {%function(d) {return {Type: "unary", Unary: d[0]};} %}

UNARY 
    -> PRIMARY {%function(d) {return {Type: "primary", Primary: d[0], Number: null, Expression: d[0].Expression};} %}
    | NUMBER {%function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};} %}
    | %lparen _ BITWISE_OR _ %rparen {%function(d) {return {Type: "parenthesis", Primary: null, Number: null, Expression: d[2]};} %}
    | %lbrace _ LIST_OF_UNARIES _ %rbrace {%function(d) {return {Type: "concat", Primary: null, Number: null, Expression: d[2]};} %}


LIST_OF_UNARIES
    -> EXPRESSION _ %comma _ LIST_OF_UNARIES {%function(d) {return {Type: "unary_list", Head : d[0], Tail: d[4]};} %}
    | EXPRESSION {% function(d) {return {Type: "unary_list", Head: d[0], Tail: null};}  %}


#### Used for unsigned numbers (only in logical/arithmetic shifts)
UNSIGNED_REDUCTED 
    -> UNSIGNED_UNARY {%function(d) {return {Type: "unary_unsigned", Unary: d[0]};} %}

UNSIGNED_UNARY
    -> U_NUMBER {%function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};} %}

U_NUMBER
    -> %unsigned_number {%function(d,l,reject) {return {Type: "number", NumberType: "decimal", Bits: null, Base: null, UnsignedNumber: d[0].value, AllNumber: null, Location: d[0].offset};} %}

##############
EQUALITY_OPERATOR -> %eq {%(d)=>{return d[0].value}%} | %neq {%(d)=>{return d[0].value}%} 

RELATIONAL_OPERATOR -> %lt {%(d)=>{return d[0].value}%} | %lte {%(d)=>{return d[0].value}%} | %gt {%(d)=>{return d[0].value}%} | %gte {%(d)=>{return d[0].value}%} 

ADDITIVE_OPERATOR -> %plus {%(d)=>{return d[0].value}%} | %minus {%(d)=>{return d[0].value}%} 

XOR_XNOR_OPERATOR -> %xor_xnor {%(d)=>{return d[0].value}%} 

SHIFT_OPERATOR -> %sll {%(d)=>{return d[0].value}%} | %srl {%(d)=>{return d[0].value}%} | %sra {%(d)=>{return d[0].value}%} 

UNARY_OPERATOR -> %lnot {%(d)=>{return d[0].value}%}  | %and {%(d)=>{return d[0].value}%}  | %nand {%(d)=>{return d[0].value}%} | %or {%(d)=>{return d[0].value}%} | %nor {%(d)=>{return d[0].value}%} #{%function(d) {return d[0].join('');} %}

MULTIPLICATION_OPERATOR -> %mult {%(d)=>{return d[0].value}%} 


PRIMARY
    -> IDENTIFIER {%function(d) {return {Type: "primary", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket _ UNSIGNED_NUMBER _ %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bit", BitsStart: d[4], BitsEnd: d[4], Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket _ UNSIGNED_NUMBER _ %colon _ UNSIGNED_NUMBER _ %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bits", BitsStart: d[4], BitsEnd: d[8], Primary: d[0]};} %}
    | IDENTIFIER _ %lbracket _ EXPRESSION _ %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bit2", BitsStart: null, BitsEnd: null, Primary: d[0], Expression: d[4], Width:1};} %}
    #| IDENTIFIER _ %lbracket _ EXPRESSION _ %minus _ %colon _ UNSIGNED_NUMBER _ %rbracket {%function(d) {return {Type: "primary", PrimaryType: "identifier_bit2", BitsStart: null, BitsEnd: null, Primary: d[0], Expression: d[4], Width:parseInt(d[10].value)};} %}


NUMBER
    -> %unsigned_number ALL_NUMERIC {%function(d,l,reject) {
        let num = d[1].slice(2);
        return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'h", UnsignedNumber: null, AllNumber: num, Location: d[0].offset};
        } %}
    | %unsigned_number BINARY_NUMBER {%function(d,l,reject) {
        let num = d[1].slice(2);
        return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'b", UnsignedNumber: null, AllNumber: num, Location: d[0].offset};
        } %}
    | %unsigned_number %decimalBase UNSIGNED_NUMBER {%function(d,l,reject) {return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'d", UnsignedNumber: null, AllNumber: d[2], Location: d[0].offset};} %}
    

UNSIGNED_NUMBER -> %unsigned_number {%(d)=>{return d[0].value}%}

ALL_NUMERIC -> %all_numeric {%(d)=>{return d[0].value}%}

BINARY_NUMBER -> %binary {%(d)=>{return d[0].value}%} 


#HEX_DIGIT -> %binary {%d => {return d[0].value}%} | %unsigned_number {%d => { return d[0].value}%}  | %all_numeric {%d => {return d.value}%}
#DECIMAL_DIGIT -> %unsigned_number {%d => { return d[0].value}%} | %binary {%d => {return d[0].value}%}
#BINARY_DIGIT -> %binary {%d => {return d[0].value}%}

#BASE -> "'b" | "'h" {% id %}

CONCAT 
    -> EXPRESSION _ %comma _ CONCAT {%function(d) {return {Type: "concatenation_list", Head: d[0], Tail: d[4]};} %}
    | EXPRESSION {% function(d) {return {Type: "concatenation_list", Head: d[0], Tail: null};}  %}


##############################################    GENERAL    #################################################

input -> %input {% d=>{return {Location: d[0].offset}} %}
output -> %output {% d=>{return {Location: d[0].offset}} %}
parameter -> %parameter {% id %}
assign -> %assign {% d=>{return {Location: d[0].offset}} %}
wire -> %wire {% d=>{return {Location: d[0].offset}} %}
logic -> %bit {% d=>{return {Location: d[0].offset}} %}
endmodule -> %endmodule {% d=>{return {Location: d[0].offset}} %}
 
EVERYTHING -> %EVERYTHING

IDENTIFIER -> %IDENTIFIER {%
    function(d,l, reject) {
        //const keywords = ["alias","and","assert","assign","assume","automatic","before","begin","bind","bins","binsof","bit","break","buf","bufif0","bufif1","byte","case","casex","casez","cell","chandle","class","clocking","cmos","config","const","constraint","context","continue","cover","covergroup","coverpoint","cross","deassign","default","defparam","design","disable","dist","do","edge","else","end","endcase","endclass","endclocking","endconfig","endfunction","endgenerate","endgroup","endinterface","endmodule","endpackage","endprimitive","endprogram","endproperty","endsequence","endspecify","endtable","endtask","enum","event","expect","export","extends","extern","final","first_match","for","force","foreach","forever","fork","forkjoin","function","generate","genvar","highz0","highz1","if","iff","ifnone","ignore_bins","illegal_bins","import","incdir","include","initial","inout","input","inside","instance","int","integer","interface","intersect","join","join_any","join_none","large","liblist","library","local","localparam","logic","longint","macromodule","matches","medium","modport","module","nand","negedge","new","nmos","nor","noshowcancelled","not","notif0","notif1","null","or","output","package","packed","parameter","pmos","posedge","primitive","priority","program","property","protected","pull0","pull1","pulldown","pullup","pulsestyle_ondetect","pulsestyle_onevent","pure","rand","randc","randcase","randsequence","rcmos","real","realtime","ref","reg","release","repeat","return","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared","sequence","shortint","shortreal","showcancelled","signed","small","solve","specify","specparam","static","string","strong0","strong1","struct","super","supply0","supply1","table","tagged","task","this","throughout","time","timeprecision","timeunit","tran","tranif0","tranif1","tri","tri0","tri1","triand","trior","trireg","type","typedef","union","unique","unsigned","use","uwire","var","vectored","virtual","void","wait","wait_order","wand","weak0","weak1","while","wildcard","wire","with","within","wor","xnor","xor"]
        const name = d[0].value; //+ d[1].join('');
        // if (keywords.includes(name)) {
        //     return reject;
        // } else {
        //     return  {Name: name, Location: l};
        // }
        return  {Name: name, Location: d[0].offset};
    }
%}

_ -> %ws:*
__ -> %ws:+ 

