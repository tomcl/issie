// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }

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
var grammar = {
    Lexer: lexer,
    ParserRules: [
    {"name": "PROGRAM", "symbols": ["MODULE"], "postprocess": function(d) {return {Type: "program", Module: d[0]};}},
    {"name": "MODULE", "symbols": ["_", (lexer.has("module") ? {type: "module"} : module), "__", "NAME_OF_MODULE", "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "LIST_OF_PORTS", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_", "MODULE_ITEMS", (lexer.has("endmodule") ? {type: "endmodule"} : endmodule), "_"], "postprocess": function(d) { return {Type: "module_old", ModuleName: d[3], PortList: d[7], ModuleItems: d[13], EndLocation: d[14].offset}; }},
    {"name": "MODULE$ebnf$1$subexpression$1", "symbols": ["IO_ITEMS", "_"], "postprocess": function(d){return d[0];}},
    {"name": "MODULE$ebnf$1", "symbols": ["MODULE$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "MODULE$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "MODULE", "symbols": ["_", (lexer.has("module") ? {type: "module"} : module), "__", "NAME_OF_MODULE", "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "MODULE$ebnf$1", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_", "NON_PORT_MODULE_ITEMS", (lexer.has("endmodule") ? {type: "endmodule"} : endmodule), "_"], "postprocess": function(d) {return {Type: "module_new", ModuleName: d[3], IOItems: d[7], ModuleItems: d[12], EndLocation: d[13].offset};}},
    {"name": "NAME_OF_MODULE", "symbols": ["IDENTIFIER"], "postprocess": id},
    {"name": "LIST_OF_PORTS", "symbols": ["PORT", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "LIST_OF_PORTS"], "postprocess": function(d, l, reject) {return {Type: "port_list", Head: d[0], Tail: d[4], Location: d[0].Location};}},
    {"name": "LIST_OF_PORTS", "symbols": ["PORT"], "postprocess": function(d,l,reject) {return {Type: "port_list", Head: d[0], Tail: null, Location: d[0].Location};}},
    {"name": "PORT", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "port", Port: d[0], Location: d[0].Location};}},
    {"name": "MODULE_ITEMS$ebnf$1", "symbols": []},
    {"name": "MODULE_ITEMS$ebnf$1", "symbols": ["MODULE_ITEMS$ebnf$1", "MODULE_ITEM"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "MODULE_ITEMS", "symbols": ["MODULE_ITEMS$ebnf$1"], "postprocess": function(d) {return {Type: "module_items", ItemList: d[0]};}},
    {"name": "NON_PORT_MODULE_ITEMS$ebnf$1", "symbols": []},
    {"name": "NON_PORT_MODULE_ITEMS$ebnf$1", "symbols": ["NON_PORT_MODULE_ITEMS$ebnf$1", "NON_PORT_MODULE_ITEM"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "NON_PORT_MODULE_ITEMS", "symbols": ["NON_PORT_MODULE_ITEMS$ebnf$1"], "postprocess": function(d) {return {Type: "module_items", ItemList: d[0]};}},
    {"name": "MODULE_ITEM", "symbols": ["INPUT_DECL", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], Decl: null, Statement: null, Location: d[0].Location};}},
    {"name": "MODULE_ITEM", "symbols": ["OUTPUT_DECL", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], Decl: null, Statement: null, Location: d[0].Location};}},
    {"name": "MODULE_ITEM", "symbols": ["NON_PORT_MODULE_ITEM"], "postprocess": id},
    {"name": "NON_PORT_MODULE_ITEM", "symbols": ["CONTINUOUS_ASSIGNMENT", "_"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, Decl: null, Statement: d[0], AlwaysConstruct: null, Location: d[0].Location};}},
    {"name": "NON_PORT_MODULE_ITEM", "symbols": ["ALWAYS_CONSTRUCT"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "always_construct", IODecl: null, Decl: null, Statement: null, AlwaysConstruct: d[0], Location: d[0].Location};}},
    {"name": "NON_PORT_MODULE_ITEM", "symbols": ["REG_DECLARATION", "_"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "logic_decl", IODecl: null, Decl: d[0], Statement: null, AlwaysConstruct: null,Location: d[0].Location};}},
    {"name": "NON_PORT_MODULE_ITEM", "symbols": ["MODULE_INSTANTIATION_STATEMENT", "_"], "postprocess": function(d,l, reject) { return {Type: "item", ItemType: "module_instantiation", IODecl: null, Decl: null, Statement: null, AlwaysConstruct: null, ModuleInstantiation: d[0], Location: d[0].Module.Location};}},
    {"name": "IO_ITEMS", "symbols": ["IO_ITEM", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "IO_ITEMS"], "postprocess": function(d) {return {Type: "io_items", Head: d[0], Tail: d[4]};}},
    {"name": "IO_ITEMS", "symbols": ["IO_ITEM"], "postprocess": function(d) {return {Type: "io_items", Head: d[0], Tail: null};}},
    {"name": "IO_ITEM", "symbols": ["INPUT_DECL"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: d[0].Location};}},
    {"name": "IO_ITEM", "symbols": ["OUTPUT_DECL"], "postprocess": function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: d[0].Location};}},
    {"name": "INPUT_DECL$subexpression$1", "symbols": [(lexer.has("bit") ? {type: "bit"} : bit), "_"]},
    {"name": "INPUT_DECL$ebnf$1$subexpression$1", "symbols": ["RANGE", "_"], "postprocess": (d) => {return d[0]}},
    {"name": "INPUT_DECL$ebnf$1", "symbols": ["INPUT_DECL$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "INPUT_DECL$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "INPUT_DECL", "symbols": ["input", "__", "INPUT_DECL$subexpression$1", "INPUT_DECL$ebnf$1", "LIST_OF_VARIABLES"], "postprocess": function(d) {
        return {Type: "declaration", DeclarationType: "input", Range: d[3], Variables: d[4], Location: d[0].Location};} },
    {"name": "OUTPUT_DECL$subexpression$1", "symbols": [(lexer.has("bit") ? {type: "bit"} : bit), "_"]},
    {"name": "OUTPUT_DECL$ebnf$1$subexpression$1", "symbols": ["RANGE", "_"], "postprocess": (d) => {return d[0]}},
    {"name": "OUTPUT_DECL$ebnf$1", "symbols": ["OUTPUT_DECL$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "OUTPUT_DECL$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "OUTPUT_DECL", "symbols": ["output", "__", "OUTPUT_DECL$subexpression$1", "OUTPUT_DECL$ebnf$1", "LIST_OF_VARIABLES"], "postprocess": function(d) {
        return {Type: "declaration", DeclarationType: "output", Range: d[3], Variables: d[4], Location: d[0].Location};} },
    {"name": "LIST_OF_VARIABLES", "symbols": ["NAME_OF_VARIABLE", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "LIST_OF_VARIABLES"], "postprocess": function(d) {return {Type: "variable_list", Head: d[0], Tail: d[4]};}},
    {"name": "LIST_OF_VARIABLES", "symbols": ["NAME_OF_VARIABLE"], "postprocess": function(d) {return {Type: "variable_list", Head: d[0], Tail: null};}},
    {"name": "LIST_OF_VARIABLES2", "symbols": ["IDENTIFIER", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "LIST_OF_VARIABLES2"], "postprocess": function(d) {return [d[0]].concat(d[4]) ;}},
    {"name": "LIST_OF_VARIABLES2", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return [d[0]];}},
    {"name": "NAME_OF_VARIABLE", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "variable", Name: d[0], Location: d[0].Location};}},
    {"name": "RANGE", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "_", "UNSIGNED_NUMBER", "_", (lexer.has("colon") ? {type: "colon"} : colon), "_", "UNSIGNED_NUMBER", "_", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d,l,reject) {return {Type: "range", Start: d[2], End: d[6], Location: d[0].offset};}},
    {"name": "REG_DECLARATION$ebnf$1$subexpression$1", "symbols": ["RANGE", "_"], "postprocess": (d,l,r) => {return d[0]}},
    {"name": "REG_DECLARATION$ebnf$1", "symbols": ["REG_DECLARATION$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "REG_DECLARATION$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "REG_DECLARATION", "symbols": [(lexer.has("bit") ? {type: "bit"} : bit), "__", "REG_DECLARATION$ebnf$1", "LIST_OF_VARIABLES2", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon)], "postprocess":  (d,l,r) => {
        return {Type: "declaration", DeclarationType: "logic", Range: d[2], Variables: d[3], Location: d[0].offset};} },
    {"name": "ALWAYS_CONSTRUCT", "symbols": [(lexer.has("always_comb") ? {type: "always_comb"} : always_comb), "__", "STATEMENT"], "postprocess": function(d) {
        return {Type: "always_construct", AlwaysType: d[0].value, Statement: d[2], ClkLoc: 0, Location: d[0].offset};} },
    {"name": "ALWAYS_CONSTRUCT", "symbols": [(lexer.has("always_ff") ? {type: "always_ff"} : always_ff), "_", (lexer.has("at") ? {type: "at"} : at), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", (lexer.has("posedge") ? {type: "posedge"} : posedge), "__", {"literal":"clk"}, "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "STATEMENT"], "postprocess": function(d) {
        return {Type: "always_construct", AlwaysType: d[0].value, Statement: d[12], ClkLoc: d[8].offset, Location: d[0].offset};} },
    {"name": "BLOCKING_ASSIGNMENT", "symbols": ["OPERATOR_ASSIGNMENT"], "postprocess": function(d) { return {Assignment: {Type: "blocking_assignment", Operator: d[0].Operator, Assignment: d[0].Assignment}, Location:d[0].Location};}},
    {"name": "OPERATOR_ASSIGNMENT", "symbols": ["VARIABLE_LVALUE", "_", "ASSIGNMENT_OPERATOR", "_", "EXPRESSION"], "postprocess": function(d) { return {Type: "operator_assignment", Operator: d[2].Operator, Assignment: {Type: "assign", LHS: d[0], RHS: d[4]}, Location: d[2].Location};}},
    {"name": "ASSIGNMENT_OPERATOR", "symbols": [(lexer.has("op_assign") ? {type: "op_assign"} : op_assign)], "postprocess": (d)=>{return {Operator: d[0].value, Location: d[0].offset}}},
    {"name": "NONBLOCKING_ASSIGNMENT", "symbols": ["VARIABLE_LVALUE", "_", (lexer.has("lte") ? {type: "lte"} : lte), "_", "EXPRESSION"], "postprocess": function(d) {return {Assignment: {Type: "nonblocking_assignment", Assignment: {Type: "assign", LHS: d[0], RHS: d[4]}}, Location: d[2].offset};}},
    {"name": "VARIABLE_LVALUE", "symbols": ["L_VALUE"], "postprocess": id},
    {"name": "VARIABLE_LVALUE", "symbols": ["VARIABLE_BITSELECT_L_VALUE"], "postprocess": id},
    {"name": "SEQ_BLOCK", "symbols": [(lexer.has("begin") ? {type: "begin"} : begin), "__", "SEQ_BLOCK_STMTS", (lexer.has("end") ? {type: "end"} : end), "__"], "postprocess": function(d) {return{Type: "seq_block", Statements: d[2], Location: d[0].offset}; }},
    {"name": "SEQ_BLOCK_STMTS$ebnf$1", "symbols": ["STATEMENT"]},
    {"name": "SEQ_BLOCK_STMTS$ebnf$1", "symbols": ["SEQ_BLOCK_STMTS$ebnf$1", "STATEMENT"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "SEQ_BLOCK_STMTS", "symbols": ["SEQ_BLOCK_STMTS$ebnf$1"], "postprocess": function(d) {return d[0]}},
    {"name": "CONDITIONAL_STATEMENT$ebnf$1", "symbols": ["ELSE"], "postprocess": id},
    {"name": "CONDITIONAL_STATEMENT$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "CONDITIONAL_STATEMENT", "symbols": ["IF", "CONDITIONAL_STATEMENT$ebnf$1"], "postprocess": function(d) {return {Type: "cond_stmt", IfStatement: d[0], ElseStatement: d[1], Location: d[0].Location};}},
    {"name": "STATEMENT", "symbols": ["COMPLETE_STATEMENT"], "postprocess": id},
    {"name": "STATEMENT", "symbols": ["INCOMPLETE_CONDITIONAL_STATEMENT"], "postprocess": id},
    {"name": "COMPLETE_STATEMENT", "symbols": ["COMPLETE_CONDITIONAL_STATEMENT"], "postprocess": id},
    {"name": "COMPLETE_STATEMENT", "symbols": ["NONBLOCKING_ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l,reject) {
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "<=";
            return {Type: "statement", StatementType: "nonblocking_assignment", NonBlockingAssign: assignment, BlockingAssign: null, SeqBlock: null, Conditional: null, CaseStatement: null, Location: d[0].Location};
        } },
    {"name": "COMPLETE_STATEMENT", "symbols": ["BLOCKING_ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l,reject){
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "=";
            return {Type: "statement", StatementType: "blocking_assignment", NonBlockingAssign: null, BlockingAssign: assignment, SeqBlock: null, Conditional: null,  CaseStatement: null, Location: d[0].Location};
        } },
    {"name": "COMPLETE_STATEMENT", "symbols": ["SEQ_BLOCK"], "postprocess": function(d,l,reject) {return {Type: "statement", StatementType: "seq_block", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: d[0], Conditional: null,  CaseStatement: null, Location: d[0].Location};}},
    {"name": "COMPLETE_STATEMENT", "symbols": ["CASE_STATEMENT"], "postprocess": function(d,l,reject) {return {Type: "statement", StatementType: "case_stmt", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: null,  CaseStatement: d[0], Location: d[0].Location};}},
    {"name": "COMPLETE_CONDITIONAL_STATEMENT", "symbols": [(lexer.has("t_if") ? {type: "t_if"} : t_if), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "COMPLETE_STATEMENT", (lexer.has("t_else") ? {type: "t_else"} : t_else), "__", "COMPLETE_STATEMENT"], "postprocess":  function(d) {
        let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
        let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: d[11], Location: d[0].offset};
        return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
        } },
    {"name": "INCOMPLETE_CONDITIONAL_STATEMENT", "symbols": [(lexer.has("t_if") ? {type: "t_if"} : t_if), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "STATEMENT"], "postprocess":  function(d) {
        let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
        let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: null, Location: d[0].offset};
        return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
        } },
    {"name": "INCOMPLETE_CONDITIONAL_STATEMENT", "symbols": [(lexer.has("t_if") ? {type: "t_if"} : t_if), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "COMPLETE_STATEMENT", (lexer.has("t_else") ? {type: "t_else"} : t_else), "__", "INCOMPLETE_CONDITIONAL_STATEMENT"], "postprocess":  function(d) {
            let ifStmt = {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset};
            let conditional = {Type: "cond_stmt", IfStatement: ifStmt, ElseStatement: d[11], Location: d[0].offset};
            return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: conditional,  CaseStatement: null, Location: d[0].offset}
        } },
    {"name": "IF", "symbols": [(lexer.has("t_if") ? {type: "t_if"} : t_if), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "STATEMENT"], "postprocess": function(d) {return {Type: "ifstmt", Condition: d[4], Statement: d[8], Location: d[0].offset}; }},
    {"name": "ELSE_IF", "symbols": [(lexer.has("t_else") ? {type: "t_else"} : t_else), "__", (lexer.has("t_if") ? {type: "t_if"} : t_if), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "STATEMENT"], "postprocess": function(d) {return {Condition: d[6], Statement: d[10]}; }},
    {"name": "ELSE", "symbols": [(lexer.has("t_else") ? {type: "t_else"} : t_else), "__", "STATEMENT"], "postprocess": function(d) {return d[2]; }},
    {"name": "STATEMENT2", "symbols": ["NONBLOCKING_ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l,reject) {
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "<=";
            return {Type: "statement", StatementType: "nonblocking_assignment", NonBlockingAssign: assignment, BlockingAssign: null, SeqBlock: null, Conditional: null, CaseStatement: null, Location: d[0].Location};
        } },
    {"name": "STATEMENT2", "symbols": ["BLOCKING_ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon), "_"], "postprocess": function(d,l,reject){
            //let len = d[2].offset-d[0].Assignment.LHS.Primary.Location+1;
            //const name = 'a'.repeat(len);
            let assignment = d[0].Assignment;
            assignment.Assignment.Type = "=";
            return {Type: "statement", StatementType: "blocking_assignment", NonBlockingAssign: null, BlockingAssign: assignment, SeqBlock: null, Conditional: null,  CaseStatement: null, Location: d[0].Location};
        } },
    {"name": "STATEMENT2", "symbols": ["SEQ_BLOCK"], "postprocess": function(d,l,reject) {return {Type: "statement", StatementType: "seq_block", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: d[0], Conditional: null,  CaseStatement: null, Location: d[0].Location};}},
    {"name": "STATEMENT2", "symbols": ["CONDITIONAL_STATEMENT"], "postprocess": function(d,l,reject) {return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: d[0],  CaseStatement: null, Location: d[0].Location};}},
    {"name": "STATEMENT2", "symbols": ["CASE_STATEMENT"], "postprocess": function(d,l,reject) {return {Type: "statement", StatementType: "case_stmt", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: null,  CaseStatement: d[0], Location: d[0].Location};}},
    {"name": "CASE_STATEMENT$ebnf$1$subexpression$1", "symbols": ["CASE_ITEM"], "postprocess": id},
    {"name": "CASE_STATEMENT$ebnf$1", "symbols": ["CASE_STATEMENT$ebnf$1$subexpression$1"]},
    {"name": "CASE_STATEMENT$ebnf$1$subexpression$2", "symbols": ["CASE_ITEM"], "postprocess": id},
    {"name": "CASE_STATEMENT$ebnf$1", "symbols": ["CASE_STATEMENT$ebnf$1", "CASE_STATEMENT$ebnf$1$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "CASE_STATEMENT$ebnf$2", "symbols": ["DEFAULT"], "postprocess": id},
    {"name": "CASE_STATEMENT$ebnf$2", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "CASE_STATEMENT", "symbols": [(lexer.has("t_case") ? {type: "t_case"} : t_case), "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "EXPRESSION", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", "CASE_STATEMENT$ebnf$1", "CASE_STATEMENT$ebnf$2", (lexer.has("t_endcase") ? {type: "t_endcase"} : t_endcase), "__"], "postprocess": function(d) { 
        return {Type: "case_stmt", Expression: d[4], CaseItems: d[8], Default: d[9], Location: d[0].offset};}},
    {"name": "DEFAULT", "symbols": [(lexer.has("t_default") ? {type: "t_default"} : t_default), "_", (lexer.has("colon") ? {type: "colon"} : colon), "_", "STATEMENT"], "postprocess": function(d){return d[4];}},
    {"name": "CASE_ITEM$ebnf$1", "symbols": []},
    {"name": "CASE_ITEM$ebnf$1$subexpression$1", "symbols": [(lexer.has("comma") ? {type: "comma"} : comma), "_", "NUMBER", "_"], "postprocess": function(d){return d[2];}},
    {"name": "CASE_ITEM$ebnf$1", "symbols": ["CASE_ITEM$ebnf$1", "CASE_ITEM$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "CASE_ITEM", "symbols": ["NUMBER", "_", "CASE_ITEM$ebnf$1", (lexer.has("colon") ? {type: "colon"} : colon), "_", "STATEMENT"], "postprocess": function(d) {expr = [d[0]].concat(d[2]); return {Type: "case_item", Expressions: expr, Statement: d[5]};}},
    {"name": "CONTINUOUS_ASSIGNMENT", "symbols": ["assign", "__", "ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon)], "postprocess": function(d) {return {Type: "statement", StatementType: "assign", Assignment: d[2], Location: d[0].Location};}},
    {"name": "CONTINUOUS_ASSIGNMENT", "symbols": ["logic", "__", "WIRE_ASSIGNMENT", "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon)], "postprocess": function(d) {return {Type: "statement", StatementType: "wire", Assignment: d[2], Location: d[0].Location};}},
    {"name": "ASSIGNMENT", "symbols": ["L_VALUE", "_", (lexer.has("op_assign") ? {type: "op_assign"} : op_assign), "_", "EXPRESSION"], "postprocess": function(d) {return {Type: "assign", LHS: d[0], RHS: d[4], Location:d[0].Primary.Location};}},
    {"name": "WIRE_ASSIGNMENT", "symbols": ["WIRE_L_VALUE", "_", (lexer.has("op_assign") ? {type: "op_assign"} : op_assign), "_", "EXPRESSION"], "postprocess": function(d) {return {Type: "bit", LHS: d[0], RHS: d[4], Location:d[0].Primary.Location };}},
    {"name": "MODULE_INSTANTIATION_STATEMENT", "symbols": ["IDENTIFIER", "__", "IDENTIFIER", "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "LIST_OF_PORT_CONNECTIONS", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen), "_", (lexer.has("semicolon") ? {type: "semicolon"} : semicolon)], "postprocess": (d) => {return {Type: "module_instantiation", Module: d[0], Identifier: d[2], Connections: d[6]}}},
    {"name": "LIST_OF_PORT_CONNECTIONS$ebnf$1", "symbols": []},
    {"name": "LIST_OF_PORT_CONNECTIONS$ebnf$1$subexpression$1", "symbols": ["_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "NAMED_PORT_CONNECTION"], "postprocess": (d)=> {return d[3];}},
    {"name": "LIST_OF_PORT_CONNECTIONS$ebnf$1", "symbols": ["LIST_OF_PORT_CONNECTIONS$ebnf$1", "LIST_OF_PORT_CONNECTIONS$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "LIST_OF_PORT_CONNECTIONS", "symbols": ["NAMED_PORT_CONNECTION", "LIST_OF_PORT_CONNECTIONS$ebnf$1", "_"], "postprocess": (d) => {return [d[0]].concat(d[1]);}},
    {"name": "NAMED_PORT_CONNECTION", "symbols": [(lexer.has("dot") ? {type: "dot"} : dot), "_", "IDENTIFIER", "_", (lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "MODULE_INSTANTIATION_PRIMARY", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen)], "postprocess": (d) => {return {Type: "named_port_connection", PortId: d[2], Primary: d[6]}}},
    {"name": "MODULE_INSTANTIATION_PRIMARY", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};}},
    {"name": "MODULE_INSTANTIATION_PRIMARY", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "UNSIGNED_NUMBER", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};}},
    {"name": "MODULE_INSTANTIATION_PRIMARY", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "UNSIGNED_NUMBER", (lexer.has("colon") ? {type: "colon"} : colon), "UNSIGNED_NUMBER", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};}},
    {"name": "WIRE_L_VALUE", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};}},
    {"name": "WIRE_L_VALUE", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "UNSIGNED_NUMBER", (lexer.has("colon") ? {type: "colon"} : colon), "UNSIGNED_NUMBER", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket), "_", "IDENTIFIER"], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[1], BitsEnd: d[3], Primary: d[6]};}},
    {"name": "L_VALUE", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};}},
    {"name": "L_VALUE", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "UNSIGNED_NUMBER", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};}},
    {"name": "L_VALUE", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "UNSIGNED_NUMBER", (lexer.has("colon") ? {type: "colon"} : colon), "UNSIGNED_NUMBER", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};}},
    {"name": "VARIABLE_BITSELECT_L_VALUE", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "EXPRESSION", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: null, BitsEnd: null, Primary: d[0], VariableBitSelect: d[3], Width: 1};}},
    {"name": "EXPRESSION", "symbols": ["CONDITIONAL"], "postprocess": id},
    {"name": "CONDITIONAL", "symbols": ["LOGICAL_OR", "_", (lexer.has("question") ? {type: "question"} : question), "_", "CONDITIONAL_RESULT"], "postprocess": function(d) {return {Type: "conditional_cond", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "CONDITIONAL", "symbols": ["LOGICAL_OR"], "postprocess": id},
    {"name": "CONDITIONAL_RESULT", "symbols": ["LOGICAL_OR", "_", (lexer.has("colon") ? {type: "colon"} : colon), "_", "LOGICAL_OR"], "postprocess": function(d) {return {Type: "conditional_result", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "LOGICAL_OR", "symbols": ["LOGICAL_OR", "_", (lexer.has("lor") ? {type: "lor"} : lor), "_", "LOGICAL_AND"], "postprocess": function(d) {return {Type: "logical_OR", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "LOGICAL_OR", "symbols": ["LOGICAL_AND"], "postprocess": id},
    {"name": "LOGICAL_AND", "symbols": ["LOGICAL_AND", "_", (lexer.has("land") ? {type: "land"} : land), "_", "BITWISE_OR"], "postprocess": function(d) {return {Type: "logical_AND", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "LOGICAL_AND", "symbols": ["BITWISE_OR"], "postprocess": id},
    {"name": "BITWISE_OR", "symbols": ["BITWISE_OR", "_", (lexer.has("or") ? {type: "or"} : or), "_", "BITWISE_XOR"], "postprocess": function(d) {return {Type: "bitwise_OR", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "BITWISE_OR", "symbols": ["BITWISE_XOR"], "postprocess": id},
    {"name": "BITWISE_XOR", "symbols": ["BITWISE_XOR", "_", "XOR_XNOR_OPERATOR", "_", "BITWISE_AND"], "postprocess": function(d) {return {Type: "bitwise_XOR", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "BITWISE_XOR", "symbols": ["BITWISE_AND"], "postprocess": id},
    {"name": "BITWISE_AND", "symbols": ["BITWISE_AND", "_", (lexer.has("and") ? {type: "and"} : and), "_", "LOGICAL_SHIFT"], "postprocess": function(d) {return {Type: "bitwise_AND", Operator:d[2].value, Head: d[0], Tail: d[4]};}},
    {"name": "BITWISE_AND", "symbols": ["EQUALITY"], "postprocess": id},
    {"name": "EQUALITY", "symbols": ["EQUALITY", "_", "EQUALITY_OPERATOR", "_", "COMPARISON"], "postprocess": function(d) {return {Type: "equality", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "EQUALITY", "symbols": ["COMPARISON"], "postprocess": id},
    {"name": "COMPARISON", "symbols": ["COMPARISON", "_", "RELATIONAL_OPERATOR", "_", "LOGICAL_SHIFT"], "postprocess": function(d) {return {Type: "comparison", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "COMPARISON", "symbols": ["LOGICAL_SHIFT"], "postprocess": id},
    {"name": "LOGICAL_SHIFT", "symbols": ["LOGICAL_SHIFT", "_", "SHIFT_OPERATOR", "_", "UNSIGNED_REDUCTED"], "postprocess": function(d) {return {Type: "SHIFT", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "LOGICAL_SHIFT", "symbols": ["LOGICAL_SHIFT", "_", "SHIFT_OPERATOR", "_", "ADDITIVE"], "postprocess": function(d) {return {Type: "SHIFT", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "LOGICAL_SHIFT", "symbols": ["ADDITIVE"], "postprocess": id},
    {"name": "ADDITIVE", "symbols": ["ADDITIVE", "_", "ADDITIVE_OPERATOR", "_", "MULTIPLICATIVE"], "postprocess": function(d) {return {Type: "additive", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "ADDITIVE", "symbols": ["MULTIPLICATIVE"], "postprocess": id},
    {"name": "MULTIPLICATIVE", "symbols": ["MULTIPLICATIVE", "_", "MULTIPLICATION_OPERATOR", "_", "REDUCTION_OR_NEGATION"], "postprocess": function(d) {return {Type: "multiplicative", Operator:d[2], Head: d[0], Tail: d[4]};}},
    {"name": "MULTIPLICATIVE", "symbols": ["REDUCTION_OR_NEGATION"], "postprocess": id},
    {"name": "REDUCTION_OR_NEGATION", "symbols": [(lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "UNARY_OPERATOR", "_", "UNARY", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen)], "postprocess": function(d) {return {Type: "reduction", Operator:d[2], Unary: d[4]};}},
    {"name": "REDUCTION_OR_NEGATION", "symbols": [(lexer.has("not") ? {type: "not"} : not), "_", "UNARY"], "postprocess": function(d) {return {Type: "negation", Operator: "~", Unary: d[2]};}},
    {"name": "REDUCTION_OR_NEGATION", "symbols": ["UNARY"], "postprocess": function(d) {return {Type: "unary", Unary: d[0]};}},
    {"name": "UNARY", "symbols": ["PRIMARY"], "postprocess": function(d) {return {Type: "primary", Primary: d[0], Number: null, Expression: d[0].Expression};}},
    {"name": "UNARY", "symbols": ["NUMBER"], "postprocess": function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};}},
    {"name": "UNARY", "symbols": [(lexer.has("lparen") ? {type: "lparen"} : lparen), "_", "BITWISE_OR", "_", (lexer.has("rparen") ? {type: "rparen"} : rparen)], "postprocess": function(d) {return {Type: "parenthesis", Primary: null, Number: null, Expression: d[2]};}},
    {"name": "UNARY", "symbols": [(lexer.has("lbrace") ? {type: "lbrace"} : lbrace), "_", "LIST_OF_UNARIES", "_", (lexer.has("rbrace") ? {type: "rbrace"} : rbrace)], "postprocess": function(d) {return {Type: "concat", Primary: null, Number: null, Expression: d[2]};}},
    {"name": "LIST_OF_UNARIES", "symbols": ["EXPRESSION", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "LIST_OF_UNARIES"], "postprocess": function(d) {return {Type: "unary_list", Head : d[0], Tail: d[4]};}},
    {"name": "LIST_OF_UNARIES", "symbols": ["EXPRESSION"], "postprocess": function(d) {return {Type: "unary_list", Head: d[0], Tail: null};}},
    {"name": "UNSIGNED_REDUCTED", "symbols": ["UNSIGNED_UNARY"], "postprocess": function(d) {return {Type: "unary_unsigned", Unary: d[0]};}},
    {"name": "UNSIGNED_UNARY", "symbols": ["U_NUMBER"], "postprocess": function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};}},
    {"name": "U_NUMBER", "symbols": [(lexer.has("unsigned_number") ? {type: "unsigned_number"} : unsigned_number)], "postprocess": function(d,l,reject) {return {Type: "number", NumberType: "decimal", Bits: null, Base: null, UnsignedNumber: d[0].value, AllNumber: null, Location: d[0].offset};}},
    {"name": "EQUALITY_OPERATOR", "symbols": [(lexer.has("eq") ? {type: "eq"} : eq)], "postprocess": (d)=>{return d[0].value}},
    {"name": "EQUALITY_OPERATOR", "symbols": [(lexer.has("neq") ? {type: "neq"} : neq)], "postprocess": (d)=>{return d[0].value}},
    {"name": "RELATIONAL_OPERATOR", "symbols": [(lexer.has("lt") ? {type: "lt"} : lt)], "postprocess": (d)=>{return d[0].value}},
    {"name": "RELATIONAL_OPERATOR", "symbols": [(lexer.has("lte") ? {type: "lte"} : lte)], "postprocess": (d)=>{return d[0].value}},
    {"name": "RELATIONAL_OPERATOR", "symbols": [(lexer.has("gt") ? {type: "gt"} : gt)], "postprocess": (d)=>{return d[0].value}},
    {"name": "RELATIONAL_OPERATOR", "symbols": [(lexer.has("gte") ? {type: "gte"} : gte)], "postprocess": (d)=>{return d[0].value}},
    {"name": "ADDITIVE_OPERATOR", "symbols": [(lexer.has("plus") ? {type: "plus"} : plus)], "postprocess": (d)=>{return d[0].value}},
    {"name": "ADDITIVE_OPERATOR", "symbols": [(lexer.has("minus") ? {type: "minus"} : minus)], "postprocess": (d)=>{return d[0].value}},
    {"name": "XOR_XNOR_OPERATOR", "symbols": [(lexer.has("xor_xnor") ? {type: "xor_xnor"} : xor_xnor)], "postprocess": (d)=>{return d[0].value}},
    {"name": "SHIFT_OPERATOR", "symbols": [(lexer.has("sll") ? {type: "sll"} : sll)], "postprocess": (d)=>{return d[0].value}},
    {"name": "SHIFT_OPERATOR", "symbols": [(lexer.has("srl") ? {type: "srl"} : srl)], "postprocess": (d)=>{return d[0].value}},
    {"name": "SHIFT_OPERATOR", "symbols": [(lexer.has("sra") ? {type: "sra"} : sra)], "postprocess": (d)=>{return d[0].value}},
    {"name": "UNARY_OPERATOR", "symbols": [(lexer.has("lnot") ? {type: "lnot"} : lnot)], "postprocess": (d)=>{return d[0].value}},
    {"name": "UNARY_OPERATOR", "symbols": [(lexer.has("and") ? {type: "and"} : and)], "postprocess": (d)=>{return d[0].value}},
    {"name": "UNARY_OPERATOR", "symbols": [(lexer.has("nand") ? {type: "nand"} : nand)], "postprocess": (d)=>{return d[0].value}},
    {"name": "UNARY_OPERATOR", "symbols": [(lexer.has("or") ? {type: "or"} : or)], "postprocess": (d)=>{return d[0].value}},
    {"name": "UNARY_OPERATOR", "symbols": [(lexer.has("nor") ? {type: "nor"} : nor)], "postprocess": (d)=>{return d[0].value}},
    {"name": "MULTIPLICATION_OPERATOR", "symbols": [(lexer.has("mult") ? {type: "mult"} : mult)], "postprocess": (d)=>{return d[0].value}},
    {"name": "PRIMARY", "symbols": ["IDENTIFIER"], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};}},
    {"name": "PRIMARY", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "_", "UNSIGNED_NUMBER", "_", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier_bit", BitsStart: d[4], BitsEnd: d[4], Primary: d[0]};}},
    {"name": "PRIMARY", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "_", "UNSIGNED_NUMBER", "_", (lexer.has("colon") ? {type: "colon"} : colon), "_", "UNSIGNED_NUMBER", "_", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier_bits", BitsStart: d[4], BitsEnd: d[8], Primary: d[0]};}},
    {"name": "PRIMARY", "symbols": ["IDENTIFIER", "_", (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "_", "EXPRESSION", "_", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) {return {Type: "primary", PrimaryType: "identifier_bit2", BitsStart: null, BitsEnd: null, Primary: d[0], Expression: d[4], Width:1};}},
    {"name": "NUMBER", "symbols": [(lexer.has("unsigned_number") ? {type: "unsigned_number"} : unsigned_number), "ALL_NUMERIC"], "postprocess": function(d,l,reject) {
        let num = d[1].slice(2);
        return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'h", UnsignedNumber: null, AllNumber: num, Location: d[0].offset};
        } },
    {"name": "NUMBER", "symbols": [(lexer.has("unsigned_number") ? {type: "unsigned_number"} : unsigned_number), "BINARY_NUMBER"], "postprocess": function(d,l,reject) {
        let num = d[1].slice(2);
        return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'b", UnsignedNumber: null, AllNumber: num, Location: d[0].offset};
        } },
    {"name": "NUMBER", "symbols": [(lexer.has("unsigned_number") ? {type: "unsigned_number"} : unsigned_number), (lexer.has("decimalBase") ? {type: "decimalBase"} : decimalBase), "UNSIGNED_NUMBER"], "postprocess": function(d,l,reject) {return {Type: "number", NumberType: "all", Bits: d[0].value, Base: "'d", UnsignedNumber: null, AllNumber: d[2], Location: d[0].offset};}},
    {"name": "UNSIGNED_NUMBER", "symbols": [(lexer.has("unsigned_number") ? {type: "unsigned_number"} : unsigned_number)], "postprocess": (d)=>{return d[0].value}},
    {"name": "ALL_NUMERIC", "symbols": [(lexer.has("all_numeric") ? {type: "all_numeric"} : all_numeric)], "postprocess": (d)=>{return d[0].value}},
    {"name": "BINARY_NUMBER", "symbols": [(lexer.has("binary") ? {type: "binary"} : binary)], "postprocess": (d)=>{return d[0].value}},
    {"name": "CONCAT", "symbols": ["EXPRESSION", "_", (lexer.has("comma") ? {type: "comma"} : comma), "_", "CONCAT"], "postprocess": function(d) {return {Type: "concatenation_list", Head: d[0], Tail: d[4]};}},
    {"name": "CONCAT", "symbols": ["EXPRESSION"], "postprocess": function(d) {return {Type: "concatenation_list", Head: d[0], Tail: null};}},
    {"name": "input", "symbols": [(lexer.has("input") ? {type: "input"} : input)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "output", "symbols": [(lexer.has("output") ? {type: "output"} : output)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "parameter", "symbols": [(lexer.has("parameter") ? {type: "parameter"} : parameter)], "postprocess": id},
    {"name": "assign", "symbols": [(lexer.has("assign") ? {type: "assign"} : assign)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "wire", "symbols": [(lexer.has("wire") ? {type: "wire"} : wire)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "logic", "symbols": [(lexer.has("bit") ? {type: "bit"} : bit)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "endmodule", "symbols": [(lexer.has("endmodule") ? {type: "endmodule"} : endmodule)], "postprocess": d=>{return {Location: d[0].offset}}},
    {"name": "EVERYTHING", "symbols": [(lexer.has("EVERYTHING") ? {type: "EVERYTHING"} : EVERYTHING)]},
    {"name": "IDENTIFIER", "symbols": [(lexer.has("IDENTIFIER") ? {type: "IDENTIFIER"} : IDENTIFIER)], "postprocess": 
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
        },
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", (lexer.has("ws") ? {type: "ws"} : ws)], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"]},
    {"name": "__$ebnf$1", "symbols": [(lexer.has("ws") ? {type: "ws"} : ws)]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", (lexer.has("ws") ? {type: "ws"} : ws)], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "__", "symbols": ["__$ebnf$1"]}
]
  , ParserStart: "PROGRAM"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
