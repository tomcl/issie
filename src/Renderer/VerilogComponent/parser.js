
const nearley = require("nearley");
const verilogGrammar = require("./VerilogGrammar.js");

// calls the nearley parser
// if parser fails it returns the error message provided by the nearley parser in a simple form
export function parseFromFile(source) {
    try {
        const parser = new nearley.Parser(nearley.Grammar.fromCompiled(verilogGrammar));
        const sourceTrimmed = source.replace(/\s+$/g, '');
        const sourceTrimmedComments = sourceTrimmed.replace(/\/\/.*$/gm,' '); //\/\*[\s\S]*?\*\/|([^\\:]|^)
        // console.log(sourceTrimmedComments);
        parser.feed(sourceTrimmedComments);
        let results = parser.results;
        
        let lines = sourceTrimmedComments.split(/\n/); 
        let linesIndex = [0];
        let count=0;
        for(let i=0;i<lines.length-1;i++){
            linesIndex.push(lines[i].length+1+count);
            count = lines[i].length+1+count;
        }
        linesIndex.push(sourceTrimmedComments.length)  
        const ast = results[0];
        return JSON.stringify({Result: JSON.stringify(ast), Error: null, NewLinesIndex: linesIndex});
    }
    catch(e) {
        // console.log(e.message)
        let token = e.token;
        let message = e.message;
        let lineCol = message.match(/[0-9]+/g)
        let expected = message.match(/(?<=A ).*(?= based on:)/g).map(s => s.replace(/\s+token/i,''));
        
        let table = message.substring(message.indexOf(".") + 1);
        let expectedKeywords = table.match(/assign|input|output|wire|parameter|endmodule/g);
        let unique = expectedKeywords.filter((v, i, a) => a.indexOf(v) === i);
        let checkForChars=false;
        let checkForEqual=false;
        let checkForKeyword=false
        for (let i = 0; i < expected.length; i++) {
            switch (expected[i]) {
                case '")"':
                    expected.splice(i,1)
                    expected.unshift('")"')
                    break;
                case 'character matching /[\\s]/':
                    expected.splice(i,1)
                    i--;
                    break;
                case 'character matching /[a-zA-Z]/':
                    expected[i] = '{VARIABLE}'
                    break;
                case 'character matching /[0-9]/':
                    expected[i] = '{NUMBER}'
                    break;
                case 'character matching /[a-zA-Z_0-9]/':
                    expected[i] = '{More characters}';
                    checkForKeyword=true;
                    break;
                case '"="':
                    checkForEqual=1;
                default:
                    let char = expected[i][1]
                    
                    // if character (except b,h,d) most likely comes from beginning of line (i.e. missing assign,wire,output...)
                    if(char.toUpperCase() != char.toLowerCase()){
                        if(char != 'b' && char != 'h' && char != 'd'){
                            expected.splice(i,1)
                            i--;
                            checkForChars=true;
                        }
                    }
                    break;
            }
        }
        if(checkForChars){
            expected.unshift('assign','wire','endmodule','input','output');
        }
        if(checkForEqual){
            expected = ['"="']
        }
        
        if(checkForKeyword &&(expected.length==1)){
            expected = ['{More characters}\nIs the previous variable a keyword?']
        }

        // console.log(expected);

        let newMessage = `Unexpected ${token.type} token "${token.value}" `+
        `at line ${lineCol[0]} col ${lineCol[1]}.`;
        if (expected && expected.length) newMessage += `\nExpected: ${[...new Set(expected)]}`;  
        if (token.value == '\n'){
            token.value = '\''+'newline'+'\'';
        }
        else if(token.value == ';'){
            token.value = '"'+token.value+'".';
        }
        else{token.value = '"'+token.value+'"'}
        let jsonobj = {Line: parseInt(lineCol[0]), Col: parseInt(lineCol[1]), Length: 2, Message: `Unexpected token ${token.value}  `+`\nExpected: ${[...new Set(expected)]}`}
        return JSON.stringify({Result: null, NewLinesIndex: null, Error: JSON.stringify(jsonobj)});
    }
}

// function used to fix the json produced by the nearley parser
// two cases:
// 1. old-style grammar (ports as names in module header, IO declaration in body)
//      port list is:
//
//      LIST_OF_PORTS
//          -> PORT _ "," _ LIST_OF_PORTS {% function (d, l, reject) { return { Type: "port_list", Head: d[0], Tail: d[4], Location: l }; } %}
//          | PORT {% function (d, l, reject) { return { Type: "port_list", Head: d[0], Tail: null, Location: l }; } %}
//
//      analyses the above and returns a list of all ports
//
//      similarly for IO declarations where we can have "input a,b,c;"

// 2. new- style grammar(IO declarations in module header)
//      as above for IO declarations in module header
//          ex: (
//          input a,b,
//          input [3:0] c,
//          output c,
//          )
//
//      and then move all IODecl in ItemList (with statements) and create a 'fake' port list with all names from IODecls
//      so that the rest of the code (errorCheck.fs, SheetCreator.fs) can work as for old-style grammar

export function fix(json_data) {
    var obj = JSON.parse(json_data);

    if (obj.Module.Type == "module_old") {

        ////////  fix port list  ////////  

        var port_list = obj.Module.PortList;
        var ports = [];
        var loc = [];

        try {
            while (port_list.Tail != null) {
                ports.push(port_list.Head.Port.Name);
                loc.push(port_list.Location)
                port_list = port_list.Tail;
            }
            ports.push(port_list.Head.Port.Name);
            loc.push(port_list.Location)
        } catch (e) {
            // console.log(e.message);
        }

        obj.Module.PortList = ports
        obj.Module.Locations = loc


        ////////  fix IO Declarations  ////////

        var item_list = obj.Module.ModuleItems.ItemList;

        var temp_var = [];

        try {
            for (let i = 0; i < item_list.length; i++) {
                if ((item_list[i].ItemType == "input_decl") | (item_list[i].ItemType == "output_decl")) {
                    let variables = item_list[i].IODecl.Variables;
                    while (variables.Tail != null) {
                        temp_var.push(variables.Head.Name);
                        variables = variables.Tail;
                    }
                    temp_var.push(variables.Head.Name);
                    item_list[i].IODecl.Variables = temp_var;
                }
                temp_var = [];
            }
        } catch (e) {
            console.log(e.message);
        }

        obj.Module.ModuleItems.ItemList = item_list
        return JSON.stringify(obj);
    }

    // CASE: new-grammar
    else {

        ////////  fix IO Declaration list  ////////

        var io_list = obj.Module.IOItems;
        var IOs = [];

        try {
            while (io_list.Tail != null) {
                IOs.push(io_list.Head);
                io_list = io_list.Tail;
            }
            IOs.push(io_list.Head);
        } catch (e) {
            console.log(e.message);
        }

        var io_list = IOs;

        ////////  get IO Declaration variables to a list  ////////

        var temp_var = [];

        try {
            for (let i = 0; i < io_list.length; i++) {
                if ((io_list[i].ItemType == "input_decl") | (io_list[i].ItemType == "output_decl")) {
                    let variables = io_list[i].IODecl.Variables;
                    while (variables.Tail != null) {
                        temp_var.push(variables.Head.Name);
                        variables = variables.Tail;
                    }
                    temp_var.push(variables.Head.Name);
                    io_list[i].IODecl.Variables = temp_var;
                }
                temp_var = [];
            }
        } catch (e) {
            console.log(e.message);
        }

        var statement_list = obj.Module.ModuleItems.ItemList;

        obj.Module.ModuleItems.ItemList = io_list.concat(statement_list);

        ////////  create a "fake" PortList element  ////////

        var ports = [];
        var loc = [];

        try {
            for (let i = 0; i < io_list.length; i++) {
                for (let j = 0; j < io_list[i].IODecl.Variables.length; j++) {
                    ports.push(io_list[i].IODecl.Variables[j].Name)
                    loc.push(0)
                }
            }
        } catch (e) {
            console.log(e.message);
        }

        obj.Module.PortList = ports
        obj.Module.Locations = loc

        // delete IOItems element from JSON obj as it doesn't exist in the old-grammar case
        delete obj.Module["IOItems"];

        return JSON.stringify(obj);
    }
}
