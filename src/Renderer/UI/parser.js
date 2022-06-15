
const nearley = require("nearley");
const verilogGrammar = require("./verilog.js");
const fs = require("fs");

export function parseFromFile(source) {
    try {
      const parser = new nearley.Parser(nearley.Grammar.fromCompiled(verilogGrammar));
    //   const source = fs.readFileSync(origin, 'utf8');
      parser.feed(source);
      let results = parser.results;
      
      const ast = results[0];
      return JSON.stringify(ast);
    }
    catch(e) {
        let token = e.token;
        let message = e.message;
        let lineCol = message.match(/[0-9]+/g)
        let expected = message.match(/(?<=A ).*(?= based on:)/g).map(s => s.replace(/\s+token/i,''));
        // console.log(expected);
        const index = expected.indexOf('character matching /[\\s]/');
        if (index > -1) {
            expected.splice(index, 1); // 2nd parameter means remove one item only
        }
    
        let newMessage = `Unexpected ${token.type} token "${token.value}" `+
        `at line ${lineCol[0]} col ${lineCol[1]}.`;
        if (expected && expected.length) newMessage += ` Tokens expected: ${[...new Set(expected)]}`;  
        // console.log(e.message)
        // console.log(newMessage);
        return newMessage;
    }
}

export function fix(json_data){
    var obj = JSON.parse(json_data);
    var port_list = obj.Module.PortList;
    var ports = []

    try{
        while (port_list.Tail != null) {
            ports.push(port_list.Head.PortName);
            port_list = port_list.Tail;
        }
        ports.push(port_list.Head.PortName);
    } catch (e) {
        console.log(e.message);
    }

    obj.Module.PortList = ports

    var item_list = obj.Module.ModuleItems.ItemList;
    
    var temp_var = [];
    
    try{
        for (let i=0; i<item_list.length; i++){
            if((item_list[i].ItemType == "input_decl") | (item_list[i].ItemType == "output_decl")){
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
