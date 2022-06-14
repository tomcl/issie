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
