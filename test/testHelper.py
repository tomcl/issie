import json

with open('main.dgm') as f:
    file = json.load(f)
components = file['NewCanvasWithFileWaveSheetInfoAndNewConns']

#get useful parts of the json
usefulParts = []
for component in components[0][0]:
    usefulParts.append({"Label":component["Label"], "X":component["X"],"Y":component["Y"],"Type":component["Type"]})

#form f# code
indent = "    "
print(indent + "initSheetModel")
def processType(typeDefList):
    line = []
    for d in typeDefList:
        if(d is not None):
            line = line + [str(d).capitalize()]
        else:
            line = line + ["None"]
    separator = ", "
    return "("+ separator.join(line) + ")"
i=0
for part in usefulParts:
    line ="placeSymbol \""+part["Label"]+"\""
    if(type(part["Type"]) is dict):
        t=list(part["Type"].keys())[0]
        line = str(line)+" ("+str(t + processType(part["Type"][t]))+")"
    else:
        line = str(line) + " "+ str(part["Type"])
    line = f"{line} {{X = {part['X']}; Y = {part['Y']}}}"
    if(i == 0):
        print("|> "+line)
    else:
        print(indent + indent + "|> Result.bind ("+line+")")
    i += 1
print(indent + indent + "|> getOkOrFail")

    #  initSheetModel
    #       |> placeSymbol "S1" (Input1(1, None)) {X = 1862.66; Y = 1841.49}
    #       |> Result.bind (placeSymbol "MUX1" Mux2 {X = 1572.66; Y = 1671.49})
    #       |> Result.bind (placeSymbol "S2" (Input1(1, None)) {X = 1752.66; Y = 1651.49})
    #       |> Result.bind (placeSymbol "MUX2" Mux2 {X = 1627.66; Y = 1776.49})
    #       |> Result.bind (placeSymbol "G1" (GateN(And, 2)) {X = 1735.16; Y = 1708.99})
    #       |> getOkOrFail