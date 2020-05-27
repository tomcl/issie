# Python script that helps transforming the result of logging a file back into
# an f# data structure.
# Useful when writing the CanvasStates for the tests.

import re

state = """
[
    {Id = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d;Type = PushToBusFirst;Label = ;InputPorts = [{Id = 94d62be4-60ae-4e5f-add7-3715341617aa;PortNumber = 0;PortType = Input;HostId = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d}; {Id = c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce;PortNumber = 1;PortType = Input;HostId = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d}];OutputPorts = [{Id = cabe29d9-74ea-06b9-8421-8547090727f9;PortNumber = 0;PortType = Output;HostId = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d}];X = 241;Y = 248};
    {Id = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67;Type = PushToBusLast;Label = ;InputPorts = [{Id = b87776d8-4588-4dc1-3251-342683ee4ba8;PortNumber = 0;PortType = Input;HostId = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67}; {Id = b26122bb-0ce1-8e3e-1021-a9aec7c4c0ca;PortNumber = 1;PortType = Input;HostId = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67}];OutputPorts = [{Id = 211fd204-86fe-e959-b4fa-129d1b4accb4;PortNumber = 0;PortType = Output;HostId = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67}];X = 325;Y = 258}
],
[
    {Id = 6aaab680-a6bc-18c1-65c6-18df546b95eb;Source = {Id = 211fd204-86fe-e959-b4fa-129d1b4accb4;PortNumber = null;PortType = Output;HostId = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67};Target = {Id = c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce;PortNumber = null;PortType = Input;HostId = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d};Vertices = [365,268; 385,268; 385,307; 221,307; 221,268; 241,268]};
    {Id = ae25cb9f-d91c-abd8-acc7-b14cac0dec45;Source = {Id = cabe29d9-74ea-06b9-8421-8547090727f9;PortNumber = null;PortType = Output;HostId = 37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d};Target = {Id = b87776d8-4588-4dc1-3251-342683ee4ba8;PortNumber = null;PortType = Input;HostId = eb13d9a8-162b-3ffc-a1d8-e6666edf7a67};Vertices = [281,258; 325,258]}
]
"""

def applyAll(functions, init):
    res = init
    for f in functions:
        res = f(res)
    return res

def addQuotesId(str):
    return re.sub(r'Id = ([0-9a-z\-]*)', r'Id = "\1"', str)

def addQuotesLabel(str):
    return re.sub(r'Label = ([0-9a-zA-Z\-]*)', r'Label = "\1"', str)

def addPortTypeDot(str):
    return re.sub(r'PortType = (Input|Output)', r'PortType = PortType.\1', str)

def replaceNull(str):
    return re.sub(r'PortNumber = null;', r'PortNumber = None;', str)

def replacePortNumberSome(str):
    return re.sub(r'PortNumber = (\d);', r'PortNumber = Some \1;', str)

def replaceCustomName(str):
    return re.sub(r'Name = ([0-9a-zA-Z\-]*)', r'Name = "\1"', str)

def replaceLabels(str):
    return re.sub(r'InputLabels = [([0-9a-zA-Z\-]*); ]', r'Label = "\1"', str)

def replaceFloat(str):
    return re.sub(r'(\d\d+)(,|;|])', r'\1.0\2', str)

functions = [
    addQuotesId,
    addQuotesLabel,
    addPortTypeDot,
    replaceNull,
    replacePortNumberSome,
    replaceCustomName,
    replaceFloat,
]

print(applyAll(functions, state))
