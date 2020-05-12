# Python script that helps transforming the result of logging a file back into
# an f# data structure.
# Useful when writing the CanvasStates for the tests.

import re

state = """
{Id = 57604fee-25b0-9498-06f1-3a065d046515; Type = Custom {Name = full-adder; InputLabels = [Cin; B; A]; OutputLabels = [Sum; Cout]}; Label = full-adder; InputPorts = [{Id = 6a5725f2-f181-d678-371f-7add822462ea; PortNumber = 0; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; {Id = 13f98ff8-2e41-2aef-cbc8-1ed636c8c8c7; PortNumber = 1; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; {Id = 025e36fe-41f2-7f7c-08e2-7b1bb047b798; PortNumber = 2; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}]; OutputPorts = [{Id = c14929a9-b56c-2414-38e6-6fab3127203f; PortNumber = 0; PortType = Output; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; {Id = 0cb60845-f6b7-4e88-53bc-18e6c06364ca; PortNumber = 1; PortType = Output; HostId = 57604fee-25b0-9498-06f1-3a065d046515}]; X = 298; Y = 129}
{Id = b2842635-e4ec-3113-9130-d827d48e2875; Type = Custom {Name = full-adder; InputLabels = [Cin; B; A]; OutputLabels = [Sum; Cout]}; Label = full-adder; InputPorts = [{Id = ce42647b-a5db-5fa1-9a94-baaeec7416cd; PortNumber = 0; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; {Id = 3496a30f-e2bf-2b12-576d-cf28201850ff; PortNumber = 1; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; {Id = 9c0d3115-bf93-6c1c-b5bb-784cd7ca7896; PortNumber = 2; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}]; OutputPorts = [{Id = f6da6a43-076e-1171-53de-6bef710bcaf8; PortNumber = 0; PortType = Output; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; {Id = 04744361-bbe8-ac74-8643-fae2126ed686; PortNumber = 1; PortType = Output; HostId = b2842635-e4ec-3113-9130-d827d48e2875}]; X = 298; Y = 298}
{Id = 78795182-35c4-1c50-2190-6fc944a2adea; Type = Input; Label = Zero; InputPorts = []; OutputPorts = [{Id = 8b0b16ed-3a4e-2ade-6e4b-6285b0d7d2c8; PortNumber = 0; PortType = Output; HostId = 78795182-35c4-1c50-2190-6fc944a2adea}]; X = 155; Y = 72}
{Id = 69a6ad2a-af19-369f-0483-0e09e6841da3; Type = Input; Label = B0; InputPorts = []; OutputPorts = [{Id = 0b74f567-0740-9667-a7e9-3042aeb7ef8f; PortNumber = 0; PortType = Output; HostId = 69a6ad2a-af19-369f-0483-0e09e6841da3}]; X = 83; Y = 150}
{Id = 82a03f0b-ae31-b487-ed1b-335e235adeb7; Type = Input; Label = A1; InputPorts = []; OutputPorts = [{Id = e3b566b6-abf4-1446-7267-37d2ad1deed7; PortNumber = 0; PortType = Output; HostId = 82a03f0b-ae31-b487-ed1b-335e235adeb7}]; X = 84; Y = 389}
{Id = a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1; Type = Input; Label = B1; InputPorts = []; OutputPorts = [{Id = d59e003b-088b-cf84-0f93-83c2235b5942; PortNumber = 0; PortType = Output; HostId = a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1}]; X = 84; Y = 324}
{Id = 86372781-c2f4-09f2-406f-f385ee7a47a9; Type = Input; Label = A0; InputPorts = []; OutputPorts = [{Id = 0d00e79c-1a98-3744-be2f-b9eb78a1cf5b; PortNumber = 0; PortType = Output; HostId = 86372781-c2f4-09f2-406f-f385ee7a47a9}]; X = 83; Y = 200}
{Id = dbb1f55a-edf3-bde2-4c69-43a02560e17d; Type = Output; Label = Sum1; InputPorts = [{Id = 8dcbc0d6-ab44-c2e2-9e26-90ee31fa2e35; PortNumber = 0; PortType = Input; HostId = dbb1f55a-edf3-bde2-4c69-43a02560e17d}]; OutputPorts = []; X = 514; Y = 321}
{Id = 8f5bded5-f46d-722d-6108-03dda4236c01; Type = Output; Label = Sum0; InputPorts = [{Id = 3a537f37-4c6d-3c85-48a7-040d6263bebe; PortNumber = 0; PortType = Input; HostId = 8f5bded5-f46d-722d-6108-03dda4236c01}]; OutputPorts = []; X = 514; Y = 150}
{Id = 7d948312-376d-1d4b-cf02-90872026be16; Type = Output; Label = Cout; InputPorts = [{Id = bb868347-969a-38d2-2389-2d4ae7b63ce8; PortNumber = 0; PortType = Input; HostId = 7d948312-376d-1d4b-cf02-90872026be16}]; OutputPorts = []; X = 514; Y = 389}
Connections:
{Id = 9a089fef-870a-53d8-00a0-e954963ac8e1; Source = {Id = 0cb60845-f6b7-4e88-53bc-18e6c06364ca; PortNumber = null; PortType = Output; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; Target = {Id = ce42647b-a5db-5fa1-9a94-baaeec7416cd; PortNumber = null; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; Vertices = [370,189; 399,189; 399,255.75; 278,255.75; 278,320.5; 298,320.5]}
{Id = 77ab7f29-5375-44b2-d66f-b7c21d412c7f; Source = {Id = 0b74f567-0740-9667-a7e9-3042aeb7ef8f; PortNumber = null; PortType = Output; HostId = 69a6ad2a-af19-369f-0483-0e09e6841da3}; Target = {Id = 13f98ff8-2e41-2aef-cbc8-1ed636c8c8c7; PortNumber = null; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; Vertices = [113,160; 214.5,160; 214.5,174; 298,174]}
{Id = dba98b52-cddf-04d3-648a-fa7b915d4f87; Source = {Id = d59e003b-088b-cf84-0f93-83c2235b5942; PortNumber = null; PortType = Output; HostId = a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1}; Target = {Id = 3496a30f-e2bf-2b12-576d-cf28201850ff; PortNumber = null; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; Vertices = [114,334; 215,334; 215,343; 298,343]}
{Id = c7ce0fcc-c122-4ce6-3779-0f548cbfab38; Source = {Id = f6da6a43-076e-1171-53de-6bef710bcaf8; PortNumber = null; PortType = Output; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; Target = {Id = 8dcbc0d6-ab44-c2e2-9e26-90ee31fa2e35; PortNumber = null; PortType = Input; HostId = dbb1f55a-edf3-bde2-4c69-43a02560e17d}; Vertices = [370,328; 446.5,328; 446.5,331; 514,331]}
{Id = abc884ae-3a1a-5a35-6f6f-5695d4898759; Source = {Id = 04744361-bbe8-ac74-8643-fae2126ed686; PortNumber = null; PortType = Output; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; Target = {Id = bb868347-969a-38d2-2389-2d4ae7b63ce8; PortNumber = null; PortType = Input; HostId = 7d948312-376d-1d4b-cf02-90872026be16}; Vertices = [370,358; 446.5,358; 446.5,399; 514,399]}
{Id = 3c7b523c-71d3-9378-176c-7c45cfc516cf; Source = {Id = c14929a9-b56c-2414-38e6-6fab3127203f; PortNumber = null; PortType = Output; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; Target = {Id = 3a537f37-4c6d-3c85-48a7-040d6263bebe; PortNumber = null; PortType = Input; HostId = 8f5bded5-f46d-722d-6108-03dda4236c01}; Vertices = [370,159; 446.5,159; 446.5,160; 514,160]}
{Id = ccb35ae8-54b6-4072-6680-3cc05bb00f95; Source = {Id = 0d00e79c-1a98-3744-be2f-b9eb78a1cf5b; PortNumber = null; PortType = Output; HostId = 86372781-c2f4-09f2-406f-f385ee7a47a9}; Target = {Id = 025e36fe-41f2-7f7c-08e2-7b1bb047b798; PortNumber = null; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; Vertices = [113,210; 214.5,210; 214.5,196.5; 298,196.5]}
{Id = 11759cef-e928-32c1-e6ec-b44b198d1a2e; Source = {Id = e3b566b6-abf4-1446-7267-37d2ad1deed7; PortNumber = null; PortType = Output; HostId = 82a03f0b-ae31-b487-ed1b-335e235adeb7}; Target = {Id = 9c0d3115-bf93-6c1c-b5bb-784cd7ca7896; PortNumber = null; PortType = Input; HostId = b2842635-e4ec-3113-9130-d827d48e2875}; Vertices = [114,399; 215,399; 215,365.5; 298,365.5]}
{Id = 2ae1f296-5dfb-986e-2c4c-3e3435d750b9; Source = {Id = 8b0b16ed-3a4e-2ade-6e4b-6285b0d7d2c8; PortNumber = null; PortType = Output; HostId = 78795182-35c4-1c50-2190-6fc944a2adea}; Target = {Id = 6a5725f2-f181-d678-371f-7add822462ea; PortNumber = null; PortType = Input; HostId = 57604fee-25b0-9498-06f1-3a065d046515}; Vertices = [185,82; 250.5,82; 250.5,151.5; 298,151.5]}
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
