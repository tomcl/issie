# Python script that helps transforming the result of logging a file back into
# an f# data structure.
# Useful when writing the CanvasStates for the tests.

import re

state = """
[
    {Id = 8078a917-236f-5a40-18a6-8e2d6a1458f5;Type = Input 2;Label = a;InputPorts = [];OutputPorts = [{Id = cc7c6510-d49b-28d9-e0bc-d5350a2c76a8;PortNumber = 0;PortType = Output;HostId = 8078a917-236f-5a40-18a6-8e2d6a1458f5}];X = 122;Y = 125};
    {Id = fbfb4202-9816-f214-0401-da18caddeb0f;Type = Demux2;Label = ;InputPorts = [{Id = 2b224026-609a-5eae-6885-ff9820dbae5f;PortNumber = 0;PortType = Input;HostId = fbfb4202-9816-f214-0401-da18caddeb0f}; {Id = 68043f9b-47e7-793f-fed2-943a7d7336dc;PortNumber = 1;PortType = Input;HostId = fbfb4202-9816-f214-0401-da18caddeb0f}];OutputPorts = [{Id = 57070484-347e-4701-8c36-f510c2ef150c;PortNumber = 0;PortType = Output;HostId = fbfb4202-9816-f214-0401-da18caddeb0f}; {Id = 123463bf-7868-9a87-6fb1-36627a3c9bad;PortNumber = 1;PortType = Output;HostId = fbfb4202-9816-f214-0401-da18caddeb0f}];X = 254;Y = 110};
    {Id = 00661085-7d05-4185-09ef-4138ac918ad2;Type = Output 2;Label = aa;InputPorts = [{Id = 7bbfa7d3-4718-e6fe-e7e9-8c00e5257f45;PortNumber = 0;PortType = Input;HostId = 00661085-7d05-4185-09ef-4138ac918ad2}];OutputPorts = [];X = 401;Y = 117}
],
[
    {Id = 1ef71074-fa16-a068-84a0-18d324e3ada6;Source = {Id = 57070484-347e-4701-8c36-f510c2ef150c;PortNumber = null;PortType = Output;HostId = fbfb4202-9816-f214-0401-da18caddeb0f};Target = {Id = 7bbfa7d3-4718-e6fe-e7e9-8c00e5257f45;PortNumber = null;PortType = Input;HostId = 00661085-7d05-4185-09ef-4138ac918ad2};Vertices = [284,126.66666666666667; 343.5,126.66666666666667; 343.5,127; 401,127]};
    {Id = f326a7a3-79d5-4ace-f0cf-606657fc2c42;Source = {Id = cc7c6510-d49b-28d9-e0bc-d5350a2c76a8;PortNumber = null;PortType = Output;HostId = 8078a917-236f-5a40-18a6-8e2d6a1458f5};Target = {Id = 2b224026-609a-5eae-6885-ff9820dbae5f;PortNumber = null;PortType = Input;HostId = fbfb4202-9816-f214-0401-da18caddeb0f};Vertices = [152,135; 254,135]}
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
