#!/bin/bash
dotnet build

# Define the directories containing the XML files
MAIN_XML="src/Main/bin/Debug/net7.0/Main.xml"
RENDERER_XML="src/Renderer/bin/Debug/net7.0/Renderer.xml"

# Use sed to replace all occurrences of <> with &lt;&gt; in the XML files
sed -i.bak 's/<>/\&lt;\&gt;/g' "$MAIN_XML"
sed -i.bak 's/<>/\&lt;\&gt;/g' "$RENDERER_XML"

# Run dotnet fsdocs build and watch
dotnet fsdocs build
dotnet fsdocs watch

