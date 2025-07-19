#!/bin/bash
dotnet build

# Define the directories containing the XML files
MAIN_XML="src/Main/bin/Debug/net8.0/Main.xml"
RENDERER_XML="src/Renderer/bin/Debug/net8.0/Renderer.xml"

# Use sed to replace all occurrences of <> with &lt;&gt; in the XML files
sed -i.bak 's/<>/\&lt;\&gt;/g' "$MAIN_XML"
sed -i.bak 's/<>/\&lt;\&gt;/g' "$RENDERER_XML"

# Run dotnet fsdocs build 
dotnet fsdocs build

# Check if the script is being run by GitHub Actions. 
# If it is, then the script will not run dotnet fsdocs watch
if [ "$GITHUB_ACTIONS" == "true" ]
then
  echo "Running on GitHub Actions"
else
  dotnet fsdocs watch
fi