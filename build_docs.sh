#!/bin/bash
set -euo pipefail

dotnet build

# fsdocs reads the XML doc file beside each assembly. F# names anonymous record
# types `<>f__AnonymousType...`, and a bare `<` is not legal inside an XML
# attribute value, so the reader throws on the first one and fsdocs writes no
# HTML at all. Escape them first. The framework directory is found rather than
# hard-coded: hard-coding it meant the move to .NET 10 left the sed pointing at
# a net8.0 path that no longer existed, and the site was replaced by a bare
# search index for several days without any run failing.
escape_angle_brackets () {
  local project=$1 assembly=$2 bin="$1/bin/Debug"
  if [ "$(find "$bin" -name "$assembly.xml" -type f | wc -l)" -eq 0 ]; then
    echo "build_docs: no $assembly.xml under $bin - did dotnet build emit one?" >&2
    exit 1
  fi
  find "$bin" -name "$assembly.xml" -type f -exec sed -i.bak 's/<>/\&lt;\&gt;/g' {} +
}

escape_angle_brackets src/Main Main
escape_angle_brackets src/Renderer Renderer

dotnet fsdocs build

# fsdocs catches its own phase errors and still exits 0, and the deploy step
# publishes whatever is in output/ - so a failed build silently overwrites the
# live site. Refuse to get that far.
if [ ! -f output/index.html ]; then
  echo "build_docs: fsdocs produced no output/index.html - refusing to publish" >&2
  exit 1
fi

# Check if the script is being run by GitHub Actions.
# If it is, then the script will not run dotnet fsdocs watch
if [ "${GITHUB_ACTIONS:-}" == "true" ]
then
  echo "Running on GitHub Actions"
else
  dotnet fsdocs watch
fi
