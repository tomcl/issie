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

# --clean, because fsdocs otherwise leaves whatever is already in output/ in
# place: reference pages for types that no longer exist, and pages carrying an
# older template, both of which then look like a change that failed to take. CI
# never sees this - it starts from a fresh checkout with no output/ at all - so
# without --clean a local build does not show what will be published.
dotnet fsdocs build --clean

# fsdocs catches its own phase errors and still exits 0, and the deploy step
# publishes whatever is in output/ - so a failed build silently overwrites the
# live site. Refuse to get that far.
if [ ! -f output/index.html ]; then
  echo "build_docs: fsdocs produced no output/index.html - refusing to publish" >&2
  exit 1
fi

# fsdocs copies its own Binder scaffolding into the output, so a Dockerfile
# pinned to the .NET 7 SDK and a NuGet.config listing long-dead feeds were being
# published at the root of the site, where they read as Issie's own.
rm -f output/Dockerfile output/NuGet.config

# A sitemap listing the documentation pages. The 750-odd generated API reference
# pages are deliberately left out: they are what buries the documentation in
# search results, and robots.txt cannot help here - it is only read from the root
# of a domain, and this site is a project page under tomcl.github.io/issie/.
sitemap=output/sitemap.xml
today=$(date -u +%Y-%m-%d)
{
  echo '<?xml version="1.0" encoding="UTF-8"?>'
  echo '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'
  (cd output && find . -name '*.html' -not -path './reference/*' | sort) |
    while read -r page; do
      url=${page#./}
      # A directory index is addressed by its directory.
      [ "$url" = "index.html" ] && url=""
      # Percent-encode the one space in a published file name.
      url=${url// /%20}
      echo "  <url><loc>https://tomcl.github.io/issie/${url}</loc><lastmod>${today}</lastmod></url>"
    done
  echo '</urlset>'
} > "$sitemap"
echo "build_docs: wrote $(grep -c '<url>' "$sitemap") urls to $sitemap"

# GitHub Pages serves this for any missing path under /issie/. Written here rather
# than kept in docs/ so that fsdocs never sees a second .html file in its input.
cat > output/404.html <<'HTML'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta content="width=device-width, initial-scale=1.0" name="viewport">
    <title>Page not found | Issie</title>
    <link href="/issie/img/favicon.ico" rel="icon" sizes="32x32" type="image/x-icon"/>
    <style>
        body { margin: 0; display: grid; place-items: center; min-height: 100vh;
               font: 16px/1.5 system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
               color: #24292f; background: #fff; }
        main { max-width: 34rem; padding: 2rem; }
        h1 { font-size: 1.75rem; margin: 0 0 1rem; }
        ul { padding-left: 1.2rem; }
        li { margin: .4rem 0; }
        a { color: #0969da; }
        @media (prefers-color-scheme: dark) {
            body { color: #e6edf3; background: #0d1117; }
            a { color: #4493f8; }
        }
    </style>
</head>
<body>
<main>
    <h1>That page is not here</h1>
    <p>The Issie documentation moved some pages around. Try one of these:</p>
    <ul>
        <li><a href="/issie/">Home</a></li>
        <li><a href="/issie/gettingStarted.html">Getting Started</a> - downloading and running Issie</li>
        <li><a href="/issie/userGuide.html">User Tutorial</a> - the one-page tutorial</li>
        <li><a href="/issie/features.html">Features</a> - what Issie does</li>
        <li><a href="/issie/reference/index.html">API reference</a></li>
    </ul>
</main>
</body>
</html>
HTML
echo "build_docs: wrote output/404.html"

# Check if the script is being run by GitHub Actions.
# If it is, then the script will not run dotnet fsdocs watch
if [ "${GITHUB_ACTIONS:-}" == "true" ]
then
  echo "Running on GitHub Actions"
else
  dotnet fsdocs watch
fi
