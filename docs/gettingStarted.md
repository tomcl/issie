---
title: Getting Started
category: Documentation
categoryindex: 1
index: 3
---

# Getting Started

## Users

Go to the [latest ISSIE release](https://github.com/tomcl/issie/releases/latest). Scroll down this page till at the bottom you find the `Assets` section - this has binaries for Windows and Macos and linux PCs. Download the appropriate one and unzip it anywhere (or add the dmg file to applications under Macos by double-clicking). 

No installation is required - ISSIE runs from the unzipped files under windows if you double-click the top-level `issie.exe` file with the blue ISSIE chip icon. The Windows binaries are currently unsigned

Issie is designed to be intuitive with no manual required. Users will find the one page [User Tutorial](userGuide.html) here is a useful introduction to Issie features. It can be skimmed quickly to see what is possible, or followed in detail for an easy introduction.

### Creating binaries

It is possible for users to create their own binaries from source, if for example the latest release does not have an uploaded set of binaries. Binaries for a given platform can be created on that platform and run successfully without signing. Follow the steps in [Getting Started as Developer](https://github.com/tomcl/ISSIE#getting-started-as-developer), then run `npm run dist`.

## Developers

See the  [Getting Started as Developer](https://github.com/tomcl/ISSIE#getting-started-as-developer) section of the repo README for how to develop Issie code.

Development works on Windows, MacOS and Linux: the same npm scripts drive all three, with
`build.cmd` on Windows and `build.sh` elsewhere for the one-off set-up.

### Resources for developers

* The ISSIE [wiki](https://github.com/tomcl/issie/wiki) has information on the details of how ISSIE is designed: 
   - [Coding guidelines](https://github.com/tomcl/issie/wiki/1---Coding-guidelines-for-ISSIE)
   - [Code Overview](https://github.com/tomcl/issie/wiki/0---Issie-Source-Code-Overview)
   - Details of main algorithms
* The [Reports page](reports.md) has some long technical reports on parts of ISSIE.
* F# XML documentation on the [ISSIE API](reference/index.html)


