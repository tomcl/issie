# Issie - an Interactive Schematic Simulator with Integrated Editor

![Release Version](https://img.shields.io/github/v/release/tomcl/issie?logo=github&label=Release%20Version)
![Release Date](https://img.shields.io/github/release-date/tomcl/issie?display_date=created_at&logo=github&label=Release%20Date)
![Documentation](https://github.com/tomcl/issie/actions/workflows/docs.yml/badge.svg)
![](https://byob.yarr.is/tomcl/issie/build-windows)
![](https://byob.yarr.is/tomcl/issie/build-macos)
![](https://byob.yarr.is/tomcl/issie/build-linux)
![Tests](https://github.com/tomcl/issie/actions/workflows/tests.yml/badge.svg)
![Downloads](https://img.shields.io/github/downloads/tomcl/issie/total?label=Downloads)
![Contributors](https://img.shields.io/github/contributors-anon/tomcl/issie?label=Contributors)

Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. Issie is developed and actively used in teaching at Imperial College London.

* If you are just interested in using the application, jump to the [Getting Started](#getting-started) section. 
* If you want user documentation and news go to the [web pages](https://tomcl.github.io/issie/).
* If you are interested in a more detailed description of Issie please check out the [Wiki](https://github.com/tomcl/issie/wiki).

For more technical info about the project, read on. This documentation is partly based on the excellent [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) documentation, given the similarity in the technology stack used.

## Introduction

For the Issie website go [here](https://tomcl.github.io/issie/).

The application is mostly written in F#, which gets transpiled to JavaScript via the [Fable](https://fable.io/) compiler. [Electron](https://www.electronjs.org/) is then used to convert the developed web-app to a cross-platform application. Electron provides access to platform-level APIs (such as access to the file system) which would not be available to vanilla browser web-apps.

[Webpack 5](https://webpack.js.org/) is the module bundler responsible for the JavaScript concatenation and automated building process: the build 
is automated using the scripts under the [scripts](scripts/) directory.

The drawing capabilities are provided by a custom schematic editor library implemented in F# and specialised for digital components.

The choice of F# as main programming language for the app has been dictated by a few factors:

* The success of the [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2), which uses a similar technology stack;
* Strongly typed functional code tends to be easy to maintain and test, as the type-checker massively helps you;
* Imperial College EEE/EIE students learn such language in the 3rd year High-Level-Programming course, hence can maintain the app in the future;
* F# can be used with the powerful [Elmish](https://elmish.github.io/elmish/) framework to develop User Interfaces in a [Functional Reactive Programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) fashion.


## Getting Started

If you just want to run the app go to the [releases page](https://github.com/tomcl/issie/releases) and
download and run the latest prebuilt binary for your platform (Windows, macOS or Linux). Issie will require in total about 200M of disk space.

That page is also where to find out what changed: every release is published there with its notes,
written from the commits it contains. There is deliberately no release-notes file in the
repository — one more place to forget to update.

* Windows: unzip \*.zip anywhere and double-click the top-level `Issie.exe` application in the unzipped files.
    * If you get a security warning saying something like: *Microsoft Defender SmartScreen prevented an unrecognized app from starting. Running this app might put your PC at risk.
More info* then:
        * Click **More Info**
        * Then click **Run Anyway**
* Macos: Double click the dmg file  and run the application inside the folder, or drag and drop this to install.
    * If Macos asks you to do this, you will need to change your security settings to allow apps not downloaded from app store
    * *Apple* -> *System settings* -> *Privacy & Security* -> (find at bottom of options by scrolling) *Allow Applications From* ->  *App Store and Known Developers*
* Linux: unzip \*.zip anywhere and run the `issie` executable in the unzipped files.

      

Issie installs and runs without making system changes - all of its code is inside the directory you download. 
You can delete this and replace it by a later version of Issie. Each design sheet is stored in a similarly named file under the project directory. 
The subdirectory `backup` there contains a large numbers of backup snapshots for design recovery. 
These are not needed for Issie operation so you can delete them - or even the whole `backup` directory, if you wish.

Issie binaries will not run (in some cases) from a networked file location (found on many cluster machines). 
If you have this problem navigate to the top-level directory containing the Issie binaries in a command window 
and type `issie.exe --no-sandbox`. See https://github.com/tomcl/issie/issues/125 for details.

Once you open up Issie and are ready to go, feel free to open one of the Demo Projects from the start-up window. These are there to show you what a complete Issie project looks like and enable you to have fun with it without having to design and build it from scratch. Every time you reopen a demo project it will be reset to its initial state.

## Getting Started as Developer

### Prerequisites (common to Windows, macOS, Linux)

Download and install (if you already have these tools, just check the versions):

* [.NET 10 SDK](https://dotnet.microsoft.com/download/dotnet/10.0). Check with `dotnet --version`.
* [Node.js v22 LTS](https://nodejs.org/en/download) or later, which includes `npm`. Check with `node -v`.
    * After installing, update npm itself: `npm install -g npm@latest`.
    * If other projects need a different Node version, use a version manager (nvm, fnm) rather
      than switching global installs.
* (recommended) An F# editor: [VS Code](https://code.visualstudio.com/) with the
  [Ionide extension](https://ionide.io/), or Visual Studio (workload ".NET desktop development"
  with "F# language support" ticked), or JetBrains Rider.
* On Windows, run the commands below from a cmd terminal (in Windows Terminal, set the default
  profile to Command Prompt rather than PowerShell): the helper scripts are `.cmd` files.

### First build

1. Clone the [Issie repo](https://github.com/tomcl/ISSIE) locally, or fork it on GitHub and clone
   your fork. (Downloading as a zip also works: on Windows first right-click the zip file,
   select Properties, and click **Unblock**, then extract.)

2. In the repo root run `build.cmd` (Windows) or `./build.sh` (macOS/Linux; run
   `chmod 755 build.sh` first if it is not executable). This restores the pinned dotnet tools
   (Fable, Paket, FAKE), installs all .NET and npm packages, deletes stale generated JavaScript,
   compiles everything, and starts the app in dev mode. The first compile takes a minute or two.

### Day-to-day development

`build.cmd`/`build.sh` is only needed once, or again when dependencies change. After that, use
the npm scripts directly - they are much faster:

* `npm run dev` - the normal dev loop: compiles the two F# projects in parallel with
  `fable watch` and starts the app. Saving an F# source file recompiles it and hot-reloads the
  running app (use the app's `File -> Reload` if the two ever get out of step). When nothing has
  changed since the last compile, the app starts in a few seconds.
* `npm run dev:once` - compile once and start the app, with no watcher: the fastest way to just
  run Issie from source.
* `npm run debug` - dev mode with runtime assertions enabled; noticeably slower.
* `npm run test` - the Expecto test suite: 396 tests in about a hundred seconds, covering
  simulation, parameter resolution, the Verilog subsystem and the draw block, all under plain .NET
  with no Electron. `npm run test -- --filter Issie.<GroupName>` runs one group, which is usually a
  couple of seconds - see [Tests/README.md](Tests/README.md) for the per-group timings.
* `npm run typecheck` - type-check the renderer under .NET without Fable: the quickest way to
  find out whether a change compiles, with better error messages than Fable's.
* `npm run dist` - production binaries for your platform, under `dist/`.
* `npm version patch|minor|major` - (maintainers, on master) make a release: syncs the version
  in `package.json` and `Version.fs`, commits, tags and pushes; CI then builds all platforms and
  publishes the GitHub release.

Other points:

* To exit dev mode, close the app window and Ctrl-C the watch script in the terminal.
* Orphan `fable watch`, webpack and Electron processes survive unusual terminations - a stray
  watcher still holds an F# compiler in memory - so kill them with `npm run clean-dev`, which works
  on all platforms (`clean-dev-win` and `clean-dev-mac` are aliases for it).
  `npm run clean-dev -- --list` shows what it would kill without killing anything.
* If you change `package.json`, run `npm install` to update `package-lock.json`, and commit both.
* Why a dev start is sometimes near-instant and sometimes a full recompile - Fable's caching
  rules and the things that silently defeat them - is explained in
  [docs/BUILD_OPTIMIZATION.md](docs/BUILD_OPTIMIZATION.md).

### Working in an IDE

Issie also compiles - but cannot run - as a normal .NET solution (`issie.sln`), since at runtime
the code needs Electron and browser APIs. This is very useful: when a Fable error is unclear,
building under .NET (Visual Studio, Ionide, Rider, or `npm run typecheck`) gives much better
error messages, and refactorings can be checked by a .NET compile without touching the app. One
caveat: code inside `#if FABLE_COMPILER` branches is only checked by an actual Fable compile.

#### Node management details

* `package-lock.json` contains exact package versions and comes from the repo. Normally you don't need to change it. The standard build runs `npm ci`, which installs exactly the locked versions and does not change the lock file.
* If you add or upgrade a package in `package.json`, run `npm install` to recreate the lock file, and commit it.
* Single packages can conveniently be changed or added using `npm upgrade name` or `npm install [-D] name` instead of editing `package.json`.
* If a package audits with a problem use `npm ls name` to find which of the required packages use it (usually upgrading or replacing them will remove the problem). Production dependencies are the ones that matter - they ship to users - and are checked by `npm run audit:prod`, which also runs automatically before `npm run dist`.

#### Development on macOS

A clean build will work equally well on macOS, however things are more likely to go wrong if you have previously installed conflicting packages:

* Legacy versions of `dotnet` - can if needed be removed [as here](https://stackoverflow.com/questions/44089518/how-can-i-uninstall-dotnet-core-from-macos-sierra):

  ```bash
  curl -O https://raw.githubusercontent.com/dotnet/sdk/main/scripts/obtain/uninstall/dotnet-uninstall-pkgs.sh
  chmod u+x dotnet-uninstall-pkgs.sh
  sudo ./dotnet-uninstall-pkgs.sh
  ```

* Root permissions in dev files. For dev to work smoothly you need every configuration file to be installed under your own username, so you have r/w access. This will break if you ever find yourself using `sudo` to root install software, or if you have done this some time in the past. In that case you can temporarily get round issues by using `sudo` to run the development (or the generated app) with admin privileges. This is the wrong thing to do. Instead you should use
  * ``chown -R `whoami` dir``
for each directory that might have the files with bad permissions. Typically your dev directory `.` and `/usr/local`.
* Uninstalling and reinstalling latest dotnet is helpful if dotnet has been installed wrong.
* For Apple silicon Mac users, you should use the Arm64 version of .NET in order to get the best results. You can get it from the official Microsoft Website, using their installer.
* `npm run dist` may prompt you to install Python 3 (needed to compile native modules). Install it and run `npm run dist` again.


### Under the hood for developers

The dev chain is identical on all platforms. What the scripts above actually do, step by step:

1. `dotnet tool restore` installs the pinned dev tools from `dotnet-tools.json`: the `Fable`
   compiler, the `Paket` dotnet package manager and the `FAKE` build tool. (Node package
   management is via `npm`, which comes with Node.)
2. `dotnet paket install` installs all of the dotnet-side packages.
3. `npm ci` installs the exact npm package versions recorded in `package-lock.json`.
4. Fable transpiles every `.fs` file in `src/Main` and `src/Renderer` to a `.fs.js` file next to
   it (these are generated files, not in the repo).
5. Webpack bundles the generated JavaScript: `webpack.config.main.js` turns `src/Main/Main.fs.js`
   into `build/index.js`, and `webpack.config.renderer.js` turns `src/Renderer/Renderer.fs.js`
   (plus CSS and static assets) into `build/renderer-index.js`. In dev mode the renderer bundle
   is served by webpack-dev-server with hot reload instead.
6. Electron runs `build/index.js` as its main process, which opens the app window.

`build.cmd`/`build.sh` run steps 1-3 and then hand over to a FAKE target defined in `build.fsx`
(`build.cmd -t <Target>`; the default target `Dev` cleans generated JS and runs `npm run dev`).
Other targets include `Build`, `Dist`, `CleanDev` and `KillZombies`.

* To update the tool versions (not normally needed) edit `dotnet-tools.json`.
* To change the dotnet packages used (advanced) change `paket.dependencies` at top level **and** `paket.references` in the directory of the relevant `.fsproj` file. Currently dotnet packages are not pinned to versions so latest compatible versions are always used. This is probably wrong but seems to work well.
* To interface to a new Node package from F# see the excellent [Fable documentation](https://fable.io/docs/communicate/js-from-fable.html). The **best** way to do this is to write an F# interface file which provides
static typing (like a typescript definition file). In fact there is a wonderful automatic converter [ts2fable](https://github.com/fable-compiler/ts2fable) which generates F# interfaces from typescript `.d` files. This works well, but manual adjustment is needed for anything complex. See [the Electron API interface](https://github.com/tomcl/issie/blob/master/src/Renderer/Common/ElectronAPI.fs) in Issie which was generated in this way from a published electron API `.d` files - in that case the manual adjustment was quite unpleasant because Electron API is very complex.
* To understand Elmish and MVU read the excellent [Elmish book](https://zaid-ajaj.github.io/the-elmish-book/#/)
* For more documentation on Issie in addition to XML code comments see the [Issie Wiki](https://github.com/tomcl/issie/wiki)


## Project Structure

Electron bundles Chromium (View) and node.js (Engine), therefore as in every node.js project, the `package.json` file specifies the (Node) module dependencies.

* dependencies: node libraries that the executable code (and development code) needs
* dev-dependencies: node libraries only needed by development tools

Its `"scripts"` section defines the in-project shortcuts, so `npm run <key>` runs the command
against that key. The ones worth knowing are listed under
[Day-to-day development](#day-to-day-development) above; `npm run` with no arguments prints them
all.

Some of those names are npm's own hooks rather than things you invoke: `predist` runs
automatically before `dist`, and `preversion`, `version` and `postversion` are the three hooks npm
runs around `npm version`, which is what makes a release one command. `npm run test` and
`npm run typecheck` are the two checks to run before a PR, since CI runs neither (it only checks
that the app still compiles).

`npm run dev` runs [dev.js](scripts/dev.js), which starts `dotnet fable watch` for the main and
renderer processes *in parallel*, transpiling the F# to javascript and watching the F# files for
changes. As soon as both projects' generated javascript is up to date — immediately if nothing
changed since the last compile, otherwise when the first compilation finishes — it runs the
[start.js script](scripts/start.js). This invokes `webpack` to pack and launch the javascript code
under electron, watches for changes in the javascript code, and *hot loads* these on the running
application. `npm run dev:once` is the same but compiles exactly once with no watcher: startup is
very fast when nothing has changed, and edits need a rerun to be picked up.

As result of this, at any time saving an edited F# renderer project file causes (nearly) immediate:

* fable transpile to from F# to javascript file (dependent F# files may also be transpiled)
* webpack hot load of any changed javascript files to the running electron application

The build system also has a `FAKE` script, `build.fsx`. FAKE is a DSL written in F# specialised to automate build tasks. `build.fsx` has targets representing build tasks (`Build`, `Dev`, `Dist`, `CleanDev`, `KillZombies`, ...), run via `build.cmd` / `build.sh` / `build.ps1`:

* `build -t <Target>` ==> `dotnet fsi build.fsx -t <Target>` (after restoring tools and packages; the default target is `Dev`)

## Code Overview

The source code consists of two distinct F# projects, transpiled separately to JavaScript, that together make a complete Electron application.

* The electron **main** process (`src/Main`) runs under the desktop native OS: it starts the app process and provides desktop access services (file system, dialogs, native menus) to it. `Main.fs` configures electron start-up and is boilerplate that rarely changes.
* The electron **renderer** (app) process (`src/Renderer`) is the application itself, running under Chromium in a browser environment isolated from the native OS.

Electron thus allows code written for a browser (HTML + CSS + JavaScript) to run as a desktop app with desktop filesystem access, via communication between the two processes: all of Issie's file I/O happens in the main process.

The Fable compiler transpiles each project's F# to JavaScript (a `.fs.js` file beside each `.fs`), and Webpack then bundles each set into a single file: `build/index.js` for the main process and `build/renderer-index.js` for the renderer. The bundling is controlled by `webpack.config.main.js` and `webpack.config.renderer.js` — boilerplate you do not need to change; normally the `.fsproj` files defining the F# sources are all that needs modifying.


## Documentation and Generation
There is a script in the root of the repository, `build_docs.sh`, which will build the documentation for the project using [fsdocs](https://fsprojects.github.io/FSharp.Formatting/). The project must be compile-ready before generating the documentation.

Markdown files under `/docs` are turned into static pages on the documentation site. Any XML comments in the code are turned into documentation comments for every function in the codebase.

To add an update, go to the `/docs/updates` folder and create a new markdown file with the following headers:

```markdown
---
layout: post
title:  [title here]
date:   [ ISO 8601 UTC datetime, etc 2021-07-04 15:52:01 +0100]
category: Updates
index: [index that decides the order of the update. later updates have greater indexes]
---
# your markdown content below
```

See other docs in the `/docs/updates` folder for examples.

All XML comments  (starting with `///`)  under any module and function declarations are turned into documentation under the API Reference section of the documentation website. 

> **Please follow XML rules when creating documentation comments in the code, i.e. no usage of triangular brackets < and > other than for tags. Please do not use double quotes as well!**

`build_docs.sh` also calls `dotnet fsdocs watch` to start a local server hosting the documentation at http://localhost:8901/. The generated documentation for the code is under the "API REFERENCE" section. 

If you've built the docs and want to access the server again, you can run `dotnet fsdocs watch` in the terminal.

> Side note: A script, rather than the usual `dotnet fsdocs build` is used due to an undocumented bug where the compiler creates invalid XML code for functions with anonymous records, assigning attributes with "<>" in their names. This causes the generation to fail. Using `<exclude/>` does not fix the issue, so a workaround is to call a script that uses regex to remove these invalid attributes from the XML documentation before building the documentation. <br> See a similar issue on GitHub that throws a similar error [here.](https://github.com/fsprojects/FSharp.Formatting/issues/707) 

## File Structure

### `src` folder

|   Subfolder or file   |                                             Description                                 |
|:----------------------|:----------------------------------------------------------------------------------------|
| `Main/Main.fs` | Code for the main electron process that sets everything up - not normally changed |
| `Renderer/Common/*`       | Provides some common types and utilities, as well as interfaces to libraries APIs and custom libraries |
| `Renderer/Interface/*` | Contains low-level interface functions, and all the low-level file management              |
| `Renderer/DrawBlock/*` | Contains all the SVG-based schematic editor code in F#|
| `Renderer/Simulator/*` | Contains the logic to analyse and simulate a schematic sheet                               |             
| `Renderer/UI/*`     | Contains the UI logic|
| `Renderer/Renderer.fs` | Top-level file that drives the renderer code: contains Elmish MVU loop and Electron menu code |

### `Tests` folder

The Expecto test suite: `npm run test` (which runs `dotnet run --project Tests/Issie.Tests -c Release` — Expecto uses `dotnet run`, **not** `dotnet test`). It compiles the whole renderer project under .NET, so simulation, parameter resolution, the draw block and UI helpers are all testable without Electron or a browser — 396 tests in about a hundred seconds, of which over a third is the `Issie.VerilogCompiler` group (it spawns node per parse, and is skipped when the `CI` environment variable is set, so `CI=true npm run test` is 385 tests in ~26s). To add a test file, list it in `Tests/Issie.Tests/Issie.Tests.fsproj` **and** add its `tests` value to the list in `Main.fs`; missing either fails silently.


### `Static` folder

Contains static files used in the application.

### `Docs` folder

Contains source information that controls the project documentation web site [https://tomcl.github.io/issie/](https://tomcl.github.io/issie/).

## Project versus File in the Issie application

Issie allows the users to create projects and files within those projects. An Issie project is simply a folder named `<project-name>` that contains an empty file named `<project_name>.dprj` (dprj stands for diagram project). The project folder contains any non-zero number of design files, each named `<component_name>.dgm` (dgm stands for diagram). Each design file represents one design sheet of a hierarchical hardware design; sheets can contain, as components, other sheets.

When opening a project, Issie will initially search the given repository for `.dgm` files, parse and load their content, and allow the user to open them in Issie or use them as components in other designs.


## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) rerun `build.cmd` (Windows) or `build.sh` (Linux and MacOS). 

## Creating binaries

`npm run dist` will generate the correct binaries for your system under `/dist`.

## Licence

Issie is free software under the **GNU General Public License v3 or later** — see
[LICENSE.md](LICENSE.md) for the full text.
