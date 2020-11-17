# Issie - an Interactive Schematic Simulator with Integrated Editor

Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues.

The application is was initially developed by Marco Selvatici, as a Final Year Project.

The intercative Wave Simulator was developed by Edoardo Santi over a Summer UROP.

It is currently being maintained and developed by Tom Clarke (owner). 

If you are just interested in using the application, jump to the [Getting Started](#getting-started) section. For more info about the project, read on.

This documentation is partly based on the excellent [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) documentation, given the similarity in the technology stack used.

## Introduction

For the Issie website go [here](https://tomcl.github.io/issie/).

The application is mostly written in F#, which gets transpiled to JavaScript via the [Fable](https://fable.io/) compiler. [Electron](https://www.electronjs.org/) is then used to convert the developed web-app to a cross-platform application. [Electron](electronjs.org) provides access to platform-level APIs (such as access to the file system) which would not be available to vanilla browser web-apps.

[Webpack 4](https://webpack.js.org/) is the module bundler responsible for the JavaScript concatenation and automated building process: the eelctron-webpack build 
is automated with the all-in-one electron-webpack package.

The drawing capabilities are provided by the [draw2d](http://www.draw2d.org/draw2d/) JavaScript library, which has been extended to support digital electronics components.

The choice of F# as main programming language for the app has been dictated by a few factors:

* the success of the [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2), which uses a similar technology stack;
* strongly typed functional code tends to be easy to maintain and test, as the type-checker massively helps you;
* Imperial College EEE/EIE students learn such language in the 3rd year High-Level-Programming course, hence can maintain the app in the future;
* F# can be used with the powerful [Elmish](https://elmish.github.io/elmish/) framework to develop User Interfaces in a [Functional Reactive Programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) fashion.

## Project Structure

Electron bundles Chromium (View) and node.js (Engine), therefore as in every node.js project, the `package.json` file specifies the (Node) module dependencies.

* dependencies: node libraries that the executable code (and development code) needs
* dev-dependencies: node libraries only needed by development tools

Additionally, the section `"scripts"`:
```
 "scripts": {
    "compile": "dotnet fable src/main && dotnet fable src/renderer",
    "dev": "dotnet fable src/main && dotnet fable watch src/renderer --run electron-webpack dev",
    "watchmain": "dotnet fable watch src/main",
    "webpack": "electron-webpack dev",
    "dist": "npm run compile && electron-builder",
    "distwin": "npm run compile && electron-builder -w",
    "distmac": "npm  compile && electron-builder -m"
  }
```
Defines the in-project shortcut commands, therefore when we use `npm run <stript_key>` is equivalent to calling `<script_value>`. 
For example, in the root of the project, running in the terminal `npm run dev` is equivalent to the command line:

```
dotnet fable src/main && dotnet fable watch src/renderer --run electron-webpack dev
```

This runs fable 3 to compile the main process, then again to compile and watch the renderer process. After the renderer compilation is finished 
`electron-webpack dev` will be run. This invokes `electron-webpack` to pack and lauch the code under electron. Fable 3 will watch the rendere F# files
and convert to javascript on the fly. Electron-webpack will watch the chnges in javascript files and hot load these into the running application.

The build system depends on a `Fake` file `build.fsx`. This has targets representing build tasks, and normally these are used, 
accessed via `build.cmd` or `build.sh`, instead of using `dotnet fake` directly.

## Code Structure

The source code consists of two distinct sections transpiled separately to Javascript to make a complete Electron application.

* The electron main process runs the Electron parent process under the desktop native OS, it starts the app process and provides desktop access services to it.
* The electron client (app) process runs under Chromium in a simulated browser environment (isolated from the native OS).

Electron thus allows code written for a browser (HTML + CSS + JavaScript) to be run as a desktop app with the additional capability of desktop filesystem access via communication between the two processes.

Both processes run Javascript under Node.

The `src/Main/Main.fs` source configures electron start-up and is boilerplate. It is transpiled to the root project directory so it can be automatically picked up by Electron.

The remaining app code is arranged in five different sections, each being a separate F# project. This separation allows all the non-web-based code (which can equally be run and tested under .Net) to be run and tested under F# directly in addition to being transpiled and run under Electron.

The project relies on the draw2d JavaScript library, which is extended to support digital electronics components. The extensions are in the `app/public/lib/draw2d_extensions` folder and are loaded by the `index.html` file. The `index.html` file is otherwise empty as the UI elements are dynamically generated with [React](https://reactjs.org/), thanks to the F# Elmish library.

The code that turns the F# project source into `renderer.js` is the FABLE compiler followed by the Node Webpack bundler that combines multiple Javascript files into a single `renderer.js`. Note that the FABLE compiler is distributed as a node package so gets set up automatically with other Node components.

The compile process is controlled by the `.fsproj` files (defining the F# source) and `webpack.additions.main.js`, `webpack.additions.renderer.js`
which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put. 
This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.

## File Structure

### `src` folder

|   Subfolder   |                                             Description                                            |
|:------------:|:--------------------------------------------------------------------------------------------------:|
| `main/` | Code for the main electron process that sets everything up - not normally changed |
| `Common/`       | Provides some common types and utilities used by all other sections, including the  WidthInferer |
| `Simulator/`    | Contains the logic to analyse and simulate a diagram.                                              |
| `Renderer/`     | Contains the UI logic, the wrapper to the JavaScript drawing library and a set of utility function to write/read/parse diagram files. This amd `main` are the only projects that cannot run under .Net, as they contain JavaScript related functionalities. |

### `Tests` folder

Contains numerous tests for the WidthInferer and Simulator. Based on F# Expecto testing library.


### `Static` folder

Contains static files used in the application.

### `Docsrc` folder

Contains source information copied (or compiled) into the `docs` directory that controls the project 
[Github Pages](https://pages.github.com/) website, with url [https://tomcl.github.io/issie/](https://tomcl.github.io/issie/).

## Concept of Project and File in Issie

ISSIE allows the users to create projects and files within those projects. A ISSIE project is simply a folder named `<project_name>.dprj` (dprj stands for diagram project). A project contains a collection of designs, each named `<component_name>.dgm` (dgm stands for diagram).

When opening a project, ISSIE will search the given repository for `.dgm` files, parse their content, and allow the user to open them in ISSIE or use them as components in other designs.

## Build Magic

This project uses modern F# / dotnet cross-platform build. The build tools all install cross-platform under dotnet, downloaded automatically by paket. The Fake 
tool (F# make) automates the rest of the build and also downloads Node.


## Getting Started

If you just want to run the app go to the [releases page](https://github.com/tomcl/issie/releases) and follow the instructions on how to 
download and run the prebuilt binaries. Note that the Windows binary will run on Linux under WINE.

If you want to get started as a developer, follow these steps:

1. Download and install the latest [Dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac and Linux users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website 
(the version from brew is incomplete, may lead to MSB error later). If the build fails due to lack of Node.js, 
download and install [Node.js v12](https://nodejs.org/dist/latest-v12.x/) and npm.

2. Download & unzip the [Issie repo](https://github.com/tomcl/ISSIE), or if contributing clone it locally, or fork it on github and then clone it locally.

3. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience 
that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). 
That makes things a lot more pleasant. The new [Windows Terminal](https://github.com/microsoft/terminal) works well.

4. Run `build.cmd` under Windows or `build.sh` under linux or macos. This will download all dependencies and create auto-documentation and binaries, then launch the application with HMR.
  
  * HMR: the application will automatically recompile and update while running if you save updated source files
  * To initialise and reload: `File -> reload page`
  * To exit: after you exit the application the auto-compile script will terminate after about 15s
  * To recompile the application `npm run dev`.
  * To generate distributable binaries for dev host system `npm run dist`.
  * If you have changed node modules use `build dev`. Note that this project uses npm, not yarn. If npm gets stuck use `build cleannode` and try again.
  * From time to time run `build killzombies` to terminate orphan node and dotnet processes which accumulate using this dev chain.


## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) rerun `build.cmd` (Windows) or `build.sh` (Linux and MacOS). You may need first to
run `build killzombies` to remove orphan processes that lock build files.

## TODO

* Incorporate zombie process killing into the build scripts to make manual run unnecessary. Requires care.
* Work out how to incorporate Node and npm dependencies (do we need npm given Fake uses it?). Paket nuget should be used for them.
* Should Node be upgraded to v14?

