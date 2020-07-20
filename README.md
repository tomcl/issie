# ISSIE - an Interactive Schematic Simulator with Integrated Editor

ISSIE is an application for digital circuit design and simulation. It is targeted to students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. ISSIE is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues.

The application is currently being developed and maintained by Marco Selvatici, as a Final Year Project.

If you are just interested in using the application, jump to the [Getting Started](#getting-started) section. For more info about the project, read on.

This documentation is largely based on the excellent [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) documentation, given the similarity in the technology stack used.

## Introduction

The application is mostly written in F#, which gets transpiled to JavaScript via the [fable](https://fable.io/) compiler. [Electron](https://www.electronjs.org/) is then used to convert the developed web-app to a cross-platform application. [Electron](electronjs.org) provides access to platform-level APIs (such as access to the file system) which would not be available to vanilla browser web-apps.

[Webpack 4](https://webpack.js.org/) is the module bundler responsible for the JavaScript concatenation and automated building process.

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
{
    ...
    "scripts": {
        "start": "cd src/Main && dotnet fable webpack --port free -- -w --config webpack.config.js",
        "build": "cd src/Main && dotnet fable webpack --port free -- -p --config webpack.config.js",
        "launch": "electron .",
        "debug": "electron . --debug",
    },
    ...
}
```
Defines the in-project shortcut commands, therefore when we use `yarn <stript_key>` is equivalent to calling `<script_value>`. For example, in the root of the project, running in the terminal `yarn launch` is equivalent to running `electron .`.

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

The compile process is controlled by the `.fsproj` files (defining the F# source) and `webpack.config.js` which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put. This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.

## File Structure

### `src` folder

|   Subfolder   |                                             Description                                            |
|:------------:|:--------------------------------------------------------------------------------------------------:|
| `Common/`       | Provides some common types and utilities used by all other sections                                |
| `WidthInferer/` | Contains the logic to infer the width of all connections in a diagram and report possible errors. |
| `Simulator/`    | Contains the logic to analyse and simulate a diagram.                                              |
| `Renderer/`     | Contains the UI logic, the wrapper to the JavaScript drawing library and a set of utility function to write/read/parse diagram files. This is the only project that cannot run under .Net, as it contains JavaScript related functionalities. |
| `Tests/`        | Contains numerous tests for the WidthInferer and Simulator. Based on F# Expecto testing library. |


### `app` folder

| Subfolder or file |                                                                                                                                                          Description                                                                                                                                                          |
|:-----------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| `public/lib/`     | Contains JavaScript code loaded by the `index.html` file. It includes the draw2d library itself, its custom extensions and jquery. Note that draw2d and jquery may be copied (and maybe should?) from the node modules when using the `copy-webpack-plugin`. See the comment in `webpack.config.js` for more info about this. |
| `scss/`           | Stylesheet required by the [Fulma](https://fulma.github.io/Fulma/) UI library (F# port of [bulma](https://bulma.io/)).                                                                                                                                                                                                        |
| `icon.ico`        | ISSIE icon.                                                                                                                                                                                                                                                                                                                  |
| `index.html`      | The page rendered by the renderer process. The HTML DOM is dynamically generated using React when the app is running.                                                                                                                                                                                                         |
| Other             | Other files will be generated in this folder at compilation time. They are ignored by the gitignore, and you don't have to worry about those.                                                                                                                                                                                 |

## Concept of Project and File in ISSIE

ISSIE allows the users to create projects and files within those projects. A ISSIE project is simply a folder named `<project_name>.dprj` (dprj stands for diagram project). A project contains a collection of designs, each named `<component_name>.dgm` (dgm stands for diagram).

When opening a project, ISSIE will search the given repository for `.dgm` files, parse their content, and allow the user to open them in ISSIE or use them as components in other designs.

## Getting Started

If you just want to run the app go to the [releases page](https://github.com/tomcl/ISSIE/releases) and follow the instructions on how to download and run the prebuilt binaries.

If you want to get started as a developer, follow these steps:

1. Download and install the latest (3.x) [Dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac and Linux users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website (the version from brew is incomplete, may lead to MSB error on step 6).

2. Download & unzip the ISSIE repo, or if contributing clone it locally, or fork it on github and then clone it locally.

3. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). That makes things a lot more pleasant. The new [Windows Terminal](https://github.com/microsoft/terminal) works well.

4. Run `build.cmd` under Windows or `build.sh` under linux or macos.

## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) run `build.bat` (Windows) or `build.sh` (Linux and MacOS).

