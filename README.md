# Issie - an Interactive Schematic Simulator with Integrated Editor

Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. Issie is developed and actively used in teaching at Imperial College London.

* If you are just interested in using the application, jump to the [Getting Started](#getting-started) section. 
* If you want user documentation and news go to the [web pages](https://tomcl.github.io/issie/).

For more technical info about the project, read on. This documentation is partly based on the excellent [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) documentation, given the similarity in the technology stack used.

## Introduction

For the Issie website go [here](https://tomcl.github.io/issie/).

The application is mostly written in F#, which gets transpiled to JavaScript via the [Fable](https://fable.io/) compiler. [Electron](https://www.electronjs.org/) is then used to convert the developed web-app to a cross-platform application. [Electron](electronjs.org) provides access to platform-level APIs (such as access to the file system) which would not be available to vanilla browser web-apps.

[Webpack 4](https://webpack.js.org/) is the module bundler responsible for the JavaScript concatenation and automated building process: the electron-webpack build 
is automated with the all-in-one electron-webpack package.

The drawing capabilities are provided (now) by a custom schemetic editor library implemented in F# and specialised for digital components.

The choice of F# as main programming language for the app has been dictated by a few factors:

* The success of the [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2), which uses a similar technology stack;
* Strongly typed functional code tends to be easy to maintain and test, as the type-checker massively helps you;
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
    "dev": "cd src/main && dotnet fable watch . --run npm run devrenderer",
    "devmain": "cd src/main && dotnet fable watch . --run npm run webpackdev",
    "devrenderer": "cd src/renderer && dotnet fable watch . --run npm run webpackdev",
    "webpackdev": "electron-webpack dev",
    "webpack": "electron-webpack",
    "dist": "npm run compile && npm run webpack &&  electron-builder",
  }
```
Defines the in-project shortcut commands as a set of `<key> : <value` lines, so that when we use `npm run <stript_key>` it is equivalent to calling `<script_value>`. 
For example, in the root of the project, running in the terminal `npm run dev` is equivalent to the command line:

```
cd src/main && dotnet fable watch . --run npm run devrenderer
```

This runs fable 3 to transpile the main process, then (`--run` is an option of fable to run another command) runs script `devrenderer` to transpile to javascript and watch the F# files in the renderer process. After the renderer transpilation is finished 
`electron-webpack dev` will be run. This invokes `webpack` to pack and lauch the javascript code, under electron, and also watches for changes in the javascript code, and *hot loads* these on the running application

As result of this, at any time saving an edited F# renderer project file causes (nearly) immediate:

* fable transpile to from F# to javascript file (dependent F# files may also be transpiled)
* webpack hot load of any changed javascript files to the running electron application

The build system depends on a `Fake` file `build.fsx`. Fake is a DSL written in F# that is specialised to automate build tasks. Build.fsx has targets representing build tasks, and normally these are run via `build.cmd` or `build.sh`, instead of using `dotnet fake` directly:

* `build <target>` ==> `dotnet fake build -t <target>`

## Code Structure

The source code consists of two distinct sections transpiled separately to Javascript to make a complete Electron application.

* The electron main process runs the Electron parent process under the desktop native OS, it starts the app process and provides desktop access services to it.
* The electron client (app) process runs under Chromium in a simulated browser environment (isolated from the native OS).

Electron thus allows code written for a browser (HTML + CSS + JavaScript) to be run as a desktop app with the additional capability of desktop filesystem access via communication between the two processes.

Both processes run Javascript under Node.

The `src/Main/Main.fs` source configures electron start-up and is boilerplate. It is transpiled to the root project directory so it can be automatically picked up by Electron.

The remaining app code is arranged in four different sections, each being a separate F# project. This separation allows all the non-web-based code (which can equally be run and tested under .Net) to be run and tested under F# directly in addition to being transpiled and run under Electron.

The project relies on the draw2d JavaScript (node) library, which is extended to support digital electronics components. The extensions are in the `draw2d` sub-folder of the renderer project source files. 

The code that turns the F# project source into `renderer.js` is the FABLE compiler followed by the Node Webpack bundler that combines multiple Javascript files into a single `renderer.js`.

The compile process is controlled by the `.fsproj` files (defining the F# source) and `webpack.additions.main.js`, `webpack.additions.renderer.js`
which define how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put. 
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

### `Docs` folder

Contains source information copied (or compiled) into the `docs` directory that controls the project 
[Github Pages](https://pages.github.com/) website, with url [https://tomcl.github.io/issie/](https://tomcl.github.io/issie/).

## Project versus File in the Issie application

Issie allows the users to create projects and files within those projects. A Issie project is simply a folder named `<project-name>` that contains an empty file named `<project_name>.dprj` (dprj stands for diagram project). The project folder any non-zero number of design files, each named `<component_name>.dgm` (dgm stands for diagram). each deisgn file represents one design sheet of a hierarchical hardware design, sheets can contain, as components, other sheets.

When opening a project, Issie will initially search the given repository for `.dgm` files, parse and load their content, and allow the user to open them in Issie or use them as components in other designs.

## Build Magic

This project uses modern F# / dotnet cross-platform build. The build process does not normally concern a developer, but here is an overview for if it needs to be adjusted.

* Before anything can be built Dotnet & Node.js are manually be (globally) installed. Dotnet includes the `paket` tool which will manage other dotnet-related dependencies. Node.js includes `npm` which will do the same for Node-related dependencies. NB - there are other popular packet managers for Node, e.g. Yarn. They do not mix with npm, so make sure you do not use them. Confusingly, they will sort-of work, but cause install incompatibilities.
  * Dotnet dependencies are executable programs or libraries that run under dotnet and are written in C#'. F#, etc.
  * Node dependencies are (always) Javascript modules which run under node.
* Initially (the first time `build.cmd` is run) the build tools categorised in `dotnet-tools.json` are installed by `dotnet tool restore`.
   * fake (with the F# compiler) 
   * fable
* Next all the project Dotnet dependencies (`paket.dependencies` for the whole project, selected from by the `paket.references` in each project directory, are loaded by the `paket` packet manager.
* Finally fake runs `build.fsx` (this is platform-independent) which uses `npm` to install all the node (Javascript) dependencies listed in `package.json`. That includes tools like webpack and electron, which run under node, as well as the node libraries that will be used by needed by the running electron app, including electron itself. These are all loaded by the `npm` packet manager. 

## Getting Started

If you just want to run the app go to the [releases page](https://github.com/tomcl/issie/releases) and
download and run the latest prebuilt binary for your platform (Windows or Macos). Issie will require in total about 200M of disk space.

* Windows: unzip \*.zip anywhere and double-click the top-level Issie.exe application un the unzipped files.
* Macos: Double click the dmg file  and run the application inside the folder, or drag and drop this to install.
    * The binaries are not signed. You will need to [perform a one-off security bypass](https://www.wikihow.com/Install-Software-from-Unsigned-Developers-on-a-Mac).

###

Issie installs and runs without making system changes - all of its code is inside the directory you download. You can delete this and replace it by a later version of Issie. Each design sheet is stored in a similarly named file under the porject directory. The subdirectory `backup` there contains a large numbers of backup snapshots for design recovery. These are not needed for Issie operation so you can delete them - or even the whole `backup` directory, if you wish.

## Getting Started as Developer

If you want to get started as a developer, follow these steps.

### Development Install Prerequisites

Download and install (if you already have these tools installed just check the version constraints).

* [.Net 6 SDK](https://dotnet.microsoft.com/download/dotnet/6.0).  Version >= 6.0
* [Node.js v12](https://nodejs.org/dist/latest-v12.x/). **Version > 12**
    * Node.js includes the `npm` package manager, so this does not need to be installed separately.
    * The lastest LTS version of Node is now v14. That will currently NOT work.
* (recommended) Visual Studio 2022 which includes F# 6.0
* (recommended) install [hyper.js](https://hyper.is/) to have a good command line interface - anything else you like ywill do.

### Issie Development

1. Download & unzip the [Issie repo](https://github.com/tomcl/ISSIE), or if contributing clone it locally, or fork it on github and then clone it locally. Make sure you are contributing to the Issie repo - not the Issie parent repo, if cloning (Github desktop gives you this option when you clone).

3. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience 
that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). 
That makes things a lot more pleasant. The new [Windows Terminal](https://github.com/microsoft/terminal) works well.

4. Run `build.cmd` under Windows or `build.sh` under linux or macos. This will download and install all dependencies then launch the application with HMR.
  
  * HMR: the application will automatically recompile and update while running if you save updated source files
  * To initialise and reload: `File -> reload page`
  * To exit: after you exit the application the auto-compile script will terminate after about 15s
  * To recompile the application `npm run dev` or `npm run devfast` (devfast switches off some debugging to make simulation run a lot faster).
  * To generate distributable binaries for dev host system `npm run dist`.
  * If you have changed node modules use `build dev`. Note that this project uses npm, not yarn. If npm gets stuck use `build cleannode` and try again.
  * From time to time run `build killzombies` to terminate orphan node and dotnet processes which accumulate using this dev chain. (Not sure if this is still needed)

#### Development on Macos

In theory the build should work equally well on macos. Practically that is now (10/22) the case. Having installed the normal prerequisites, and Visual Studio for Mac, which itself has the F# compiler, the one-off setup can be done manually from the various build steps needed:

* git clone to local project directory as normal (with github desktop or command line git - one off)
* dotnet tool restore  (build tools - one off)
* paket install (install dotnet packages one off)
* npm install (install node packages - one off)
* npm run dev (run the dev envt) 


One unresolved issue that can occur on Macs is file permission problems. Best practice is for all installation and dev to run under the current (non-admin) user. If any part of the necessary downloaded development files gets written as root then subsequent development commands that modify it will need to be executed using sudo.

```
sudo npm run devfast
```

If possible, try to avoid this, but if necessary it can be done. Probably the better solution is to investigate properly which install steps introduce these root owner files, change the file ownership back to current user with `chown -R <username> <directory>`. Please document any progress made with mac builds (detailing which mac OS) on an issue.


## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) rerun `build.cmd` (Windows) or `build.sh` (Linux and MacOS). You may need first to
run `build killzombies` to remove orphan processes that lock build files.

## TODO


* Should Node be upgraded to v14?
* Make source map support work more consistently (sourceroot option on compiler?)
* Clean up Paket dependencies

