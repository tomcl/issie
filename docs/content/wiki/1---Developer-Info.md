# Debugging Issie

Issie is written as pure functional code (nearly always) with the idea that most code should be correct by construction and need no debugging. This is of course not true. The resources for debugging Issie are as follows:

* Run Issie with `npm run dev` or `npm run debug`. The `debug` script sets `ASSERTS` on in the code. This slows down simulation by a factor (100?). ASSERTS were used in the original Issie but not much since then, so they are of limited use.
* When running Issie as above press `Ctrl/Shift/i` to open the *dev tools window*. Normally you look at the console tab of this, which shows all `printf` output from the running app.
* When part of the app crashes (white window, or something obviously wrong) the exception will be shown in the dev tools console as a red text backtrace. often there are multiple exceptions - always look at the first (highest in window) one.
* To debug an exception:
    * Look at the error message.
    * Go down the red text looking for the *top `.fs` file reference*. (The `.js` file above this will be unhelpful library code).
    * You can click the reference to go to the relevant F# code in dev tools, and then look up the F# file and line number in your IDE.
    * Exceptions in F# **should not happen**. When they do in Issie, they are because of unsafe code that has failed, e.g. `Option.get` on `None`, `Map.find` with a none-existent key, array index out of bounds, an incomplete match statement (these should never exist), of a `failwithf` code  generated exception with an error message. You can work out why the code failed and fix it.
* To debug Elmish messages:
    * Switch on  `trace all` on from the Issie files menu.
    * The dev tools console will show Elmish messages (type `Msg`) in the order they are executed. You can see what each message does by finding the reference to the message value in `update.fs`. Since all Issie state change comes from messages the history of the app can be understood in this way.
* Further debugging:
   * Add print statements to the code `printf "%A"  x` will print any type x. `printf "%dA" x` will print d characters of a large structure x (the default print size is quite small). NB - if you add debug messages please delete them after debugging is complete, or make them proper messages switched on according to debug level. Unfortunately in Issie we do not have consistent use for logging, so debugging prints tend to be ad hoc. It is useful to add to all your debug messages a searchable key so you can easily find them and delete them.
   * Improve an existing `failwithf` message by adding data to print.
   * Use in F# debugger, set breakpoints, etc. I do not do this. Is it ever worth it?

# Build Overview

This project uses modern F# / dotnet cross-platform build. The build process does not normally concern a developer, but here is an overview for if it needs to be adjusted. Details are included below under **Getting Started As A Developer**.

* Before anything can be built Dotnet & Node.js are manually be (globally) installed. Dotnet includes the `paket` tool which will manage other dotnet-related dependencies. Node.js includes `npm` which will do the same for Node-related dependencies. NB - there are other popular packet managers for Node, e.g. Yarn. They do not mix well with npm, so make sure you do not use them. Confusingly, they will sort-of work, but cause occasional install incompatibilities.
  * Dotnet dependencies are executable programs or libraries that run under dotnet and are written in C#. F#, etc.
  * Node dependencies are (always) Javascript modules which run under Node.
* Initially (the first time `build.cmd` is run) the build tools categorised in `dotnet-tools.json` are installed by `dotnet tool restore`.
   * fake (F# build automation tool) 
   * fable (F# to js compiler)
   * paket (alt dotnet package manager to Nuget)
* Next all the project Dotnet dependencies (`paket.dependencies` for the whole project, selected from by the `paket.references` in each project directory, are loaded by the `paket` packet manager.
* Finally fake runs `build.fsx` (this is platform-independent) which uses `npm` to install all the node (Javascript) dependencies listed in `package.json`. That includes tools like webpack and electron, which run under node, as well as the node libraries that will be used by needed by the running electron app, including electron itself. These are all loaded by the `npm` packet manager. 

# Building Issie as an Electron App

1. Issie uses a build for Electron which incorporates Fable Compiler to turn F# `.fs` files into javascript `.fs.js` files that are then used as input to a fairly standard electron build.
2. The build has two forms: development and production
    * Development: the app is run with *Hot Module Reloading" (HMR) so that source code changes can be seen and tested in a running application quickly
    * Production: the javascript files and other assets are packaged with a Chromium browser and published as a zippable directory with a top-level executable file that runs the Electron app.
    * Production (and development) build should run cross-platform, on all of win64, macos, linux. In theory mac M1 (arm64) is also build target although since M1 macs can run old macos apps this is not a requirement.
3. The build takes javascript files defining two separate processes (main and renderer) and bundles them to make single javascript files (`index.js`, `renderer-index.js`). These processes are similar to, and built in the same way, as node web back-end (`main.fs`) or front-end (`renderer.fs`) processes.
4. The initial install for the build has as prerequisites [.Net SDK](https://dotnet.microsoft.com/en-us/download) (latest) and [Node.js](https://nodejs.org/en/download/) (latest LTS), and involves a script that downloads and installs 3 distinct set of items:
    * Dotnet build tools installed using dotnet (`./dotnet-tools.json`)
    * Dotnet packages installed using `paket`  (`./paket.dependencies`)
    * Node packages installed using `npm` (`package.json`)
        * Node packages can either be used in the running javascript app, or *only used* in the build code

### Keeping build logic cross-platform

In order to have an uncomplicated and maintanable build process, all the tools used for the build can run in all 3 main Operating Systems: Windows, MacOs, and Linux. These tools are: [.Net SDK](https://dotnet.microsoft.com/en-us/download) (latest) and [Node.js](https://nodejs.org/en/download/) (latest LTS).

The master repository includes a build script (`build.cmd` for Windows or `build.sh` for MacOS/Linux) which runs the following 3 commands:

1. `dotnet tool restore` &rarr; Installs the `.NET` build tools specified in `./dotnet-tools.json` &rarr; `fable`, `fake`, and `paket`
2. `dotnet paket install` &rarr; Installs all `.NET` packages specified in `./paket.dependencies`
3. `dotnet fake build -t %*` &rarr; Runs the main build script, using `fake`, with the following 5 targets:
   1. `CleanFableJS` &rarr; Deletes all `.fs.js` and `.fs.js.map` files produced by Fable
   2. `CleanDev` &rarr; Deletes all files under the `./dist` directory
   3. `DotnetRestore` &rarr; Runs `dotnet restore issie.sln` which restores the issie solution file
   4. `NpmInstall` &rarr; Runs `npm install` to install all node packages specified in `package.json`
   5. `Dev` &rarr; Runs `npm run dev` which corresponds to the `dev` script of `package.json` (see below) which transpiles the code to JavaScript using `Fable`, bundles the JS files into a single file using `Webpack`, and starts the `Electron` Application under development mode (i.e. with **HMR enabled**) 

The code snippet below includes all the node scripts

```json
"scripts": {
    "clean-dev-mac": "sudo killall -9 node && sudo killall -9 dotnet && sudo killall -9 issie",
    "clean-dev-win": "taskkill /f /im node.exe && taskkill /f /im dotnet.exe && taskkill /f /im issie.exe",
    "compile": "dotnet fable src/Main -s && dotnet fable src/Renderer -s",
    "dev": "dotnet fable watch src/Main -s --run npm run devrenderer",
    "devrenderer": "dotnet fable watch src/Renderer -s --define ASSERTS --run npm run start",
    "start": "cross-env NODE_ENV=development node scripts/start.js",
    "build": "cross-env NODE_ENV=production node scripts/build.js",
    "pack": "npm run compile && npm run build && electron-builder --dir",
    "dist": "npm run compile && npm run build && electron-builder",
    "buildonly": "electron-builder",
    "compile-sass": "cd src/renderer/scss && node-sass main.scss main.css"
}
```

After all tools have been installed, the developer only needs to run `npm run dev` from the terminal of his choise to compile and run ISSIE under development mode.

### Configuring the build

#### Webpack

`Webpack` needs careful configuration so that all `.fs.js` (produced by Fable), `.css` (styling sheets), and `.fs.js.map` (sourcemaps) files are bundled appropriately into a single `index.js` and `renderer-index.js` file. Given the different use of `Main` and `Renderer`, they are configured differently in `./webpack.config.main.js` and `./webpack.config.renderer.js`.

 
#### Development (with HMR) Mode


ISSIE runs under devolpement mode using the `npm run dev` script (see `package.json` scripts above) which runs:
1. The Fable Compiler in `watch` mode (used for HMR)
2. `./scripts/start.js`: Runs the Webpack Bundler and starts a development server on port 9000 using `webpack-dev-server` npm package. As soon as Webpack has bundled all files, `Electron` is started to display the application.  

The Hot-Module-Reloading (HMR) process is:
- Fable under `watch` mode: as soon as it detects a change in a `.fs` file it re-compiles only the specific file (and other affected files by the change), producing new `.fs.js` files
- `Webpack-dev-server`: detects the changes in the `.fs.js` files and re-bundles the changed files without losing the application's state (as it would happen in the case of a full reload). 


#### Production Mode

The ISSIE application (`.exe` or `.dmg`) is produced using the `npm run dist` script (see `package.json` scripts above), which runs:
1. The Fable Compiler: all F# files get transpiled to JavaScript
2. `./scripts/build.js`: Runs webpack to Bundle Main and Renderer to `index.js` and `renderer-index.js`
3. `Electron-builder`: Builds the (platform-specific) distributable in the `./dist` directory       

#### Electron Builder

`Electron-builder` is configured in `package.json` under the `"build"` property where the source of `index.js` and `renderer-index.js`, the name of the application, and some special instructions per OS are specified.
   ```json
    "build": {
    "appId": "ISSIE",
    "files": [
      "build/**/*"
    ],
    "extraFiles": [
      {
        "from": "static",
        "to": "resources/static",
        "filter": [
          "**/*"
        ]
      }
    ],
    "appx": {
      "applicationId": "ISSIE"
    },
    "win": {
      "icon": "public/icon.ico",
      "target": "zip"
    },
    "linux": {
      "icon": "public/icon.png",
      "target": "zip"
    },
    "mac": {
      "icon": "public/icons/icon.icns",
      "target": {
        "target": "default",
        "arch": [
          "x64",
          "arm64"
        ]
      }
    }
   ``` 


### Development Environment

After running `npm run dev`, the Electron App will be launched in Development mode. The "developement app" is identical to the app produced by electron builder (.exe or .dmg) with one difference: access to Development Tools.

By clicking `View` => `Toggle Dev Tools` (or using the shortcut `Ctrl+Shift+I`) the Chromium Developer Tools open on the right-hand side of the app, giving you access to the Console and the DOM element inspector. In the console tab you can see all the messages written (logged) to the console by the `console.log()` Method (or `printfn` in F#). In the elements tab, you can inspect the produced HTML code for the whole app, or for the specific element you are interested in, by using the element selector option.

Source-maps are also configured in ISSIE. A source map is **a file that maps from the transformed source to the original source**, enabling the browser to reconstruct the original source and present the reconstructed original in the debugger. In other words, assuming there is an error in the renderer process, instead of getting an error message directing you somewhere in the 100.000 line `renderer-index.js` bundled file, you get an error message referencing the original `.fs` file, function name, and line where the error is located. Source maps are used for:
   - bundled `renderer-index.js` => original `.fs.js` files
   - `.fs.js` files => f# source files (`.fs`)

### NPM

#### General Information

`npm` is the default package manager for the JavaScript runtime environment Node.js. It consists of a command line client, also called `npm`, and an online database of public and paid-for private packages called the `npm registry`.

#### package.json

`package.json` contains all the information about the ISSIE project, including all the node packages it uses. These are split into two categories:
1. `dependencies`: Packages required by the application in production. 
2. `devDependencies`: Packages that are only needed for local development and testing  

These are examples of `dependencies`:
```json
"webpack-dev-server": "4.7.4",
"bufferutil": "^4.0.3"
```

`4.7.4` and `^4.0.3` represent the versions of the packages. The meaning of the caret operator `^` is the following:
- ^1.2.3 := >=1.2.3-0 <2.0.0-0 "Compatible with 1.2.3". When using caret operators, anything from the specified version (including prerelease) will be supported up to, but not including, the next major version (or its prereleases). 1.5.1 will satisfy ^1.2.3, while 1.2.2 and 2.0.0-beta will not.
- ^0.1.3 := >=0.1.3-0 <0.2.0-0 "Compatible with 0.1.3". 0.x.x versions are special: the first non-zero component indicates potentially breaking changes, meaning the caret operator matches any version with the same first non-zero component starting at the specified version.
- ^0.0.2 := =0.0.2 "Only the version 0.0.2 is considered compatible"

#### Installing Packages

The installation of the node packages specified in `dependencies` and `devDependencies` in your local directory can be done in 2 different ways
1. `npm install`: installs the latest (compatible according to `package.json`) versions of all the packages (e.g. for a package with version `^7.4.3`, if version `7.4.4` is out, it will install `7.4.4`)
2. `npm ci`: installs all packages with the versions specified in the `package-lock.json` file (currently used by the `build.cmd` and `build.sh` scripts)
   - `package-lock.json` file contains all the packages and their versions as they were when the latest commit to master was made. This means (following the previous example) that if the user who made the latest change to master used version `7.4.3`, the same version will be installed to your local directory as well (not the newest `7.4.3`) 


#### Updating Packages

Packages can be updated either by `npm update [package-name]` or by changing the version in the `package.json` file.

### Security

Electron strictly recommends disabling `nodeIntegration` to prevent remote attacks on the user's PC by attackers.

***From the Electron Documentation:*** "A security issue exists whenever you receive code from an untrusted source (e.g. a remote server) and execute it locally. As an example, consider a remote website being displayed inside a default BrowserWindow. If an attacker somehow manages to change said content (either by attacking the source directly, or by sitting between your app and the actual destination), they will be able to execute native code on the user's machine."

However, ISSIE requires access to your local directories in order to save and load the schematics. This is why in ISSIE, `nodeIntegration` is enabled, and all electron warnings are force-disabled by `ELECTRON_DISABLE_SECURITY_WARNINGS`. Despite the warnings and `nodeIntegration` being enabled, because ISSIE only accesses your local files, and does not load remote content (all code and files are either local or packaged together with the application), ISSIE does **not** give access to your system to attackers, and is therefore 100% secure.

# Git Workflow

Developers should, where possible, follow the same Git workflow when contributing to Issie to ensure changes are easy to follow and keep track of. This includes commit messages, the use of pull requests (PRs) and how PRs are merged.

We *try* follow the rebase-and-squash style suggested by github:
https://github.com/MakingSense/development-guidelines/blob/main/git-workflow/README.md

NB - if you are working on a feature with a lot of independent parts you may want to do less squashing. 
* The rationale for squash is when you have a lot of commits it makes finding a specific change more difficult. In addition (see below) troublesome commits that resolve merge conflicts can be seen easily if features are always squashed down to one commit and then added.
* The rationale for not squashing is that if a commit contains different things it is less easy to identify and correct them when there is a problem.



### Contributing
Developers should work off either a private branch, or a fork of the Issie repo. Most developers will not have write access to the Issie repo and will therefore need to use a fork. In either case, developers should create a branch off of `master`. It is suggested that the branch be named according to the feature that the developer is implementing.

Each branch should be used only for the development of a single feature: once this feature is complete, a PR should be used to integrate this change into the `master` branch, the development branch deleted, and a new branch created for the next feature.

It is recommended that while working on new features, development branches are regularly rebased onto the `master` branch. This will ensure that the branch stays up-to-date with new code, making it easier to integrate new features with the `master` version of Issie.

### Commits

Commits should reflect the progress of a feature: think of a commit as a milestone in the implementation of a new feature.

Each commit should have a concise and informative message that states what the commit should do.

A useful explanation of what makes a good commit message can be found [here](https://reflectoring.io/meaningful-commit-messages/).

In particular, the following points:
* Commits should be atomic (i.e. one commit per change).
* Messages should be short
* Use of imperative present tense.

### Pull Requests

The preference for Pull Requests is that they are rebased. Prior to opening a new PR, the development branch should first be rebased on top of the `master` branch. During the rebase, developers should use `squash` and `fixup` to remove any unnecessary commits from the history. This will make it easier for future developers to read through history.

Once a Pull Request has been opened, reviewed, and can be merged into `master`, the PR should be **rebased**, rather than merged or squashed. The use of rebase preserves a linear history so it is easier to understand how Issie has been developed, and also does not 'pollute' the commit history with merge commits. The use of rebase over squash preserves the commit history of large changes in a way that makes it easier to see how these changes were implemented. Squashing does not preserve this history, and reduces large changes down to a single commit.

It is accepted that the use of rebase instead of merge is more complicated, and that many Issie developers will not be familiar with the practice, especially given that Imperial EE does not teach students how to use Git properly. That being said, learning how to rebase is a very valuable skill, and is commonly used in industry.

An interactive rebase (`git rebase -i master) can be used to see how a git history will be changed, as well as allow the developer to choose which commits should be picked, dropped, squashed, or fixed up, as well as allow commits to be reordered.

Finally, following a rebase, a `force push` will be required, since a rebase rewrites history. Developers should use `git push --force-with-lease --force-if-includes` when pushing work upstream. This provides some protection from overwriting other people's work when using a force push.

For a good guide on merging vs rebasing, see [here](https://www.atlassian.com/git/tutorials/merging-vs-rebasing).

### Code Review

In an ideal world, all PRs would be small (think one PR per feature). This makes PRs easier to review, and for changes to be focused. However, Issie is a special case: most of the developers are students, many of whom will be working on Issie for their Final Year Project. FYP students need to demo their projects; because of the need to have a good project, it is easier to gain credit for analysis, evaluation, and features, rather than for good code. Therefore, requiring industry-standard code from FYP students is unreasonable.

Therefore, this section is less strict on the processes developers use, though where possible, they should be followed:
* PRs should be small and focused: one PR per feature.
* Code should be reviewed by the developer who wrote the code prior to the PR being opened.
* Code should _ideally_ be reviewed by another developer prior to being merged. This may not be possible in all cases.
* Code should follow the [Issie Coding Guidelines](https://github.com/tomcl/issie/wiki/Coding-guidelines-for-ISSIE).


# Adding an Issie Catalog Component

### Introduction

Adding a new component is now a very easy process because of the powerful `Ionide` IDE. All you need to do is add your component in the `Common/CommonTypes.fs` file under the `type ComponentType` definition and in the same file add it to `JSONComponent.ComponentTypes` as well (in the `module JSONComponent`).

`Ionide` will then identify all the incomplete pattern matches (when matching `ComponentType`) and guide you to the files that require changes. 

Apart from the incomplete pattern matches, you must also create the component's creation button and popup, which is done in the `UI/CatalogueView.fs` file.

Another thing not obvious from the pattern match warnings is that you need to edit
some simulation functions as well (see below for exact procedure).

Last but not least, you can customise your symbol in `DrawBlock/SymbolView.fs` by speicifying the `points` and `additions` and giving details for your component in the following functions:
1. `getPrefix`
2. `portNames`
3. `getComponentLegend`

Ionide won't identify a missing pattern in the aforementioned cases because the wildcard pattern `_` is used in them.

### Step-By-Step

The full procedure will be analysed here following this format:
```
Directory
- File
  - function1
  - function2
```

#### Common

- CommonTypes.fs
  - `type ComponentType` (add the new component)
  - `type ComponentType` in `module JSONComponent` (add same component)
- WidthInferer.fs
  - `calculateOutputPortsWidth` function


#### DrawBlock

- Symbol.fs
  - `getPrefix` function
  - `getComponentLegend` function
  - `getComponentProperties` function
  - `getSymbolColour` (if you don't want the default yellow colour)
- SymbolView.fs
  - `points`, `additions` (variables inside the `drawSymbol` function)

#### Simulator

- CanvasStateAnalyser.fs
  - `portNames` function
- Builder.fs
  - `getDefaultState` function
- FastCreate.fs
  - `getPortNumbers` function
  - `findBigIntState` function
  - `addComponentWaveDrivers` function
- FastReduce.fs
  - `fastReduce` function
  - `fastReduceFData`
- SynchronousUtils.fs
  - `hasSynchronousComponents` function
  - `couldBeSynchronousComponent` function
- Verilog.fs
  - `getVerilogComponent` function

#### UI

- CatalogueView.fs
  - add component's button (HTML) (in `viewCatalogue` function)
  - create Popup function
- SelectedComponentView.fs
  - `makeDescription` function
  - `makeExtraInfo` for adding a custom properties tab (use configuration functions like `makeNumberOfBitsField` or write your own)
- WaveSim/WaveSimHelpers.fs
  - `getCompDetails` function
  - `getCompGroup` function
- WaveSim/WaveSimSelect.fs
  - `getInputPortName` function
  - `getInputName` function
  - `getOutputName` function
  - `getOutputPortName` function