# Fable.Elmish.React Template

This template can be used to generate a simple web app with [Fable](http://fable.io/) and [Elmish](https://fable-elmish.github.io/).
You can find more templates by searching `Fable.Template` packages in [Nuget](https://www.nuget.org).

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0.0 or higher
* [node.js](https://nodejs.org) 4.8.2 or higher
* npm5: JS package manager

Although is not a Fable requirement, on macOS and Linux you'll need [Mono](http://www.mono-project.com/) for other F# tooling like Paket or editor support.

## Editor

The project can be used by editors compatible with the new .fsproj format, like VS Code + [Ionide](http://ionide.io/), Emacs with [fsharp-mode](https://github.com/fsharp/emacs-fsharp-mode) or [Rider](https://www.jetbrains.com/rider/). **Visual Studio for Mac** is also compatible but in the current version the package auto-restore function conflicts with Paket so you need to disable it: `Preferences > Nuget > General`.

## Installing the template

In a terminal, run `dotnet new -i Fable.Template.Elmish.React::*` to install or update the template to latest version.

> In some shells you many need quotations: `dotnet new -i "Fable.Template.Elmish.React::*"`. If you use the dotnet core SDK version 2 or higher, you should only need to type `dotnet new -i Fable.Template.Elmish.React`.

## Creating a new project with the template

In a terminal, run `dotnet new fable-elmish-react` to create a project in the current directory. Type `dotnet new fable-elmish-react -n awesome` instead to create a subfolder named `awesome` and put the new project there.

> The project will have the name of the directory. You may get some issues if the directory name contains some special characters like hyphens

## Building and running the app

* Install JS dependencies: `npm install`
* `npm start` (or `npx webpack-dev-server` to run manually)
* In your browser, open: http://localhost:8080/

If you are using VS Code + [Ionide](http://ionide.io/), you can also use the key combination: Ctrl+Shift+B (Cmd+Shift+B on macOS) instead of typing the `npx webpack-dev-server` command. This also has the advantage that Fable-specific errors will be highlighted in the editor along with other F# errors.

Any modification you do to the F# code will be reflected in the web page after saving. When you want to output the JS code to disk, run `npx webpack` and you'll get all the files minified in `deploy` folder.

## Project structure

### Paket

[Paket](https://fsprojects.github.io/Paket/) is the package manager used for F# dependencies. It doesn't need a global installation, the binary is included in the `.paket` folder. Other Paket related files are:

- **paket.dependencies**: contains all the dependencies in the repository.
- **paket.references**: there should be one such a file next to each `.fsproj` file.
- **paket.lock**: automatically generated, but should committed to source control, [see why](https://fsprojects.github.io/Paket/faq.html#Why-should-I-commit-the-lock-file).
- **Nuget.Config**: prevents conflicts with Paket in machines with some Nuget configuration.

> Paket dependencies will be installed in the `packages` directory. See [Paket website](https://fsprojects.github.io/Paket/) for more info.

### npm

- **package.json**: contains the JS dependencies together with other info, like development scripts.
- **package-lock.json**: is the lock file created by npm5.

> JS dependencies will be installed in `node_modules`. See [npm](https://www.npmjs.com/) website for more info.

### Webpack

[Webpack](https://webpack.js.org) is a bundler, which links different JS sources into a single file making deployment much easier. It also offers other features, like a static dev server that can automatically refresh the browser upon changes in your code or a minifier for production release. Fable interacts with Webpack through the `fable-loader`.

- **webpack.config.js**: is the configuration file for Webpack. It allows you to set many things: like the path of the bundle, the port for the development server or [Babel](https://babeljs.io/) options. See [Webpack website](https://webpack.js.org) for more info.

### F# source files

The template only contains two F# source files: the project (.fsproj) and a source file (.fs) in `src` folder.
