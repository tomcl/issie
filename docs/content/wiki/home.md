## Welcome to the Issie wiki!

This Wiki is designed for those who wish to patch / update or fork the Issie code. 

Issie is written mostly in F#, transpiled to Javascript and run under the electron / node framework which allows applications to be constructed from web pages using the Node ecosystem.

IMPORTANT WARNING - the libraries used here are Node / electron libraries, not .Net F# libraries. See [FABLE compatibility](https://fable.io/docs/dotnet/compatibility.html) documentation for which F# libraries have been ported to FABLE and therefore can be used. Core (functional) F# is very well supported, .Net methods less well. Nevertheless to develop F# you need to know F#, javascript is not required.

Here is the source documentation on technology stack used, and the code structure. All of these documents are comprehensive introductory references. However, you don't need all of this (or even any of it) to make changes to the code base.

* [F# docs](fsharp.org) for F#
* [FABLE docs](fable.io) for the (few) differences between FABLE F# and .NET F#, and for details of how FABLE F# interoperates with Javascript.
* [Elmish docs](https://elmish.github.io/elmish/) for an introduction to Elmish Model-View -Update (MVU) web pages.
* [Electron docs](electronjs.org) For electron API documentation - e.g. all the native dialog and file i/o commands via Node.
  * [Fable Electron](https://github.com/fable-compiler/fable-electron) contains the API bindings for Electron that provide an F# typed interface to Electron and Node APIs (much much easier than using dynamic Javascript - though this is also possible if needed).
* [Marco Selvatici's dissertation for a detailed introduction to Issie code](https://github.com/tomcl/issie/blob/master/docsrc/files/marco-report.pdf)

Pages on this Wiki document various detailed information that developers might need to make sense of some of the finer issues in the code base. Most of this is also available in the above extensive introductory documentation.

