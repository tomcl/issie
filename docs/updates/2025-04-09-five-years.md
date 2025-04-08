---
title:  Five years of Issie development: the good, the bad, and the ugly
date:   2025-04-09 01:00:01 +0100
category: Updates
index: 6
---
# Five years of Issie development: the good, the bad, and the ugly


Issie development over 5 years has added roughly 10K lines of code per year. It has also required some refactoring to reduce technical debt where new features break the old structure. What are the merits and demerits of Issie's design as seen from a maintainer's perspective?

## Elmish Model-View-Update

Nearly all of the Issie UI runs using a strict MVU architecture in which a single global model record (with many sub-records) holds all persistent state. The Elmish framework uses a React virtual DOM and allows global state to be distributed using react components. *Issie does not do this*. Performance is obtained by caching parts of the View that do not change in a given update. Because we write pure functional code this is very easily done by comparing function inputs and not evaluating functions if they don't change.

This paradigm works very well. Different parts of Issie code do not have unexpected interactions because the use different parts of the state, and where they do change state what they do is transparent. As a result Issie development is robust. Features can safely be added, or code refactored, looking only at a small part of the code base and not understanding the rest. This is the expected result of functional programming but it was not obvious it would work well in a large application.

Originally we designed the schematic editor as [three nested blocks](https://github.com/tomcl/issie/wiki/Draw%E2%80%90Block) dealing with sheet, wire, and symbol-level operations respectively. These almost work like react components and we tried to hide information so that for example symbol messages could only operate on symbol state. That worked well in general, but badly in a few specific cases. We have moved to a system with escape hatched that allows but discourages each block's code from sending messages that change a higher-level block's state.

## F#'s Enforced Layering of Software: no forward references

F# is an opinionated language and the compiler originally had  a strict rule not allowing forward references. For F# 7 that was relaxed a bit, so that a module could be tagged recursive and have arbitrary forward references. The restriction between modules of no forward references remains.

We generally found that code naturally layered so that this restriction was no problem. Because we have a near-strict functional programming paradigm it is easy to work out module boundaries.  We had one case - the waveform simulator, where the code as originally written was in a recursive module and seemed impossible to separate into layers. It was also extremely difficult to work with. A major refactoring of code (done in several stages) together with a reworking of the use of global state split this into 11 separate modules, none of which are recursive. The new code is much easier to work with than the old. So this is a case example of why *writing* code without forward references on a large scale correlates with more maintainable code.

Issie currently has 4 *recursive* modules, all containing types and little else. It in general is impossible to avoid types that depend in a circular fashion on other types because message types  must reference implementation types, and (sometimes) implementation types must reference message types.

## Functional Programming, Mutable data, and Recursion

We use guidelines for F# that discourage mutable data more than is typical in the language. For example, we do not use list comprehensions and instead use `List.map` and so on. Very occasionally there are examples where an unbounded while loop is needed and implementing this through recursion seems artificial. But much more often we find that programmers need help to see that a declarative solution to a coding problem is possible and more readable than something using sequence. Even when forced not to use mutable variables programmer will use linear recursion or folding - both of which effectively perform sequential update - unnecessarily.

From our experience teaching programmers NOT to do this unnecessarily pays dividends.

Students find it surprising that they learn about recursion as the only way to implement iterative operations with immutable data, but then find in the Issie code base that there is very little recursion. Indeed recursion is officially a *code smell* in F#, and experience coding Issie has taught me this is true. Even so mutable variables are except in very special cases a worse sin than recursion.

For an example, given a collection of standard programming challenge problems involving lists or maps, classify solutions (all using immutable data) as follows:

1. Using neither linear recursion nor fold
2. Using linear recursion but not fold
3. Using neither linear recursion nor fold.

In general solutions of type 3 should be preferred over 2, and 2 preferred over 1. It is quite surprising how unnatural it is for programmers brought up with procedural programming to see a much simpler solution of type 3 instead of one of type 2 or 1. Partly this is unfamiliarity with available standard collection library functions. Partly it is that they automatically convert problems into procedural solutions.

A great example. 

```
/// Given a list of non-duplicate integers, return a list of lists of consecutive integers containing the same values.
/// You may assume the input list is sorted in ascending numerical order.
/// Example: [1;2;3;5;6;7;10] -> [[1;2;3];[5;6;7];[10]]
let consecutiveLists (lst: int list) : int list list =
    failwithf "not implemented"
```

Programmers are told to prefer solutions of type 3 over 2 over 1, and not allowed to use mutable variables. Even then only one in ten, without training to think declaratively, will solve this problem in the most natural, and easily simplest, way:

```
let declarativeConsecutiveLists (lst: int list) : int list list =
    List.indexed lst // add positions in list
    |> List.groupBy (fun (index,number) -> number - index) // consecutive integer sub-lists are defined by number - index being a constant
    |> List.map (fun (grp, grpLst) -> grpLst |> List.map snd)
```

Showing a class of programmers many such problems, asking them to each generate pencil and paper approximate solutions, and then comparing these, is a great way to reprogram brains into thinking declaratively when that is appropriate. The point being that *when* declarative solutions exist they are nearly always more readable than more implementation-focussed iterative, or iterative-equivalent using fold or recursion, solutions.

## Using HTML and React for a GUI with Electron & FABLE.

We have had a good experience using [Electron](https://www.electronjs.org/) to write a desktop application as effectively a single-page front-end coded web app, even though our programmers are often inexperienced in web development. The code is all written in F#, not Javascript, and transpiled to javascript by [FABLE](https://fable.io/). When developing Issie it is never necessary to think about javascript - and the necessary electron API and html / CSS mostly can be written by someone with little front-end experience benefitting from a strongly types DSL.

HTML/CSS at its worst, when demanding UI behaviour is needed, has the full complexity of front-end development. But it allows complex responsive UIs that solve every problem, and the complexity is only needed when circumstances require it. Most of the time things are simple. The schematic editor uses Model-View-Update SVG throughout and this is a good solution with excellent performance and simple implementations.

We prefer to write everything in F# and minimise the dependence on external npm ecosystem packages: although electron has a lot of unavoidable dependencies.

The most unpleasant part of the technical stack for us is React. Its merit is that as a very well-optimised virtual DOM it allows us to run a big application as MVU. The partial DOM updates are optimised. We do not use the complexity of React (life cycle etc) at all. We do not even think about react, the View function implements HTML. Update events are generated from HTML event listeners. So the complexity of React is nearly all hidden.

We had last year a very unpleasant experience with an unexplained memory leak. We normally just can't get memory leaks, a functional style of programming makes that impossible, unless the Model contains unbounded data structures. In the end we traced this to an undocumented [React 17 bug](https://github.com/tomcl/issie/issues/463) in not garbage collecting `Ref` hook references. This was fixed in React 18 with the fix never back-ported to React 17.

FABLE is outstanding technology. We have never had problems with it, and with many others thank Alfonso Garcia-Caro.

Developing the F# simulator has been challenging because we want both time and space performance. Understanding this fully requires knowledge of:

* F# compiler optiomisations
* FABLE translation
* Chromium JVM optimisations

Chromium (the JVM forked from the Chrome browser and used by electron) developers made a decision a few years ago to use compressed (32 bit) pointers in the heap. That limits the heap size to 4GB and practically, because of multiple space garbage collection, more like 2GB. This limitation is regrettable but baked into Chromium, for typical use cases the time advantage of pointer compression far outweighs the disadvantage of a space limitation. We needed to implement simulation data storage using JS numeric typed arrays that are not stored in the heap by Chromium (the array elements need not be garbage collected) and therefore can be larger than 4GB. Doing this from F# is in fact quite straightfoward because numeric arrays in F# are implemented as typed numeric arrays in Javascript: but it requires some care. If interested see [Yujie's technical Report](https://tomcl.github.io/issie/pdf/1714652_Rpt_A%20high%20performance%20digital%20circuit%20simulator%20for%20ISSIE.pdf).

## Overall

The technical stack we use is unusually complex. To build Issie we need both .Net and npm package management. The build boilerplate is quite unpleasant. However, now it is sorted out, it has proven to be very reliable and its complexity is irrelevant for developers. Electron itself does a very good job of making the generated binaries cross-platform. We run Issie on Windows, Macos, and Linux. Macos has become increasingly restrictive when running unsigned binaries so now, to avoid very unpleasant user setup, we need a paid developer's license to sign the Macos binaries.

Now we spend much less time on boilerplate and tooling issues than we did at the start. FABLE has got easier to use, F# has become much more uniform cross-platform and tools now install quickly without hassle. We can spend more time on productive coding, which usually, writing code in F#, is a very pleasant experience.

Relatively inexperienced programmers have been able to make significant contributions to Issie. The paradigm we use avoids many of the bear traps of bad coding. Still we find it is best to train Issie programmers explicitly in how to write productive code in a declarative style with proper use of names: it is *possible* to write bad code in any paradigm!

For me Issie has meant innovating a wide range of complex algorithms, exploring many different technologies, and discovering what matters when writing a "no user manual should be required" user interface. All of these have been a great personal pleasure. That I am able to share this with many students is an special joy.