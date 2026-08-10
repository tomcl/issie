# Review of the documentation and code

A read of every markdown file in the repository against the code it describes, plus a pass over the
build scripts, the CI workflows and the test harness. Written 2026-08-09 against `1b9a104ec`.

Everything below was checked rather than inferred: file and line references are what was read, and
the counts come from running the thing. Where a claim in the docs turned out to be right it is not
listed — this is a defect list, and the state of the documentation is much better than its length
suggests.

Everything the review found that was *wrong* — thirteen priority-1 items, from a `clean: true` that
could package an app with no renderer in it down to a misspelling — and every smaller code defect
alongside them were fixed on 2026-08-09, and have been deleted per the rule below. What remains,
and what this page is now, is the gaps: work that is absent rather than incorrect.

Method, so that a re-run is possible: `npm run test` (396 pass, 54s here) and `CI=true npm run test`
(385 pass, 21s here); all relative links in every `.md` resolved against the filesystem; every
`` `path/to/file.ext` `` in the docs resolved; the Verilog corpus counted; module-level `let mutable`
enumerated across `src/`; the generated site in `output/` inspected for how each page lands.

Fix an item and delete it from this page. A review that keeps its history stops being read — the
same rule [openIssues.md](openIssues.md) is kept under.

---

## The remaining gaps

Things that are absent rather than wrong.

**1. No test reaches the waveform simulator, the truth-table UI, the Elmish update loop, or wire
routing beyond the simple cases.** `Tests/README.md:150-151` states this plainly, which is right,
but it has stayed stated for a while. Of the four, wire routing is the one with a ready-made route
in: `DrawBlockTests.fs` already builds symbols and routes wires under plain .NET, and
`BusWireSeparate.fs:961` carries a TODO admitting the separation pass does not check its own result
("include a comprehensive check for any remaining overlapping wires after this - and fix them"). A
test that asserts no two wires overlap after separation would be one assertion against the property
the module exists to provide.

**2. 96 `TODO`/`FIXME` comments in `src/`, tracked nowhere.** `openIssues.md` is a deliberately
curated list of rough edges; the TODOs are the untriaged remainder, and several are substantive
rather than cosmetic — `EvalAlgebraic.fs:25` ("the isClockedReduction = true code below under
asyncRAM should be deleted. It is not used"), `FastExtract.fs:32` ("future steps are not rerun
(perhaps they should be!)"), `CommonTypes.fs:1098`. Worth one pass that promotes the real ones into
`openIssues.md` and deletes the rest, then a note in `AGENTS.md` that a new TODO goes in
`openIssues.md` instead.

**3. Nothing stops `docs/mutableState.md`'s audit going stale again.** It was brought back in step
with the source on 2026-08-09 — four module-level mutables had accumulated that no table listed, one
of which (`Main.fs` `appStarted`) cites that very document as its justification. Bringing it up to
date fixes today only. `Tests/Issie.Tests/SourceHygiene.fs` is the precedent for the durable fix: a
test that enumerates module-level `let mutable` across `src/` and fails when one appears that the
document does not name. The audit is the document's whole value, and a stale audit is worse than
none, because the next person adds one and assumes the list is complete.

**4. Developer pages have no fsdocs frontmatter, so the published site files them under "Other".**
`docs/BUILD_OPTIMIZATION.md`, `docs/mutableState.md` and all six `docs/dev/*.md` (this page
included) start with a heading rather than a `---` block. Every other page in `docs/` has `title`,
`category`, `categoryindex` and `index`. The generated navigation puts the frontmatter-less pages in
an unnamed group after the user documentation, in arbitrary order, on the site students read. Give
them `category: Developer` and an index; `docs/index.md:55` already links one of them from the front
page, so they are meant to be published.

**5. `Tests/Issie.Tests/SourceHygiene.fs` allowlists by file name, not path.** `allowed` holds bare
names (`"Main.fs"`, `"TimeHelpers.fs"`, …) and the offender filter compares `Path.GetFileName`. Any
future `src/**/Main.fs` silently inherits permission to print — and the "every allowlisted file
exists" test that guards against a stale allowlist matches by name too, so it would not notice.
Compare against the path relative to `src/`. Separately, `console.log` is matched only by the
*commented-out* pattern; a live `JS.console.log` in renderer code passes, which is the gap the module
exists to close.

**6. `README.md:174-176` lists three pinned tools; `dotnet-tools.json` pins five.** `fsdocs-tool`
and `fantomas` are missing from the list. `fantomas` matters because `AGENTS.md:60-62` tells people
to run it.

**7. Two sources of truth for coding guidelines.** `README.md:196` and `docs/gettingStarted.md:30-34`
send contributors to the GitHub wiki for coding guidelines and a code overview, as does
`CONTRIBUTING.md`. `CLAUDE.md` and `AGENTS.md` say they are the guide and are kept current, and
`.github/copilot-instructions.md` explicitly warns that overlapping guides drift. The wiki page
itself is real and substantial (`wiki/1---Coding-guidelines-for-ISSIE`; the un-numbered URL
`CONTRIBUTING.md` used to carry silently landed on the wiki Home instead, and was corrected). So
this is a structural decision, not a broken link: decide which is authoritative and make the other
say so.

**8. Test counts and timings are hand-copied into five files.** 396 / 385 / "about a hundred
seconds" / "~26s" appear in `README.md`, `AGENTS.md`, `CLAUDE.md`, `Tests/README.md` and
`CONTRIBUTING.md`. The counts are correct today (verified: 396 and 385, both green). The timings are
machine-dependent — 54s and 21s here — and five places is four too many to keep true. Keep the
numbers in `Tests/README.md` and have the others link to it.

**9. Orphan assets.** `docs/pdf/marco-poster.pdf` and `docs/img/homePage/verilogOutput.png` are
referenced by nothing. `LICENSE` and `LICENSE.md` are byte-identical duplicates of 35KB
(`README.md:324` links the second). All three are one deletion each.

---

## What is in good shape

So that the list above is read for what it is. `docs/dev/simulatorStructure.md` and
`docs/dev/verilogTesting.md` are unusually good: they name the debt, quantify it, and say what a fix
would cost. Every relative link in every markdown file resolves. Every code path cited in the docs
exists. The Verilog corpus figures, the demo count, the test group table in `Tests/README.md` and
its per-group breakdown all check out — the group counts sum to exactly 396, and both suite counts
were confirmed by running them. `scripts/clean-dev.js`, `scripts/app.js` and
`scripts/refresh-stale-output.js` are careful, and each carries a comment saying which failure it
exists to prevent. `openIssues.md`'s entries were spot-checked against the code
(`ParameterAnalysis.fs:566`'s `Set.minElement`, the unvalidated `pathJoin [| root; libName |]` in
`MiscMenuView.fs:825`, `FilesIO.modifiedTimeMs` having no callers) and every one held.

The masking invariant the docs make so much of holds where it was checked: `EvalCompiled.maskOf`
special-cases width 32, and the ROM tabulation is gated on `maxRomTableAddressWidth` before
`1 <<< AddressWidth` can overflow.

---

## Suggested order

1. Items 1-3, the coverage gaps. Each is a small piece of work with a clear finish, and each leaves
   behind a check rather than a paragraph.
2. Items 4-9, the documentation-structure gaps.
