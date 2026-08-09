# Issie — instructions for coding agents

Issie (Interactive Schematic Simulator with Integrated Editor) is a digital circuit design and
simulation application written in F#, transpiled to JavaScript by Fable and run under Electron.

**Read [CLAUDE.md](../CLAUDE.md) before changing anything.** It is the single guide for this
repository and is kept current: how to build, run and test; the conventions that differ from F#
defaults (optics rather than record-copy syntax, strict immutability, `Option`/`Result` rather
than nulls); the things the code will not tell you; and the gotchas that cost time.

[AGENTS.md](../AGENTS.md) summarises the same ground for tools that look for that filename. Where
the two disagree, CLAUDE.md is right.

Do not duplicate either of them here. Three overlapping guides drift, and the stale one is always
the one that gets read.
