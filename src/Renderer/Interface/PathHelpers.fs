/// Pure path arithmetic, shared by the Fable and .NET builds.
///
/// These were node's `path` under Fable and System.IO.Path under .NET - two implementations that
/// already disagreed (node's join normalises, Path.Join does not), and one of which disappears with
/// contextIsolation since `path` is a node module the renderer will not be able to require.
///
/// Joining strings is not a privileged operation and has no business crossing a process boundary, so
/// rather than becoming ten more bridge calls at 189 call sites this is one implementation for both
/// targets. It follows node's semantics, which is what the app was built against, and
/// Tests/Issie.Tests/PathHelperTests.fs holds it to them.
///
/// Separator handling is platform-dependent in the way node's is: on Windows both `/` and `\` divide
/// segments and output uses `\`; on POSIX only `/` divides and a backslash is an ordinary filename
/// character. The leading root - a drive, a UNC share, or `/` - is found but never rewritten by
/// dirname and basename, because node does not rewrite it either, and UNC paths matter here: Issie
/// is run from networked locations on cluster machines.
module PathHelpers

let private isWin = Bridge.isWindows

/// The separator this platform writes.
let sepChar = if isWin then '\\' else '/'

let private sepStr = string sepChar

/// The separators this platform reads. On POSIX a backslash is a legal filename character.
let private splitChars = if isWin then [| '/'; '\\' |] else [| '/' |]

let isSeparator (c: char) = c = '/' || (isWin && c = '\\')

let rec private skipSeps (p: string) (i: int) =
    if i < p.Length && isSeparator p[i] then skipSeps p (i + 1) else i

let rec private untilSep (p: string) (i: int) =
    if i < p.Length && not (isSeparator p[i]) then untilSep p (i + 1) else i

/// Length of the leading part of a path that says where it starts from: `C:` or `C:\`, a
/// `\\server\share\` UNC prefix, or a single `/`. Zero for a relative path.
///
/// Returned as a length rather than a string so that callers which must not rewrite it - dirname and
/// basename, which node leaves alone - can take the original characters, while normalise can put its
/// own separator in.
let private rootLength (p: string) =
    let n = p.Length
    if isWin && n >= 2 && p[1] = ':' && System.Char.IsLetter p[0] then
        // "C:" is relative to that drive's working directory; "C:\" is absolute. Both are a root.
        skipSeps p 2
    elif isWin && n >= 2 && isSeparator p[0] && isSeparator p[1] then
        // UNC: the server and the share are part of the root, not segments that `..` may climb past
        let serverStart = skipSeps p 0
        let serverEnd = untilSep p serverStart
        let shareStart = skipSeps p serverEnd
        let shareEnd = untilSep p shareStart
        if serverEnd > serverStart && shareEnd > shareStart then skipSeps p shareEnd
        // "\\" or "\\server" with no share is not a UNC name; node falls back to a one-character
        // root here rather than swallowing both separators
        else 1
    elif n >= 1 && isSeparator p[0] then
        skipSeps p 0
    else
        0

/// Only the drive letter counts as a root to node's basename - unlike dirname, it does not know
/// about UNC, so basename("\\\\server\\share") is "share" and not "".
let private driveRootLength (p: string) =
    if isWin && p.Length >= 2 && p[1] = ':' && System.Char.IsLetter p[0] then 2 else 0

/// True when the path starts from a root rather than from wherever the process happens to be.
/// A bare drive is not absolute: "C:" and "C:a" are both relative to that drive's working directory.
let isAbsolute (p: string) =
    let n = p.Length
    if isWin then
        (n >= 1 && isSeparator p[0])
        || (n >= 3 && p[1] = ':' && System.Char.IsLetter p[0] && isSeparator p[2])
    else
        n >= 1 && p[0] = '/'

/// Drop `.`, and cancel each `..` against the segment before it. A `..` that would climb above the
/// start is kept for a relative path (node does the same: "../a" cannot be simplified) and dropped
/// for an absolute one, where there is nothing above the root to reach.
let private collapse (rooted: bool) (segs: string list) =
    let step acc seg =
        match seg with
        | ""
        | "." -> acc
        | ".." ->
            match acc with
            | prev :: rest when prev <> ".." -> rest
            | [] when rooted -> []
            | acc -> ".." :: acc
        | s -> s :: acc

    List.fold step [] segs |> List.rev

/// node's path.normalize: collapse `.` and `..`, squeeze repeated separators, and write the
/// platform's separator throughout. A trailing separator is kept, as node keeps it.
let normalise (p: string) =
    if p = "" then
        "."
    else

    let rLen = rootLength p
    let root =
        if rLen = 0 then
            ""
        elif isWin && rLen >= 2 && p[1] = ':' && System.Char.IsLetter p[0] then
            // "C:" stays bare; "C:\" and "C://" both become "C:\"
            if rLen > 2 then p.Substring(0, 2) + sepStr else p.Substring(0, 2)
        elif isWin && rLen >= 2 && isSeparator p[0] && isSeparator p[1] then
            // rebuild the UNC prefix with this platform's separator, keeping server and share
            let serverStart = skipSeps p 0
            let serverEnd = untilSep p serverStart
            let shareStart = skipSeps p serverEnd
            let shareEnd = untilSep p shareStart
            if serverEnd > serverStart && shareEnd > shareStart then
                sepStr + sepStr
                + p.Substring(serverStart, serverEnd - serverStart)
                + sepStr
                + p.Substring(shareStart, shareEnd - shareStart)
                + sepStr
            else
                sepStr
        else
            sepStr

    let segs =
        p.Substring rLen
        |> fun rest -> rest.Split splitChars
        |> List.ofArray
        |> collapse (rLen > 0)

    let body = String.concat sepStr segs
    let trailing =
        if segs <> [] && isSeparator p[p.Length - 1] then sepStr else ""

    match root, body with
    | "", "" -> "."
    | r, "" when r.EndsWith sepStr -> r
    // a bare drive is relative to that drive's working directory, and node spells that "C:."
    | r, "" -> r + "."
    | "", b -> b + trailing
    | r, b -> r + b + trailing

/// node's path.join: empty parts are dropped, the rest are joined and the result normalised.
let join (parts: string array) =
    let joined =
        parts
        |> Array.filter (fun s -> s <> "")
        |> String.concat sepStr

    if joined = "" then "." else normalise joined

let rec private lastSepIndex (s: string) (i: int) =
    if i < 0 then -1
    elif isSeparator s[i] then i
    else lastSepIndex s (i - 1)

let rec private trimTrailingSeps (s: string) =
    if s.Length > 0 && isSeparator s[s.Length - 1] then trimTrailingSeps (s.Substring(0, s.Length - 1))
    else s

/// node's path.basename: trailing separators are ignored, then everything after the last one. The
/// root has no basename, so "C:\" and "/" give "".
let basename (p: string) =
    let rest = trimTrailingSeps (p.Substring(driveRootLength p))
    match lastSepIndex rest (rest.Length - 1) with
    | -1 -> rest
    | i -> rest.Substring(i + 1)

/// node's path.dirname. Separators inside the path are left exactly as they were found - node does
/// not normalise here, and neither does this.
let dirname (p: string) =
    let rLen = rootLength p
    let root = p.Substring(0, rLen)
    let rest = trimTrailingSeps (p.Substring rLen)

    match lastSepIndex rest (rest.Length - 1) with
    | -1 -> if rLen > 0 then root else "."
    | i ->
        // node slices at the last separator without squeezing the run before it, so dirname("a//b")
        // is "a/" rather than "a". Trimming here would quietly rewrite a path the caller gave us.
        let head = rest.Substring(0, i)
        if rLen > 0 then root + head
        elif head = "" then sepStr
        else head

/// node's path.extname: from the last dot in the basename, when that dot is not the first character.
/// So "a.txt" gives ".txt", "a.b.c" gives ".c", and a dotfile like ".gitignore" gives "".
let extname (p: string) =
    let b = basename p

    let rec lastDot i =
        if i <= 0 then -1
        elif b[i] = '.' then i
        else lastDot (i - 1)

    // "." and ".." are names, not extensions - node gives "" for both
    if b <> "" && Seq.forall (fun c -> c = '.') b then ""
    else
        match lastDot (b.Length - 1) with
        | -1 -> ""
        | i -> b.Substring i
