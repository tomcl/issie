/// Keys drawn as keys.
///
/// One place, this early in the build, because everything that tells the user which keys to press
/// draws them the same way: the shortcut list in Info, the dialogs that name a chord, and the help
/// text, which is markdown and so cannot reach any module that knows about the model. The
/// appearance is `keyCap` in extra.css; what is here is the arrangement.
module KeyCaps

open Fable.React
open Fable.React.Props

/// One chord drawn as the keys to press: [Ctrl]+[Alt]+[-].
///
/// Takes the key names rather than a shortcut, so that a caller showing a platform other than the
/// one it is running on can do so - the shortcut list in Info shows both. Empty means the shortcut
/// has no chord on that platform, which is said in words: an empty row would read as "no key
/// needed".
let render (parts: string list) : ReactElement =
    match parts with
    | [] -> span [] [ str "(none)" ]
    | parts ->
        parts
        |> List.map (fun k -> span [ HTMLAttr.ClassName "keyCap" ] [ str k ])
        |> List.mapi (fun i cap ->
            if i = 0 then [ cap ] else [ span [ HTMLAttr.ClassName "keyCapPlus" ] [ str "+" ]; cap ])
        |> List.concat
        |> span [ Style [ WhiteSpace WhiteSpaceOptions.Nowrap ] ]
