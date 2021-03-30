module Verilog

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open Fast
open Helpers
open NumberHelpers

let activeComps (fs:FastSimulation) =
    [
        fs.FConstantComps
        fs.FClockedComps
        fs.FOrderedComps
    ]
    |> Array.concat

let makeAccessPathIndex (fs: FastSimulation) =
    let apArr = Array.append [|[]|] (activeComps fs |> Array.map (fun fc -> fc.AccessPath))
    apArr
    |> Array.distinct
    |> Array.sortBy (fun ap -> List.length ap)
    |> Array.indexed
    |> Array.map (fun (index,ap) -> ap,index)
    |> Map.ofArray

let makeUniqueCompLabels fs =
    let index = makeAccessPathIndex fs
    let label (cid,ap) = fs.G.Labels.[cid]
    let makeUniqueLabels (comps: FastComponent array) =
        Array.groupBy (fun comp -> label comp.fId) comps 
        |> Array.collect (fun (lab, fIdArr) -> 
            match fIdArr with
            | [|single|] -> [|single.fId, lab|]
            | fcArr -> 
                Array.indexed fcArr
                |> Array.map (fun (n,fc) ->
                    let fid = fc.fId
                    let apN = index.[snd fid]
                    let suffix = match apN with | 0 -> "" | n -> sprintf "_%d" n
                    match n with
                    | 0 -> fid, sprintf "%s%s" (label fid) suffix
                    | n -> fid, sprintf "%s_%d%s" (label fid) n suffix))
    let uLabels =
        Array.append (activeComps fs) fs.FGlobalInputComps
        |> Array.groupBy (fun fc -> fc.AccessPath)
        |> Array.collect (fun (ap,comps) -> makeUniqueLabels comps)
    assertThat
        (uLabels
        |> Array.map snd
        |> fun labs -> labs.Length = (Array.distinct labs).Length)
        (sprintf 
            "Error making unique labels %A" 
            (uLabels
            |> Array.map (fun (fid,lab) -> lab, fs.FComps.[fid].FullName)
            |> Array.sort))
    Map.ofArray uLabels

     
    


    
let getVerilog (fs:FastSimulation) =
    let uniqueLabels = makeUniqueCompLabels fs
    ()

