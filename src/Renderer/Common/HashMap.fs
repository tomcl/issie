module HashMap

//=================================================//
// Faster version of Map for objects with SHA hash //
//=================================================//

// Currently not used. Does it work?

type HMap<'T> = 
    | Tree of HMap<'T> array
    | Found of 'T
    | Empty

let inline initHArray() = Array.create 16 Empty

let charCodeF = int 'a' 
let charCode0 = int '0' 

let inline getHashDigit n (h:string) =
    let ch = h[n]
    if System.Char.IsDigit ch then
        int ch - charCode0          
    elif System.Char.IsLetter ch then
        int ch - charCodeF + 10
    elif ch = '-' then 0           
    else
        failwithf "Hash to digit conversion failed on n={n}, char={ch}"

let getFastHash (sha:string) =
    sha 
    |> Seq.toArray 
    |> Array.mapi (fun i _ -> getHashDigit i sha)

let getFastHItem (x,_,_) = x

let getFastSHA (sha:string, x:int array, _)  n = x[n]

let inline copyUpdate n item arr =
    Array.init 
        (Array.length arr) 
        (fun i -> if i = n then item else Empty)

let hMapAdd (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
    let hash = getSHA item
    let rec hAdd shaIndex hm =
        match hm with
        | Empty -> Found item
        | Found item' ->
            let hash' = getSHA item'
            if getFastEq item' = getFastEq item then  
                Found item
            else
                let h = hash shaIndex
                let h' = hash' shaIndex
                let arr = initHArray()
                if h <> h' then
                    arr[h] <- Found item 
                    arr[h'] <- Found item'
                    Tree arr
                else 
                    arr[h'] <- hAdd (shaIndex+1) (Found item')
                    Tree arr
        | Tree arr ->
            let h = hash shaIndex
            let hm' = hAdd (shaIndex+1) arr[h]
            Tree <| copyUpdate h hm' arr
    hAdd 0 hm  


let hMapAddMutate (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
    let hash = getSHA item
    let rec hAdd shaIndex hm =
        match hm with
        | Empty -> Found item
        | Found item' ->
            let hash' = getSHA item'
            if getFastEq item = getFastEq item' then  
                Found item
            else
                let arr = initHArray()
                let h = hash shaIndex
                let h' = hash' shaIndex
                if h <> h' then
                    arr[h] <- Found item 
                    arr[h'] <- Found item'
                    Tree arr
                else 
                    arr[h'] <- hAdd (shaIndex+1) (Found item')
                    Tree arr
                    
        | Tree arr ->
            let h = hash shaIndex
            let hm' = hAdd (shaIndex+1) arr[h]
            arr[h] <- hm'
            Tree arr
    hAdd 0 hm  

let hMapTryFind (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
    let rec lookup shaIndex (hm:HMap<'T>) =
        match hm with
        | Found item' when getFastEq item' = getFastEq item ->
            Some item'
        | Tree arr ->
            let hit = arr[getSHA item shaIndex]
            lookup (shaIndex+1) hit
        | _ -> None
    lookup 0 hm

let rec hMapFilter (pred: 'T -> bool) (hm: HMap<'T>) =
    match hm with
    | Found item' as x when pred item' -> x
    | Tree arr ->
        let arr' = Array.map (hMapFilter pred) arr
        if Array.exists (fun x -> x <> Empty) arr' then 
            Tree arr'
        else Empty                
    | _ -> Empty

let rec hMapToArray (hm: HMap<'T>) =
    match hm with
    | Empty -> [||]
    | Found x -> [|x|]
    | Tree arr -> 
        arr
        |> Array.map hMapToArray
        |> Array.concat

let rec arrayToHmap (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (arr: 'T array) =
    (Empty, arr)
    ||> Array.fold (fun hm item -> hMapAddMutate getFastEq getSHA item hm)


let rec hMapCount (hm: HMap<'T>) =
    match hm with
    | Empty -> 0
    | Found _ -> 1
    | Tree arr -> 
        arr
        |> Array.map hMapCount
        |> Array.sum

