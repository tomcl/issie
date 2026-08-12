/// How a read/write memory's contents are held while a simulation runs.
///
/// See [docs/dev/ramRepresentation.md] for why this exists and what it is worth. In short: the
/// contents used to be a `Map<bigint,bigint>` and a fresh snapshot of it was stored for every
/// clock step, so a write cost an AVL path copy plus two boxed `BigInt`s, a read cost a descent
/// with boxed comparisons, and a long waveform simulation retained every version it had ever
/// made. Here
///
/// - a read during simulation is an array index,
/// - a write appends two numbers to shared flat arrays,
/// - the value an address held at any past step is a binary search of that address's own writes,
///   whose cost does not depend on how far the cursor moved, and
/// - nothing at all is allocated per step.
///
/// **Storage is CSR.** Every address that is ever written is given a *slot* number, and a slot's
/// writes are one contiguous run of the shared `IStep`/`IVal` arrays, delimited by `Start`. The
/// alternative - a growable list per address - costs a record and two list objects per address,
/// which is around 230 bytes for what may be a single 8-byte write. That is affordable for one
/// big RAM and is not for a hundred small ones, which between them have just as many addresses.
/// Recent writes go to a small tail and are folded in by `compact`.
///
/// **Where this file sits.** F# compile order is the dependency layering, and
/// `SimGraphTypes.SimulationComponentState` names this type, so it has to be compiled before
/// `SimGraphTypes.fs` and cannot live in the `FastSim` block with the rest of the simulator. It
/// depends on nothing but `CommonTypes`, which is what makes that possible.
///
/// **Mutability.** This is the same layer as the step arrays, which are already mutable, and
/// [docs/mutableState.md] allows it for a measured performance reason - the whole point here is
/// to stop allocating. Nothing mutable escapes: `Memory1` remains the immutable type everywhere
/// outside the running simulation, and `toMemory` builds one on demand.
module RamStore

open CommonTypes

module Constants =
    /// Up to this address width every address gets a slot number in one flat array: 2^16 ints is
    /// 256kB, which is always affordable. Above it the address space outruns what can be
    /// allocated and the memory is assumed to be occupied thinly.
    let maxDenseAddressWidth = 16

    /// Up to this address width an address still fits a uint32 and the trie can key on one.
    /// Above it addresses arrive as bigints and the lookup falls back to a Map - a memory with
    /// more than 4G words cannot be meaningfully simulated, so that path is a guard, not a
    /// design.
    let maxTrieAddressWidth = 32

    /// Children per trie node. Four bits a level.
    let trieBranch = 16

    /// Compact once the tail is at least this long *and* at least as long as what is already
    /// indexed. Doubling like this makes the copying amortised O(1) a write.
    ///
    /// Waiting for a query instead, when there is nothing out of window to drop, was tried and is
    /// worse: a tail entry carries a slot number that an indexed one does not, so leaving writes
    /// in the tail costs 12 bytes each against 8, and on a hundred small RAMs that measured 19.9 MB
    /// against 11.8 MB with no speed to show for it.
    let minTailBeforeCompact = 4096

/// A 16-way trie over the address, used when the address space is too large to give every
/// address a slot number in a flat array.
///
/// It is grown in place and never copied: a slot is filled once and never repointed, which is
/// what removes the per-write path copy the `Map` paid. A `Leaf` may sit above its natural depth
/// - that path compression is the whole reason this is a union rather than a fixed-depth array
/// of arrays, since depth follows from the address width and would otherwise be known - so the
/// leaf carries its own address and it is checked on arrival.
type NodeOrLeaf =
    | Node of NodeOrLeaf option array
    | Leaf of addr: uint32 * slot: int

/// How an address is turned into its slot number. This is the *only* place the strategies differ:
/// everything after it works on slot numbers.
type Addressing =
    /// Slot number per address, -1 for never written. `AddressWidth <= maxDenseAddressWidth`.
    | Dense of int array
    /// A path-compressed trie, with the depth it was built for.
    | Sparse of root: NodeOrLeaf option array * depth: int
    /// Addresses wider than 32 bits, which arrive as bigints. Sparse by construction.
    | Wide of Map<bigint, int> ref
    /// A read-only memory: contents are part of the component type and never change, so there
    /// are no slots and every read is of `InitialData`.
    | Fixed

/// A read/write memory as the simulator holds it.
type Ram =
    { AddressWidth: int
      WordWidth: int
      /// `WordWidth > 32`, so values live in the bigint arrays rather than the uint32 ones. The
      /// same split `IOArray` makes between `UInt32Step` and `BigIntStep`, and for the same reason.
      BigVals: bool
      Init: InitMemData
      Comments: Map<bigint, string> option
      /// What the memory starts with, kept so that a restart can put it back.
      InitialData: Map<bigint, bigint>
      Addressing: Addressing
      /// How many steps of history are reachable. The step simulator's arrays wrap, so anything
      /// older than this can never be asked for and is dropped at the next compaction; the
      /// waveform simulator's arrays are sized for the whole run, so nothing is ever out of
      /// window there.
      Window: int

      /// Current contents by *address*, for the dense case, so that a read during simulation is
      /// one indexed load. Empty otherwise, when the read goes via the slot number instead.
      Words: uint32 array
      BigWords: bigint array

      // ---- slots: one per address ever written ----
      mutable SlotCount: int
      /// slot -> address. `uint32` because a slot only exists once an address has been written,
      /// and the wide case keeps its own reverse list.
      mutable SlotAddr: uint32 array
      mutable SlotAddrBig: ResizeArray<bigint>
      /// slot -> current contents, for the cases with no `Words`.
      mutable CurVal: uint32 array
      mutable CurBig: bigint array

      // ---- indexed entries: slot s owns IStep/IVal[Start[s] .. Start[s+1]), ascending by step ----
      mutable Start: int array
      mutable IStep: int array
      mutable IVal: uint32 array
      mutable IBig: bigint array
      mutable ILen: int

      // ---- tail: writes not yet folded into the indexed region ----
      mutable TSlot: int array
      mutable TStep: int array
      mutable TVal: uint32 array
      mutable TBig: bigint array
      mutable TLen: int

      /// The most recent step written, which is what pruning measures the window back from.
      mutable LastStep: int
      /// Writes actually recorded. A write of the value already there is not one of them.
      mutable Writes: int }

//-------------------------------------------------------------------------------------------//
//----------------------------------- growable flat arrays ----------------------------------//
//-------------------------------------------------------------------------------------------//

// One per element type rather than one inline generic: `Array.zeroCreate` on bigint is not
// something to rely on under Fable, and three short functions are clearer than a constraint.

let private ensureInt (arr: int array) (n: int) =
    if arr.Length >= n then arr
    else
        let a = Array.zeroCreate<int> (max 8 (max n (arr.Length * 2)))
        Array.blit arr 0 a 0 arr.Length
        a

let private ensureU32 (arr: uint32 array) (n: int) =
    if arr.Length >= n then arr
    else
        let a = Array.zeroCreate<uint32> (max 8 (max n (arr.Length * 2)))
        Array.blit arr 0 a 0 arr.Length
        a

let private ensureBig (arr: bigint array) (n: int) =
    if arr.Length >= n then arr
    else
        let a = Array.create (max 8 (max n (arr.Length * 2))) 0I
        Array.blit arr 0 a 0 arr.Length
        a

/// Index of the last entry of `steps[lo..hi)` at or before `step`, or -1. The entries are
/// appended in step order, so a plain binary search does it.
let private lastAtOrBefore (steps: int array) (lo: int) (hi: int) (step: int) =
    let mutable a = lo
    let mutable b = hi - 1
    let mutable ans = -1
    while a <= b do
        let mid = (a + b) / 2
        if steps[mid] <= step then
            ans <- mid
            a <- mid + 1
        else
            b <- mid - 1
    ans

//-------------------------------------------------------------------------------------------//
//------------------------------------- address -> slot --------------------------------------//
//-------------------------------------------------------------------------------------------//

let private nibble (addr: uint32) (level: int) (depth: int) =
    int ((addr >>> (4 * (depth - 1 - level))) &&& 15u)

let rec private findAt (arr: NodeOrLeaf option array) (level: int) (addr: uint32) (depth: int) =
    match arr[nibble addr level depth] with
    | None -> -1
    | Some(Leaf(a, slot)) -> if a = addr then slot else -1
    | Some(Node children) -> findAt children (level + 1) addr depth

/// Find or create the slot for `addr`, splitting a compressed leaf if a second address arrives
/// beneath it.
let rec private insertAt
    (arr: NodeOrLeaf option array)
    (level: int)
    (addr: uint32)
    (depth: int)
    (mk: unit -> int)
    : int
    =
    let i = nibble addr level depth
    match arr[i] with
    | None ->
        let slot = mk ()
        arr[i] <- Some(Leaf(addr, slot))
        slot
    | Some(Leaf(a, slot)) when a = addr -> slot
    | Some(Leaf(a, slot)) ->
        // two addresses now share this path: push the sitting leaf one level down and try again.
        // If they collide there too the next call splits again, which terminates because
        // distinct addresses must differ in some nibble.
        let children: NodeOrLeaf option array = Array.create Constants.trieBranch None
        children[nibble a (level + 1) depth] <- Some(Leaf(a, slot))
        arr[i] <- Some(Node children)
        insertAt children (level + 1) addr depth mk
    | Some(Node children) -> insertAt children (level + 1) addr depth mk

/// The slot for an address, or -1 if it has never been written.
let private slotOf (ram: Ram) (addr: uint32) =
    match ram.Addressing with
    | Dense slots -> slots[int addr]
    | Sparse(root, depth) -> findAt root 0 addr depth
    | Wide _
    | Fixed -> -1

let private slotOfBig (ram: Ram) (addr: bigint) =
    match ram.Addressing with
    | Wide table ->
        match Map.tryFind addr table.Value with
        | Some slot -> slot
        | None -> -1
    | _ -> slotOf ram (uint32 addr)

/// Give a new address a slot, growing the per-slot arrays with it.
let private newSlot (ram: Ram) (addr: uint32) (addrBig: bigint) =
    let slot = ram.SlotCount
    ram.SlotCount <- slot + 1
    ram.SlotAddr <- ensureU32 ram.SlotAddr (slot + 1)
    ram.CurVal <- ensureU32 ram.CurVal (slot + 1)
    if ram.BigVals then ram.CurBig <- ensureBig ram.CurBig (slot + 1)
    ram.SlotAddr[slot] <- addr
    match ram.Addressing with
    | Wide _ -> ram.SlotAddrBig.Add addrBig
    | _ -> ()
    slot

/// The slot for an address, created if it does not yet exist.
let private slotFor (ram: Ram) (addr: uint32) =
    match ram.Addressing with
    | Dense slots ->
        let s = slots[int addr]
        if s >= 0 then s
        else
            let s = newSlot ram addr (bigint addr)
            slots[int addr] <- s
            s
    | Sparse(root, depth) -> insertAt root 0 addr depth (fun () -> newSlot ram addr (bigint addr))
    | Wide _ -> failwith "RamStore: a wide-addressed memory must be written through slotForBig"
    | Fixed -> failwith "RamStore: a read-only memory cannot be written"

let private slotForBig (ram: Ram) (addr: bigint) =
    match ram.Addressing with
    | Wide table ->
        match Map.tryFind addr table.Value with
        | Some slot -> slot
        | None ->
            let slot = newSlot ram 0u addr
            table.Value <- Map.add addr slot table.Value
            slot
    | _ -> slotFor ram (uint32 addr)

/// The address a slot stands for.
let private addressOf (ram: Ram) (slot: int) =
    match ram.Addressing with
    | Wide _ -> ram.SlotAddrBig[slot]
    | _ -> bigint ram.SlotAddr[slot]

//-------------------------------------------------------------------------------------------//
//------------------------------------- compaction ------------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Fold the tail into the indexed region, and drop entries that have fallen out of the reachable
/// window while doing it.
///
/// Pruning here rather than per write is what makes CSR workable: dropping an entry from the
/// middle of a slot's run would otherwise mean shifting everything after it, whereas a compaction
/// is rebuilding the runs anyway. It **keeps the most recent out-of-window entry** for each slot:
/// dropping all of them would lose the value of an address written once long ago and never since,
/// for which that entry is the only record.
let private compact (ram: Ram) =
    let slotCount = ram.SlotCount
    let lowWater = ram.LastStep - ram.Window

    // stable counting sort of the tail by slot, so each slot's tail entries can be visited in
    // the order they were written, which is step order
    let tCount = Array.zeroCreate<int> (slotCount + 1)
    for i in 0 .. ram.TLen - 1 do
        tCount[ram.TSlot[i]] <- tCount[ram.TSlot[i]] + 1
    let tStart = Array.zeroCreate<int> (slotCount + 1)
    let mutable acc = 0
    for s in 0 .. slotCount - 1 do
        tStart[s] <- acc
        acc <- acc + tCount[s]
    tStart[slotCount] <- acc
    let fill = Array.copy tStart
    let tOrder = Array.zeroCreate<int> ram.TLen
    for i in 0 .. ram.TLen - 1 do
        let s = ram.TSlot[i]
        tOrder[fill[s]] <- i
        fill[s] <- fill[s] + 1

    // how many entries each slot keeps, and so where its run starts
    let oldStart = ram.Start
    let oldStartValid = oldStart.Length
    let inline idxLo s = if s + 1 < oldStartValid then oldStart[s] else ram.ILen
    let inline idxHi s = if s + 1 < oldStartValid then oldStart[s + 1] else ram.ILen

    /// Entries of this slot at or before the low water mark, all but the last of which go. The
    /// entries are in step order - the indexed run first, then the tail - so this counts forwards
    /// and stops at the first entry inside the window.
    let dropCount s =
        if ram.Window <= 0 || lowWater <= 0 then
            0
        else
            let iLo, iHi = idxLo s, idxHi s
            let mutable nLE = 0
            let mutable k = iLo
            while k < iHi && ram.IStep[k] <= lowWater do
                nLE <- nLE + 1
                k <- k + 1
            if nLE = iHi - iLo then
                let mutable j = tStart[s]
                while j < tStart[s + 1] && ram.TStep[tOrder[j]] <= lowWater do
                    nLE <- nLE + 1
                    j <- j + 1
            max 0 (nLE - 1)

    let newStart = Array.zeroCreate<int> (slotCount + 1)
    let mutable out = 0
    for s in 0 .. slotCount - 1 do
        newStart[s] <- out
        let total = (idxHi s - idxLo s) + (tStart[s + 1] - tStart[s])
        out <- out + total - dropCount s
    newStart[slotCount] <- out

    // write the kept entries out in slot order
    let nStep = Array.zeroCreate<int> (max 8 out)
    let nVal = if ram.BigVals then Array.empty else Array.zeroCreate<uint32> (max 8 out)
    let nBig = if ram.BigVals then Array.create (max 8 out) 0I else Array.empty
    for s in 0 .. slotCount - 1 do
        let mutable drop = dropCount s
        let mutable o = newStart[s]
        let mutable k = idxLo s
        let iHi = idxHi s
        while k < iHi do
            if drop > 0 then drop <- drop - 1
            else
                nStep[o] <- ram.IStep[k]
                if ram.BigVals then nBig[o] <- ram.IBig[k] else nVal[o] <- ram.IVal[k]
                o <- o + 1
            k <- k + 1
        let mutable j = tStart[s]
        while j < tStart[s + 1] do
            let i = tOrder[j]
            if drop > 0 then drop <- drop - 1
            else
                nStep[o] <- ram.TStep[i]
                if ram.BigVals then nBig[o] <- ram.TBig[i] else nVal[o] <- ram.TVal[i]
                o <- o + 1
            j <- j + 1

    ram.Start <- newStart
    ram.IStep <- nStep
    ram.IVal <- nVal
    ram.IBig <- nBig
    ram.ILen <- out
    ram.TLen <- 0

/// Queries work on the indexed region only, so anything in the tail is folded in first. Writes
/// and queries do not interleave within a step, so this happens at most once per render.
let private ensureCompact (ram: Ram) =
    if ram.TLen > 0 then compact ram

//-------------------------------------------------------------------------------------------//
//-------------------------------------- construction ---------------------------------------//
//-------------------------------------------------------------------------------------------//

let private lowWordMask = (1I <<< 32) - 1I

/// Reduce a stored value to the uint32 the simulation carries. Contents come from a `.dgm` or a
/// `.ram` file and are trusted to be within the word width, as everything in a step array is.
let private toU (v: bigint) = uint32 (v &&& lowWordMask)

//-------------------------------------------------------------------------------------------//
//----------------------------------------- reading -----------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Current contents at `addr`, on the uint32 path. One array index in the dense case, which is
/// the case every RAM a design can actually simulate falls into.
let readU (ram: Ram) (addr: uint32) : uint32 =
    match ram.Addressing with
    | Dense _ -> ram.Words[int addr]
    | Fixed -> toU (Map.tryFind (bigint addr) ram.InitialData |> Option.defaultValue 0I)
    | _ ->
        match slotOf ram addr with
        | -1 -> 0u
        | s -> ram.CurVal[s]

/// Current contents at `addr`, on the bigint-value path.
///
/// A store keeps values in one of two forms, chosen once by word width, and the caller has to ask
/// for the one it holds - the evaluators do, via `BigIntState`. Asking for the other would
/// otherwise read an array that was never filled, so it is redirected rather than trusted.
let readBigVal (ram: Ram) (addr: uint32) : bigint =
    if not ram.BigVals then
        bigint (readU ram addr)
    else
        match ram.Addressing with
        | Dense _ -> ram.BigWords[int addr]
        | Fixed -> Map.tryFind (bigint addr) ram.InitialData |> Option.defaultValue 0I
        | _ ->
            match slotOf ram addr with
            | -1 -> 0I
            | s -> ram.CurBig[s]

/// Current contents at a bigint address - only reachable when the address bus is over 32 bits.
let private readWideU (ram: Ram) (addr: bigint) : uint32 =
    match slotOfBig ram addr with
    | -1 -> 0u
    | s -> ram.CurVal[s]

let private readWideBig (ram: Ram) (addr: bigint) : bigint =
    if not ram.BigVals then
        bigint (readWideU ram addr)
    else
        match slotOfBig ram addr with
        | -1 -> 0I
        | s -> ram.CurBig[s]

/// The four combinations the evaluators need, named as the `Memory1` versions they replace so
/// that the call sites read the same.
let readAddrUInt32DataUInt32 (ram: Ram) (addr: uint32) : uint32 = readU ram addr

let readAddrUInt32DataBigInt (ram: Ram) (addr: uint32) : bigint = readBigVal ram addr

let readAddrBigIntDataUInt32 (ram: Ram) (addr: bigint) : uint32 =
    match ram.Addressing with
    | Wide _ -> readWideU ram addr
    | _ -> readU ram (uint32 addr)

let readAddrBigIntDataBigInt (ram: Ram) (addr: bigint) : bigint =
    match ram.Addressing with
    | Wide _ -> readWideBig ram addr
    | _ -> readBigVal ram (uint32 addr)

//-------------------------------------------------------------------------------------------//
//----------------------------------------- writing -----------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Everything a write does once its slot is known: append to the tail, and compact if the tail
/// has grown enough to be worth folding in. This is where the addressing strategies meet - they
/// differ only in how the slot was reached - so there is one of these and not one per strategy.
let private append (ram: Ram) (slot: int) (step: int) (value: uint32) (big: bigint) =
    let n = ram.TLen
    ram.TSlot <- ensureInt ram.TSlot (n + 1)
    ram.TStep <- ensureInt ram.TStep (n + 1)
    if ram.BigVals then ram.TBig <- ensureBig ram.TBig (n + 1)
    else ram.TVal <- ensureU32 ram.TVal (n + 1)
    ram.TSlot[n] <- slot
    ram.TStep[n] <- step
    if ram.BigVals then ram.TBig[n] <- big else ram.TVal[n] <- value
    ram.TLen <- n + 1
    ram.LastStep <- step
    ram.Writes <- ram.Writes + 1
    if ram.TLen >= Constants.minTailBeforeCompact && ram.TLen >= ram.ILen then
        compact ram

/// Record a write, unless it changes nothing.
///
/// A write of the value already at that address is not a write: skipping it costs one comparison
/// on a value the reducer has already read (it is the old contents, which a RAM returns as its
/// output) and saves an entry, which every later search and every later compaction then does not
/// have to walk. Nothing observable changes - the contents are identical by definition, and the
/// RAM table's read/write highlighting comes from the input step arrays rather than from here.
let writeU (ram: Ram) (step: int) (addr: uint32) (value: uint32) =
    if readU ram addr <> value then
        let slot = slotFor ram addr
        ram.CurVal[slot] <- value
        if ram.Words.Length > 0 then ram.Words[int addr] <- value
        append ram slot step value 0I

let writeBigVal (ram: Ram) (step: int) (addr: uint32) (value: bigint) =
    if not ram.BigVals then writeU ram step addr (toU value)
    elif readBigVal ram addr <> value then
        let slot = slotFor ram addr
        ram.CurBig[slot] <- value
        if ram.BigWords.Length > 0 then ram.BigWords[int addr] <- value
        append ram slot step 0u value

let private writeWideU (ram: Ram) (step: int) (addr: bigint) (value: uint32) =
    if readWideU ram addr <> value then
        let slot = slotForBig ram addr
        ram.CurVal[slot] <- value
        append ram slot step value 0I

let private writeWideBig (ram: Ram) (step: int) (addr: bigint) (value: bigint) =
    if not ram.BigVals then writeWideU ram step addr (toU value)
    elif readWideBig ram addr <> value then
        let slot = slotForBig ram addr
        ram.CurBig[slot] <- value
        append ram slot step 0u value

let writeAddrUInt32DataUInt32 (ram: Ram) (step: int) (addr: uint32) (value: uint32) =
    writeU ram step addr value

let writeAddrUInt32DataBigInt (ram: Ram) (step: int) (addr: uint32) (value: bigint) =
    writeBigVal ram step addr value

let writeAddrBigIntDataUInt32 (ram: Ram) (step: int) (addr: bigint) (value: uint32) =
    match ram.Addressing with
    | Wide _ -> writeWideU ram step addr value
    | _ -> writeU ram step (uint32 addr) value

let writeAddrBigIntDataBigInt (ram: Ram) (step: int) (addr: bigint) (value: bigint) =
    match ram.Addressing with
    | Wide _ -> writeWideBig ram step addr value
    | _ -> writeBigVal ram step (uint32 addr) value

//-------------------------------------------------------------------------------------------//
//----------------------------------------- history -----------------------------------------//
//-------------------------------------------------------------------------------------------//

/// What a slot held at the end of `step`. A binary search of that slot's run, so the cost does not
/// depend on where the cursor was - the property the waveform viewer's RAM table needs, and the
/// one a single global write log would not have.
let private slotValueAt (ram: Ram) (slot: int) (step: int) =
    let lo = ram.Start[slot]
    let hi = ram.Start[slot + 1]
    match lastAtOrBefore ram.IStep lo hi step with
    | -1 -> 0I
    | i -> if ram.BigVals then ram.IBig[i] else bigint ram.IVal[i]

/// What one address held at the end of `step`.
let wordAt (ram: Ram) (step: int) (addr: bigint) : bigint =
    match ram.Addressing with
    | Fixed -> Map.tryFind addr ram.InitialData |> Option.defaultValue 0I
    | _ ->
        ensureCompact ram
        match slotOfBig ram addr with
        | -1 -> 0I
        | s -> slotValueAt ram s step

/// Whether more than `limit` words were non-zero at the end of `step`.
///
/// This is all the RAM table needs: it chooses between listing every non-zero location and a
/// windowed display, and the exact figure never appears. Asking the cheaper question means the
/// walk stops as soon as the answer is settled, and usually it never starts - a memory that has
/// never had more than `limit` addresses written cannot have more than `limit` non-zero.
let liveCountExceeds (ram: Ram) (step: int) (limit: int) : bool =
    match ram.Addressing with
    // A read-only memory's contents are already the answer, but they are still walked lazily and
    // only as far as the slot walk below would go. Counting all of them meant a filtered copy of
    // the whole thing - for a large ROM, on every render of the table that asks.
    | Fixed ->
        ram.InitialData
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v <> 0I)
        |> Seq.truncate (limit + 1)
        |> Seq.length
        |> (fun count -> count > limit)
    | _ ->
        if ram.SlotCount <= limit then
            false
        else
            ensureCompact ram
            let mutable count = 0
            let mutable s = 0
            while count <= limit && s < ram.SlotCount do
                if slotValueAt ram s step <> 0I then count <- count + 1
                s <- s + 1
            count > limit

/// How many words were non-zero at the end of `step`. Only diagnostics and tests want the exact
/// number; the display wants `liveCountExceeds`.
let liveCountAt (ram: Ram) (step: int) : int =
    match ram.Addressing with
    | Fixed -> ram.InitialData |> Map.filter (fun _ v -> v <> 0I) |> Map.count
    | _ ->
        ensureCompact ram
        let mutable count = 0
        for s in 0 .. ram.SlotCount - 1 do
            if slotValueAt ram s step <> 0I then count <- count + 1
        count

/// The whole memory as of `step`, as the immutable type the rest of Issie uses. Only for the
/// places that genuinely want all of it - a memory diff, a golden file - never per render.
let toMemory (ram: Ram) (step: int) : Memory1 =
    let data =
        match ram.Addressing with
        | Fixed -> ram.InitialData |> Map.filter (fun _ v -> v <> 0I)
        | _ ->
            ensureCompact ram
            let mutable acc = Map.empty
            for s in 0 .. ram.SlotCount - 1 do
                let v = slotValueAt ram s step
                if v <> 0I then acc <- Map.add (addressOf ram s) v acc
            acc
    { Init = ram.Init
      AddressWidth = ram.AddressWidth
      WordWidth = ram.WordWidth
      Data = data
      Comments = ram.Comments }

//-------------------------------------------------------------------------------------------//
//------------------------------------ create and reset -------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Put the memory back to the contents it was built with, discarding all history. Called when a
/// simulation restarts, which is the one thing the old representation got wrong: it read the
/// state left by the wrapped-out end of the previous run instead.
let reset (ram: Ram) =
    match ram.Addressing with
    | Dense slots -> Array.fill slots 0 slots.Length -1
    | Sparse(root, _) -> Array.fill root 0 root.Length None
    | Wide table -> table.Value <- Map.empty
    | Fixed -> ()
    if ram.Words.Length > 0 then Array.fill ram.Words 0 ram.Words.Length 0u
    if ram.BigWords.Length > 0 then Array.fill ram.BigWords 0 ram.BigWords.Length 0I
    ram.SlotCount <- 0
    ram.SlotAddr <- Array.empty
    ram.SlotAddrBig <- ResizeArray<bigint>()
    ram.CurVal <- Array.empty
    ram.CurBig <- Array.empty
    ram.Start <- Array.zeroCreate<int> 1
    ram.IStep <- Array.empty
    ram.IVal <- Array.empty
    ram.IBig <- Array.empty
    ram.ILen <- 0
    ram.TSlot <- Array.empty
    ram.TStep <- Array.empty
    ram.TVal <- Array.empty
    ram.TBig <- Array.empty
    ram.TLen <- 0
    ram.LastStep <- 0
    ram.Writes <- 0
    // Seed the initial contents as writes at step -1, so that an address with no entries reads as
    // zero and nothing has to fall back to a Map on the read path.
    match ram.Addressing with
    | Fixed -> ()
    | _ ->
        // A location the memory does not have is ignored rather than written. Contents come from a
        // .dgm and are not guaranteed to match the address width it declares - the .ram reader and
        // the memory editor both range-check, but a file need not have come from either - and an
        // address past the end is one the simulation can never present, so dropping it is what the
        // Map this replaced did in effect. Writing it would index off the end of the slot array
        // and fail the whole simulation with an internal error.
        let pastTheEnd = 1I <<< ram.AddressWidth
        ram.InitialData
        |> Map.iter (fun addr value ->
            if value <> 0I && addr >= 0I && addr < pastTheEnd then
                if ram.BigVals then writeAddrBigIntDataBigInt ram -1 addr value
                else writeAddrBigIntDataUInt32 ram -1 addr (toU value))
        // seeding is not simulation: it should not count as writes the design made
        ram.Writes <- 0
        ram.LastStep <- 0

let private emptyRam addressWidth wordWidth init comments initialData addressing words bigWords window =
    { AddressWidth = addressWidth
      WordWidth = wordWidth
      BigVals = wordWidth > 32
      Init = init
      Comments = comments
      InitialData = initialData
      Addressing = addressing
      Window = window
      Words = words
      BigWords = bigWords
      SlotCount = 0
      SlotAddr = Array.empty
      SlotAddrBig = ResizeArray<bigint>()
      CurVal = Array.empty
      CurBig = Array.empty
      Start = Array.zeroCreate<int> 1
      IStep = Array.empty
      IVal = Array.empty
      IBig = Array.empty
      ILen = 0
      TSlot = Array.empty
      TStep = Array.empty
      TVal = Array.empty
      TBig = Array.empty
      TLen = 0
      LastStep = 0
      Writes = 0 }

/// Build the store for one read/write memory. `window` is the simulation's `MaxArraySize`: the
/// number of past steps that can still be asked about.
let ofMemory (window: int) (mem: Memory1) : Ram =
    let aw = mem.AddressWidth
    let big = mem.WordWidth > 32
    let addressing, words, bigWords =
        if aw <= Constants.maxDenseAddressWidth then
            let n = 1 <<< aw
            Dense(Array.create n -1),
            (if big then Array.empty else Array.zeroCreate<uint32> n),
            (if big then Array.create n 0I else Array.empty)
        elif aw <= Constants.maxTrieAddressWidth then
            Sparse(Array.create Constants.trieBranch None, (aw + 3) / 4), Array.empty, Array.empty
        else
            Wide(ref Map.empty), Array.empty, Array.empty
    let ram =
        emptyRam aw mem.WordWidth mem.Init mem.Comments mem.Data addressing words bigWords window
    reset ram
    ram

/// A read-only memory wrapped so that the one extraction path can serve ROMs and RAMs alike.
let fixedOf (mem: Memory1) : Ram =
    emptyRam mem.AddressWidth mem.WordWidth mem.Init mem.Comments mem.Data Fixed Array.empty Array.empty 0
