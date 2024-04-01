module GenerateData
(*******************************************************************************************
                Create Random of Systematic sequences of test data
********************************************************************************************)


//-----------------------------------------------------------------------------------------//  
//------------------------UTILITY FUNCTIONS FOR DATA GENERATION------------------  --------//
//-----------------------------------------------------------------------------------------//  

    module Constants =
        /// Warn user when product test data takes longer than this to change
        /// its second data value. That is when the 1st value size is too large.
        /// Value is a bit arbitrary.
        let maxFirstProductArgSize = 1000

        /// source of random numbers
    let random = System.Random()

    /// Fischer-Yates shuffle algorithm
    /// Returns a random shuffled array without changing the input array
    let shuffleA arrayToShuffle: 'a array =
        let tmpA = Array.copy arrayToShuffle
        for i = 0 to tmpA.Length - 1 do 
            let r = random.Next(i, tmpA.Length);
            (tmpA[i],tmpA[r])
            |> fun (iv, rv) -> tmpA[r] <- iv;  tmpA[i]  <- rv
        tmpA

    let satTimes (a: int32) (b: int32) =
        if int64 a * int64 b > int64 System.Int32.MaxValue
        then System.Int32.MaxValue
        else a * b

    let satPlus (a: int32) (b: int32) =
        if int64 a + int64 b > int64 System.Int32.MaxValue
        then System.Int32.MaxValue
        else a + b


//-----------------------------------------------------------------------------------------//  
//------------------------RANDOM DATA GENERATORS AND COMBINATORS---------------------------//
//-----------------------------------------------------------------------------------------//  

    /// (Data i) is an infinite sequence of test values indexed by non-negative i, repeating after Size
    type Gen<'a> = {Data: int -> 'a; Size: int32}

    let fromList (l: 'a list) =
        {Data=(fun i -> l[i % l.Length]); Size= l.Length}


    let fromArray (l: 'a array) =
        {Data=(fun i -> l[i % l.Length]); Size= l.Length}

    let toArray (g: Gen<'a>) =
        Array.init g.Size g.Data

    let toList (g: Gen<'a>) =
        List.init g.Size g.Data

    /// return a shuffled range of integers
    let randomInt (min:int) (step:int) (max:int) : Gen<int> =
            [|min..step..max|]
            |> shuffleA
            |> fromArray

//------------------------Combinators to transform and combine random sequences-----------------//

    /// Map the sequence elemntwise
    let map (f: 'a -> 'b) (g1: Gen<'a>) : Gen<'b> =
        {Data = (fun i -> g1.Data i |> f); Size = g1.Size}

    /// Map two sequences elementwise to make a third using f (e.g. this could zip two sequences together)
    let map2 (f: 'a1 -> 'a2 -> 'b) (g1: Gen<'a1>) (g2: Gen<'a2>) : Gen<'b> =
        {Data = (fun i -> f (g1.Data i) (g2.Data i)); Size = g1.Size}

    /// Map three sequences elementwise to make a fourth using f to combine them.
    let map3 (f: 'a1 -> 'a2 -> 'a3 -> 'b) (g1: Gen<'a1>) (g2: Gen<'a2>) (g3: Gen<'a3>): Gen<'b> =
        {Data = (fun i -> f (g1.Data i) (g2.Data i) (g3.Data i)); Size = g1.Size}

    /// Cartesian product: output sequence cycles through all combinations of the two inputs, combined by f
    /// All elements in g1 are used in order combined with the first element of g2, then the 2nd, etc.
    /// Therefore this will not work well if g1.Size is too large!
    let product (f: 'a1 -> 'a2 -> 'b) (g1: Gen<'a1>) (g2: Gen<'a2>) : Gen<'b> =
        let n1 = g1.Size
        let n2 = g2.Size
        if g1.Size > Constants.maxFirstProductArgSize then
            printfn $"Warning: product of sequence sizes: {g1.Size} X {g2.Size}. {g1.Size} \
                will take a like time to cycle through {g1.Size} values of g1 before advancing g2"
        {Data = (fun i -> f (g1.Data <| i % n1) (g2.Data <| (i / n1) % n2)); Size = satTimes n1 n2}

    /// Output sequence cycles through the two input sequences appended together
    let sum (g1: Gen<'a>) (g2: Gen<'a>) : Gen<'a> =
        let n = g1.Size + g2.Size
        let f i =
            let i' = (i % int n)
            if i' < g1.Size then g1.Data i' else g2.Data (i' - g1.Size)
        {Data = f; Size = satPlus g1.Size g2.Size}

    /// The same as List.filter but works well on infinite sequences. Note that Size must be small enough
    /// for the filter function to be applied to everything: a scalable much more complex implementation
    /// would evaluate the sequence on demand and allow input Size up to Int32.Max.
    let filter (f: 'a -> bool) (g: Gen<'a>) : Gen<'a> =
         (*  Probably the best scalable solution would be to keep hold of a contiguous array of indices into the unfiltered
             sequence and drop the initial part of this when it gets too big, retaining only 'key' indexes from which any index
             can be reconstructed when it gets too big. Getting thr correct trade-off between calculation time ad storage is
             complex. Principles:
                1. Contiguous values must always be fast.
                2. For small output sequences (repeatedly evaluated)  the input sequence should only be evaluated once.
                3. Hold cached as large an output sequence as is feasible given memory limits
                4. To minimise size store only the input index of each output. Apply g.Data to this on demand to get output.
         *)

        // Use this implementation in case g is so long that turning it into a list would be problematic
        // In that case, after filtering, it may still be much smaller.
        // if output length is very long this implementation is not efficient and a proper "on demand"
        // version should be considered.
        let mutable filteredL = []
        // this loop is more scalable than using List.Filter since it does not require the input list
        // to be stored in memory
        for n = 0 to g.Size - 1 do
            let v = g.Data n
            if f v then
                filteredL <- v :: filteredL
        filteredL
        |> List.rev // get result list the right way round
        |> fromList // turn into a Gen

    /// Truncate samples down to size (repeating) samples.
    /// runTests will run this number of separate tests.
    let truncate (size: int) (samples: Gen<'a>) =
        {samples with Size = size}
            
