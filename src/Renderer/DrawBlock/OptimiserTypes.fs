module OptimiserTypes

    open DrawModelType

    //----------------------------------------------------------------------------------------------------------------//
    //---------------------------------------------Suggested Interface for Optimisation-------------------------------//
    //----------------------------------------------------------------------------------------------------------------//

    type IOptimise<'LOCS,'NEIGHBOURS when 'LOCS:comparison and 'NEIGHBOURS:comparison> =

        /// More positive values => better sheets. Can be positive or negative.
        abstract ObjectiveFn: SheetT.Model -> float

        /// for any Team's work, 'LOCs and 'NEIGHBOURS with have specific known types (e.g. ComponentId and STransform, or whatever)
        /// these function signatures indicate a complete set of functions that define how a sheet is optimised
        /// 'LOCS enumerates (vaguely) independent ways sheet can be mutated:
        /// e.g. for D2 it might be the  set of MUXES and Gates on the sheet
        abstract GetLocsFromSheet: SheetT.Model -> 'LOCS Set

        /// 'NEIGHBOURS has all possible values representing distinct circuit mutations of one element of 'LOCs.
        /// This function specifies which specific mutations are possible at which LOC.
        /// e.g. for all MUXes we might have 4 values (isReversed,isFlipped) = (false,false), (false,true), (true,false) , (true,true)
        /// It can be used to enumerate neighbours
        abstract GetNeighboursFromLoc: SheetT.Model -> 'LOCS -> 'NEIGHBOURS Set

    
        /// Generate a mutated sheet from a specification of all the mutation parameters
        abstract MutateSheet:  Map<'LOCS,'NEIGHBOURS> -> SheetT.Model -> SheetT.Model

        /// function that can extract from a sheet its mutation parameters
        /// as a map from location to the corresponding neighbour value
        /// (e.g. from MUX to the current flip/reverse state of MUX)
        abstract GetSheetLocValues: SheetT.Model -> Map<'LOCS,'NEIGHBOURS>

        /// One-size-fits-all top-lebel beautify function
        abstract Beautify: SheetT.Model -> SheetT.Model

    
    /// example of how to implement the IOptimiser interface using your own functions
    /// your functions should replace the skeleton interface functions defined below.
    /// note that <int,string> must be replaced with the types you are using for 'LOCS and 'NEIGHBOURS respectively
    /// your types are likely to be D.U.s
    let myOptimisationCode = {

        new IOptimise<int,string> with

            member this.ObjectiveFn sheet =
                // example of how to call any of the interface functions
                // from anotehr interface function
                let locs = this.GetLocsFromSheet 
                failwithf "Objective function implementation"
        
            member this.GetLocsFromSheet sheet = failwithf "get Locs from sheet implementation"

            member this.GetNeighboursFromLoc sheet loc = failwithf "get Locs from sheet implementation"

            member this.MutateSheet mutationMap sheet = failwithf "get mutated sheet from sheet and map of mutations"

            member this.GetSheetLocValues sheet = failwithf "get Loc values defining sheeet"

            member this.Beautify sheet = sheet // dummy function does nothing
        }

    /// example of how to use your functionns via the IOptimiser interface
    let evalSheet sheet = myOptimisationCode.ObjectiveFn sheet // use the objective function from the interface
