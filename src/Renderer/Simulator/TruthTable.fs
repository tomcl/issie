module TruthTable

type TruthTableCell = SimulatorTypes.SimulationIO * SimulatorTypes.Bit
type TruthTableRow = TruthTableCell list
type TruthTableStruct = Map<TruthTableRow,TruthTableRow>

let tableCell (comp: SimulatorTypes.SimulationIO) (bit: SimulatorTypes.Bit): TruthTableCell =
    (comp,bit)

let bitstring (num: int): SimulatorTypes.Bit list =
    let bitFromInt i =
        match i with
        | 0 -> SimulatorTypes.Zero
        | 1 -> SimulatorTypes.One
        | _ -> failwithf "what? bits can only be created as 0 or 1"
    let rec bitstring_ (num: int) (lst: SimulatorTypes.Bit list): SimulatorTypes.Bit list =
        match num with 
        | 0 -> if lst.IsEmpty then SimulatorTypes.Zero::lst else lst
        | _ -> bitstring_ (num/2) ((bitFromInt (num%2))::lst)

    bitstring_ num []

let tableLHS (inputs: SimulatorTypes.SimulationIO list): TruthTableRow list = 
    let numRows = int (2.0**(float inputs.Length))

    [0 .. numRows-1]
    |> List.map bitstring
    |> List.map (fun l -> [for i in 1..(inputs.Length - l.Length) do SimulatorTypes.Zero])
    |> List.map (fun row -> List.map2 tableCell inputs row)

let rowRHS 
    (lhs: TruthTableRow) 
    (outputs: SimulatorTypes.SimulationIO list) 
    (simData: SimulatorTypes.SimulationData)
    : TruthTableRow = 
    let updateOutputs (cell: TruthTableCell) = 
        let ((cid,_,_),bit) = cell
        Fast.changeInput cid [bit] simData.ClockTickNumber simData.FastSim
    let _ = List.map updateOutputs lhs

    (outputs,simData)
    ||> Fast.extractFastSimulationIOs
    |> List.map (fun (comp,wd) -> (comp,wd.Head))

let truthTable 
    (inputs: SimulatorTypes.SimulationIO list) 
    (outputs: SimulatorTypes.SimulationIO list)
    (simData: SimulatorTypes.SimulationData)
    : TruthTableStruct = 

    let lhs = tableLHS inputs
    let rhs = List.map (fun i -> rowRHS i outputs simData) lhs

    let table : Map<TruthTableRow list,TruthTableRow list> = Map.empty

    List.zip lhs rhs
    |> Map.ofList

