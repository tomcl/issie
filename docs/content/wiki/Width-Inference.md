# Introduction

Width inference is one of the major advantages of ISSIE. Based on the components used in a sheet, ISSIE propagates the correct width to the output ports of a component and to all wires connected to each output port. Whenever a new connection is made, ISSIE re-runs the width inference function and checks for possible errors in the circuit.

Errors are of type:
```fs
type WidthInferError = {
  Msg : string
  ConnectionsAffected : ConnectionId list
}
```
In case of an error, ISSIE will display the `Msg` (Message) to the user, and highlight (change color to red) the `ConnectionsAffected` (wires). This makes the error-correction process very easy as it is clear which connections are affected by the error, and the message gives all the necessary information to correct the error. 

## Possible Errors

ISSIE checks for the following errors:

1. Wrong signal width on component input port (e.g. connected a 2-bit signal to the Cin port of an adder)
2. Wrong signal width on mergeWires, SplitWire, BusSelect (e.g. spliWire which splits the wire to X and Y bit signals, expects the input to be at least >=(Y+1) bits. Returns a message of type "expecting at least (Y+1) bits")
3. Non-equal width of inputs on variable width components (e.g. MUX,N-bits XOR, etc.)
4. Wire ends up having 2 different widths (possibly because of a combinatorial loop)
5. Input port has more than 1 wire connected to it
6. Wire label has more than 1 driving components (P.S. wire labels with the same name represent the same signal)


## Procedure

The main width inference function is called `inferConnectionsWidth`. It has one parameter: the current `CanvasState`
```fs
(comps: Component list, conns: Connection list) : CanvasState
```

## Running from BusWire message

When editing (before starting simulation) the bus `inferconnectionsWidth` must be run whenever an edit change is made that alters the circuit correctness or bus widths.

BusWire message `BusWidths` (full name `DrawModelType.BusWireT.Msg.BusWidths`) implements this and updates the circuit error status. Thus, any operation that changes the current sheet in a way that affects correctness or bus widths should dispatch this message after it completes. Note that changing the name of a non-IOLabel component does NOT chnage the circuit in a significant way and therefore does not run `BusWidths`.

Procedure of `inferConnectionsWidth`:

1. Initially an empty Map is created where the value of all connections is set to None. This Map will be updated as the algorithm traverses through the components and the connections of a sheet.
```fs
type ConnectionsWidth = Map<ConnectionId, int option>
```
2. The following `staticMaps` are created to allow fast lookup of connections and components
```fs
(inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>),
(outputPortIdsToConnections : Map<OutputPortId, Connection list>),
(compIdsToComps : Map<ComponentId, Component>),
(outputPortsOfBusLabels: Map<string,OutputPortId list>)
```
3. The function iterates over the list of `comps` and updates `connectionsWidth` in the process by using the function `infer`
4. `infer` function does the following:
   1. For all input ports of the component: Obtains the width of the wire connecting to them, and the ConnectionId of such wire
   2. Checks the inputs are correct
   3. Calculates the widths of all output ports of the component using `calculateOutputPortsWidth`:
      - Given a component and a set of input connection widths, check these inputs
        widths are as expected and try to calculate the width of the outgoing connections. Components can produce outputs as soon as they have enough info (e.g. gates can output width 1 straight away, PushToBusFirst can output n + 1 as soon as it finds out n, and so on). This is possible because setConnectionsWidth will make sure that we do not re-explore an already set connection. This should allow partially connected components to still work if they have already enough info.
   4. Then for each output:
       1. Get all connections that are connected to that port.
       2. Set the width of the connection to the inferred value. If the
         connection has already been inferred, it must be because of a loop. If the value is different, return error. If the value is the same, just ignore the connection otherwise you would get stuck in the
         loop.
       3. For the non-ignored connections, call again the recursive `infer` function on the
         `Target.HostId` (all components connected to the output ports of the current component). 