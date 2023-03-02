## Contributions to Modules

### **Omar (HLP23: Author Omar):** 
SmartWire, BusWireUpdate 
 
#### Notes: ####
- Group project work to implement wire labels during wire updates - updateWire needs to be able to return ModelT type as well (using smartautoroute D.U. return type I created). Chose not to implement in updateWires as I have to change the type of references to updateWire which stretches to SmartPortOrder, which was assigned to Indraneel, which would affect his implementation in his individual code. Hence, I chose to implement this advancement with him in the group project phase so that we can collaborate directly on his SmartPortOrder file.
- Wires with Top output port edge exhibit weird behaviour with regards to routing, as the smartwire module incorrectly detects that there is a symbol in the way since middlecondition is met, however, when tightening the constraint of the middlecondition to only include wires with output port edge being left or right, this was not possible since there is a bug with the wire output port edge not being stored in the orientation and order maps. - Will be fixed in group phase.

### **Indraneel (HLP23: Author Indraneel):** 
SmartPortOrder
#### Notes: #### 
The SmartPortOrdering can only re-order contigous sections of wires. For example: you can't have 2 wires with input port indexes [0,1] and the output ports[3,47]. They have to be contigously connected in output and input.

### **Shaanuka (HLP23: Author Shaanuka):** 
SymbolView, Renderer, DrawHelpers, DrawModelType, SmartSizeSymbol, Style.

#### For Markers: ####
As requested and accepted by Dr Clarke: Before running, change line 375 of SymbolView.fs from " let posThree = {X= midX+35.;Y=H/6.+5.} " to " let posThree = {X= midX+39.;Y=H/6.+5.} ".
This is a tiny offset change in float (35 to 39) that shifts the label to the right so no overlap occurs - Shaanuka.

### **Ifte (HLP23: Author Ifte):** 
SmartSizeSymbol

