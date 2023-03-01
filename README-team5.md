## Contributions to Modules

### **Omar (HLP23: Author Omar):** 
SmartWire, BusWireUpdate 
 
#### Notes: ####
- Wires with Top output port edge exhibit weird behaviour with regardfs to routing, as the smartwire module thinks there is a symbol in teh way since middlecondition is met, however, when tightening the constraint of the middlecondition to only include wires with output port edge being left or right, this was not possible since there is a bug with the wire output port edge not being stored in the orientation and order maps. - Will be fixed in group phase.
- Group project work to implement wire labels during wire updates - updateWire needs to be able to return ModelT type as well (using smartautoroute D.U. return type I created). Chose not to implement in updateWires as I have to change the type of references to updateWire which stretches to SmartPortOrder, which was assigned to Indraneel, which would affect his implementation in his individual code. Hence, I chose to implement this advancement with him in the group project phase so that we can collaborate directly on his SmartPortOrder file.

### **Indraneel (HLP23: Author Indraneel):** 
SmartPortOrder

### **Shaanuka (HLP23: Author Shaanuka):** 
SymbolView, Renderer, DrawHelpers, DrawModelType, SmartSizeSymbol, Style.

### **Ifte (HLP23: Author Ifte):** 
SmartSizeSymbol

