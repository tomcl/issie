# Implemented Functionality

## Smart Custom Symbol Sizing
### 1) Auto

Given two connected symbols, resize the one selected second to make the port gaps on both symbols equal, and reposition the symbol so wires the are straight. Invoke using menu dropdown (Edit > TestResize). 

- works for all orientations
- 'works' for all components in the sense that the algorithm is applied for all component, may have some unexpected behaviour for special edge cases
- does not have expected behaviour if ports are 'tangled', recommended to run Smart Port Reordering before this feature
- does not check for overlapping with other symbols after resize and reposition
- minimal error checking done purposefully as this is meant to be manual feature requiring careful user input, for safer but less ambitious cleanup use Smart Beautify

### 2) Manual

Hold `Ctrl` near a custom component to highlight its four corners. Drag a corner in any direction to resize or mirror the component. UI implemented to visualise the resize action.

- component is returned to original position if resizing causes it to overlap with other components

## Smart Port Reordering
- Smart Port Reordering is invoked by key binding `CtrlOrCmd+R` or menu dropdown (Edit > Sheet > Reorder Ports).
- Reorders ports such that wire nets do not cross each other. 
    - Limitation: Ports may be rearranged even if no nets cross each other.
    - Limitation: Ordering of Ports only work if wires come from one other component.
- 'Flips' non custom components with two IO ports on an edge (eg. Mux2, Demux2, And, etc.) to minimize wire crossings. 
    - Limitation: Flips only work if wires come from one other component.

## Smart Beautify
- Heuristic based algorithm to cleanup the whole sheet is invoked using menu dropdown (Edit > Sheet Beautify) or `CtrlOrCmd+B`.
- Applies smart port reordering on 10 pairs of components that require the most swaps.
- Applies smart size and respositioning to custom components if the operation does not cause symbols to overlap.
     - Limitation: Repositioning may move components too much. Cases which produce undesirable results have not been identified yet.
