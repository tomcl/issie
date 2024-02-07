# TBL1
Team Name: *#est tool in the shed*

## Flip Function

### Call Tree

```
Flip
--> Rotate.flipBlock
    |-> getBlock
    |   --> getRotateHandW (4x)
    |       --> getCompRotateHandWS
    |-> flipSymbolinBlock
        |-> getRotateHandW
        |   --> (...)
        |-> flipPointAboutBlockCentre
        |-> AdjustPosForBlockFlip
        |-> SymbolResizeHelper.flipSideHorizontal (flipPortlist)
        |   --> rotateSide
        |-> flipPortList (from flipSymbolinBlock)
        |-> Symbol.calcLabelBoundingBox
            |-> CombineRoation
            |-> getRotateHandW
            |   --> (...)
            |-> getRotatedSymbolCentre
            |   --> getRotateCompCentre
            |       --> getRotateCompHandW
            |-> DrawHelperTextWidthInPixels
            |   --> canvasWidth.measureText
            |       --> ( browser )
            |-> getBlock
            |   --> (...)
            |-> CommonType.boundingBox.centre
            |-> RotateSymbolinBlock
... continues
```

## Rotate Function

### Call Tree

```
Rotate
-----
    RotateScale.rotateBlock
    -----
    |   getBlock
    |   -----
    |   |   getRotatedHAndW
    |   |   -----
    |   |   |   getCompRotatedHAndW
    |   |
    |   rotateSymbolInBlock
    |   -----
    |   |   getRotatedHAndW
    |   |   -----
    |   |       getCompRotatedHAndW
    |   |
    |   -----
    |   |   rotatePointAboutBlockCentre
    |   |
    |   -----
    |   |   invertRotation
    |   |
    |   -----
    |   |   adjustPosForBlockRotation
    |   -----
    |   |   invertRotation
    |   -----
    |   |   combineRotation
    |   -----
    |   |   invertRotation
    |   -----
    |   |   combineRotation
    |   -----
    |   |   rotatePortInfo
    |   |   -----
    |   |       rotateSide
    |   |       rotatePortList
    |   |
    |   -----
    |       calcLabelBoundingBox
    |       -----
    |       |   combineRotation
    |       |
    |       -----
    |       |   getRotatedHAndW
    |       |   -----
    |       |       getCompRotatedHAndW
    |       |
    |       -----
    |       |   getRotatedSymbolCentre
    |       |   -----
    |       |   |   getRotatedCompCentre
    |       |   |   -----
    |       |   |       getRotatedHAndW
    |       |   |       -----
    |       |   |           getCompRotatedHAndW
    |       |    
    |       -----
    |           getTextWidthInPixels
    |
    appendUndoList
    -----
    |   recursive call removeLast
    |
    Symbol.getBoundingBoxes
    -----
    |   getSymbolBoundingBox
    |   -----
    |       getRotatedHAndW
    |       -----
    |           getCompRotatedHAndW
    notIntersectingComponents
    -----
    |   boxesIntersect
    |   -----
    |
    # BEGIN Message Handling
    |
    symbolCmd
    |
    wireCmd
    |
    sheetCmd
```

### Implementation

The rotation begins by getting the original unaltered block—this is the role of the `getBlock` function. This is used to get the centre of the block prior to rotation

To perform `getBlock`, the function `getRotatedHAndW` which calls `getCompRotatedHAndW` is called. These function take into account the effect of rotation on a block. The rotation state is not captured by the x and y position of the block, it is a separate proporty.

The new, rotated symbol is created by the `rotateSymbolInBlock` function. There are two possible rotations and two possible flip states, giving all four possibilites. The match which performs the `combineRotation` checks which need to be changed.

The bounding box is updated by `calcLabelBoundingBox`.

The undo list is preserved by keeping the unrotated model in the undo list.

Errors are generated if bounding boxes intersect, this is done by `notIntersectingComponents`.

Finally, messages are handled if there are errors.

## Scaling

### Call Stack

```
Scaling ->
  findSelectedSymbols
  xYSC
  getScalingFcactorAndOffsetCentreGroup ->
    getRotatedHAndW ->
      getCompRotatedHAndW
    getRotatedSymbolCentre ->
      getRotatedCompCentre ->
        getCompRotatedHAndW
    getScalingFactorAndOffsetCentre 
  scaleSymFunc
  scaleSymbol ->
    getRotatedSymbolCentre ->
      getRotatedCompCentre ->
        getCompRotatedHAndW
    getRotatedHAndW ->
      getCompRotatedHAndW
  newSymmModel 
  groupNewSelectedSymmModel ->
    scaleSymFunc 
  newmodel ->
    newSymmModel ->
      scaleSymmFunc ->
        xYSC
```