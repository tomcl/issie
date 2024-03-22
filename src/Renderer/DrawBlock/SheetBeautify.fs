module SheetBeautify
open DrawModelType
let beatifySheet (sheet: SheetT.Model) =
    sheet
    |> SheetBeautifyRotate.optimizePortOrder
    |> SheetBeautifyAlign.sheetAlignScale 3
    |> SheetBeautifyWireLabel.wireLabelBeautify