module SheetBeautify
open DrawModelType
let beatifySheet (sheet: SheetT.Model) =
    sheet
    |> SheetBeautifyD2.optimizePortOrder
    |> SheetBeautifyD1.sheetAlignScale 3
    |> SheetBeautifyD3.wireLabelBeautify