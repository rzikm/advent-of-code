module Geometry

open FSharpPlus

// calculates the area of a polygon with axis-aligned edges including the border
let inline axisAlignedPolygonArea points =
    // use shoelace formula to calculate area
    let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne

    let edges = Seq.append points (Seq.singleton (Seq.head points)) |> Seq.pairwise
    let circumference = Seq.sumBy (uncurry Tuple2.manhattanDist) edges
    let area2 = Seq.sumBy (fun ((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1) edges

    area2 / two + circumference / two + LanguagePrimitives.GenericOne
