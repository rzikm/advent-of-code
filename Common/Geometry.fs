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

let inline lineIntersects2d ((x0, y0), (dx0, dy0)) ((x1, y1), (dx1, dy1)) =
    // see https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
    // rename using names from the above link

    let x1, x2, x3, x4 = x0, x0 + dx0, x1, x1 + dx1
    let y1, y2, y3, y4 = y0, y0 + dy0, y1, y1 + dy1

    let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if denominator = LanguagePrimitives.GenericZero then
        None
    else
        let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator
        let u = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / denominator

        let pos = Tuple2.add (x1, y1) (Tuple2.scale t (dx0, dy0))

        (pos, t, u) |> Some
