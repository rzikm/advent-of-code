module AoC202322

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FParsec

let parser =
    let coord = tuple3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') pint32
    ParseUtils.lines (coord .>> pstring "~" .>>. coord)

let intersects (x0, x1) (y0, y1) =
    let xy0 = Tuple2.map2 max x0 y0
    let xy1 = Tuple2.map2 min x1 y1
    Tuple2.reduce (&&) <| (Tuple2.map2 (<=) xy0 xy1)

let getGroundOutline ((x0, y0, _), (x1, y1, _)) = (x0, y0), (x1, y1)

let getTop (_, (_, _, top)) = top
let getBottom ((_, _, bottom), _) = bottom

let placeBrick currentStack brick =
    let brickOutline = getGroundOutline brick

    let top =
        currentStack
        |> Seq.filter (fun b -> getGroundOutline b |> intersects brickOutline)
        |> Seq.map getTop
        |> Seq.fold max 0

    let delta = getBottom brick - top - 1
    (brick |> Tuple2.map (Tuple3.mapItem3 (fun z -> z - delta))) :: currentStack

let stackBricks input =
    input |> sortBy getBottom |> List.fold placeBrick []

let getBricksAbove stacked =
    Utils.memoize
    <| fun brick ->
        let outline = getGroundOutline brick

        stacked |> List.filter (fun bb -> getBottom bb = getTop brick + 1 && intersects (getGroundOutline bb) outline)

let getBricksBelow stacked =
    Utils.memoize
    <| fun brick ->
        let outline = getGroundOutline brick

        stacked |> List.filter (fun bb -> getTop bb = getBottom brick - 1 && intersects (getGroundOutline bb) outline)

let solve1 input =
    let stacked = stackBricks input
    let getBricksBelow = getBricksBelow stacked

    let unsafeToRemove =
        stacked |> Seq.choose (getBricksBelow >> Seq.tryExactlyOne) |> Seq.distinct |> Seq.length

    List.length input - unsafeToRemove

let solve2 input =
    let stacked = stackBricks input

    let getBricksAbove = getBricksAbove stacked
    let getBricksBelow = getBricksBelow stacked

    let getAllSupportedBricks brick =
        let rec loop acc newBricks =
            let acc = Set.union acc newBricks

            let next =
                newBricks
                |> Seq.collect (getBricksAbove >> Seq.filter (getBricksBelow >> Seq.forall (flip Set.contains acc)))
                |> Set.ofSeq

            if Set.isEmpty next then
                Set.count acc - 1
            else
                loop acc next

        loop Set.empty <| Set.singleton brick

    stacked |> Seq.sumBy getAllSupportedBricks

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "1,0,1~1,2,1"
           "0,0,2~2,0,2"
           "0,2,3~2,2,3"
           "0,0,4~0,2,4"
           "2,0,5~2,2,5"
           "0,1,6~2,1,6"
           "1,1,8~1,1,9"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 5

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 7
