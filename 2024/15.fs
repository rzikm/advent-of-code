module AoC202415

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let map =
        let pWall = charReturn '#' true
        let pEmpty = charReturn '.' false

        let pBox =
            skipChar 'O' >>. getPosition .>>. getUserState
            >>= (fun (pos, (boxes, robot)) -> setUserState ((int pos.Column - 2, int pos.Line - 1) :: boxes, robot))
            >>% false

        let pRobot =
            skipChar '@' >>. getPosition .>>. getUserState
            >>= (fun (pos, (boxes, _)) -> setUserState (boxes, Some(int pos.Column - 2, int pos.Line - 1)))
            >>% false

        ParseUtils.grid (choice [ pWall; pEmpty; pBox; pRobot ])

    let dir =
        [ ('^', (0, -1)); ('v', (0, 1)); ('<', (-1, 0)); ('>', (1, 0)) ]
        |> List.map (fun (c, d) -> charReturn c d)
        |> choice

    tuple3 (map .>> spaces) (many1 (dir .>> spaces)) getUserState
    |>> fun (map, dirs, (boxes, robot)) -> ((map, boxes |> Set.ofList, Option.get robot), dirs)

let scorePos (x, y) = y * 100 + x

let move hitboxes (getWall, boxes, robot) dir =
    let boxFromPoint boxes pos =
        hitboxes |> List.tryPick (fun d -> Tuple2.sub pos d |> Option.returnIf (fun box -> Set.contains box boxes))

    let rec tryMakeRoom boxes pos =
        if getWall pos then
            None
        else
            match boxFromPoint boxes pos with
            | None -> Some(boxes)
            | Some box ->
                let nextPos = Tuple2.add box dir
                let boxes = Set.remove box boxes

                hitboxes
                |> List.map (Tuple2.add nextPos >> Some)
                |> List.fold (Option.bind2 tryMakeRoom) (Some boxes)
                |> Option.map (Set.add nextPos)

    match tryMakeRoom boxes (Tuple2.add robot dir) with
    | Some newBoxes -> (getWall, newBoxes, Tuple2.add robot dir)
    | None -> (getWall, boxes, robot)

let solve1 ((grid, boxes, robot), dirs) =
    let getWall pos = Array.item2dp pos grid
    let (_, boxes, _) = dirs |> Seq.fold (move [ (0, 0) ]) (getWall, boxes, robot)
    boxes |> Seq.sumBy scorePos

let solve2 ((grid, boxes, robot), dirs) =
    let getWall (x, y) = Array.item2dp (x / 2, y) grid
    let boxes = boxes |> Set.map (fun (x, y) -> x * 2, y)
    let robot = robot |> Tuple2.mapItem1 ((*) 2)

    let (_, boxes, _) =
        dirs |> Seq.fold (move [ (0, 0); (1, 0) ]) (getWall, boxes, robot)

    boxes |> Seq.sumBy scorePos

let solution = makeSolution ([], None) parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "##########"
           "#..O..O.O#"
           "#......O.#"
           "#.OO..O.O#"
           "#..O@..O.#"
           "#O#..O...#"
           "#O..O..O.#"
           "#.OO.O.OO#"
           "#....O...#"
           "##########"
           ""
           "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
           "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
           "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
           "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
           "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
           "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
           ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
           "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
           "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
           "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^" |]

    let input2 =
        [| "#######"
           "#...#.#"
           "#.....#"
           "#..OO@#"
           "#..O..#"
           "#.....#"
           "#######"
           ""
           "<vv<<^^<<^^" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 10092

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 9021
