module AoC202123

open AdventOfCode
open FSharpPlus
open FParsec

type Amphipod =
    | Amber
    | Bronze
    | Copper
    | Desert

type State = { hallway: Amphipod option []; rooms: Amphipod list [] }

let parser =
    let pAmphipodType c t = pchar c >>. preturn t

    let pAmphipod =
        choice [ pAmphipodType 'A' Amber
                 pAmphipodType 'B' Bronze
                 pAmphipodType 'C' Copper
                 pAmphipodType 'D' Desert ]

    let pNewline = pchar '\n'
    let pTop = pstring "#############"
    let pHallway = pstring "#...........#"
    let pUpper = pstring "###" >>. parray 4 (pAmphipod .>> pchar '#') .>> pstring "##"
    let pLower = pstring "  #" >>. parray 4 (pAmphipod .>> pchar '#')
    let pBottom = pstring "  #########"

    let initState (top, bottom) =
        let pick i =
            [ (Array.item i top); (Array.item i bottom) ]

        { hallway = Array.init 7 (fun _ -> None)
          rooms = [| pick 0; pick 1; pick 2; pick 3 |] }

    pTop >>. pNewline >>. pHallway .>> pNewline >>. pUpper .>> pNewline .>>. pLower .>> pNewline .>> pBottom
    |>> initState

let roomIndexToAmphipod =
    function
    | 0 -> Amber
    | 1 -> Bronze
    | 2 -> Copper
    | _ -> Desert

let getTargetRoomIndex =
    function
    | Amber -> 0
    | Bronze -> 1
    | Copper -> 2
    | Desert -> 3

let getStepCost =
    function
    | Amber -> 1
    | Bronze -> 10
    | Copper -> 100
    | Desert -> 1000

let hallwaySpaceHorizontalOffset =
    function
    // #############
    // #01.2.3.4.56#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########"
    | 0 -> 0
    | 1 -> 1
    | 2 -> 3
    | 3 -> 5
    | 4 -> 7
    | 5 -> 9
    | 6 -> 10
    | _ -> failwith "Invalid space index"

let roomHorizontalOffset =
    function
    // #############
    // #..0.1.2.3..#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########"
    | 0 -> 2
    | 1 -> 4
    | 2 -> 6
    | 3 -> 8
    | _ -> failwith "Invalid room index"

let getNeighbors roomSize (state: State) =
    // only one of the following can happen
    //  - an amphipod exits a room to the hallway
    //  - an amphipod enters the target room from the hallway

    seq {
        // check exiting from a room from which an amphipod can leave
        for roomIdx in 0..3 do
            let room = state.rooms.[roomIdx]

            match room with
            // can only leave if any amph in the room should not be there
            | amph :: otherAmphs when room |> List.exists (getTargetRoomIndex >> (<>) roomIdx) ->
                // check all destination spaces on the hallway
                let verticalDist = roomSize - List.length otherAmphs

                let candidates =
                    Seq.concat [
                                 // left side
                                 seq { 0 .. (roomIdx + 1) }
                                 |> Seq.rev
                                 |> Seq.takeWhile (fun i -> Array.item i state.hallway |> Option.isNone)

                                 // right side
                                 seq { (roomIdx + 2) .. 6 }
                                 |> Seq.takeWhile (fun i -> Array.item i state.hallway |> Option.isNone) ]

                for hallwaySpaceIndex in candidates do
                    let horizontalDist =
                        abs (roomHorizontalOffset roomIdx - hallwaySpaceHorizontalOffset hallwaySpaceIndex)

                    let cost = getStepCost amph * (verticalDist + horizontalDist)

                    yield
                        { state with
                            hallway = Array.updateAt hallwaySpaceIndex (Some amph) state.hallway
                            rooms = Array.updateAt roomIdx otherAmphs state.rooms },
                        cost
            | _ -> ()

        // check entering a room
        for space in state.hallway |> Seq.indexed do
            match space with
            | _, None -> ()
            | hallwaySpaceIndex, Some amph ->
                let targetRoomIdx = getTargetRoomIndex amph
                let roomOffset = roomHorizontalOffset targetRoomIdx
                let startOffset = hallwaySpaceHorizontalOffset hallwaySpaceIndex
                let room = state.rooms.[targetRoomIdx]

                if List.length room < roomSize && List.forall (getTargetRoomIndex >> (=) targetRoomIdx) room then
                    let spacesThatMustBeEmpty =
                        if startOffset < roomOffset then
                            // going to the right, last index to check is (1 + targetRoomIdx)
                            [ (1 + hallwaySpaceIndex) .. (1 + targetRoomIdx) ]
                        else
                            // going to the left, last index to check is (2 + targetRoomIdx)
                            [ (2 + targetRoomIdx) .. (hallwaySpaceIndex - 1) ]

                    if List.forall (fun i -> Array.item i state.hallway |> Option.isNone) spacesThatMustBeEmpty then
                        let horizontalDist = abs (roomOffset - startOffset)
                        let verticalDist = roomSize - List.length room
                        let cost = getStepCost amph * (horizontalDist + verticalDist)

                        yield
                            { hallway = Array.updateAt hallwaySpaceIndex None state.hallway
                              rooms = Array.updateAt targetRoomIdx (amph :: room) state.rooms },
                            cost
    }

let printState roomSize (state: State) =
    let amphToString =
        function
        | Amber -> "A"
        | Bronze -> "B"
        | Copper -> "C"
        | Desert -> "D"

    let getRoomLineSegment item list =
        list |> List.tryItem (List.length list - roomSize + item) |> Option.map amphToString |> Option.defaultValue "."

    // #############
    // #...D.A.....#
    // ###.#.#.#.###
    //   #.#.#.#.#
    //   #########"
    printfn "#############"

    printfn
        "#%s#"
        (state.hallway
         |> Seq.map (Option.map amphToString >> Option.defaultValue ".")
         |> Seq.insertAt 2 "."
         |> Seq.insertAt 4 "."
         |> Seq.insertAt 6 "."
         |> Seq.insertAt 8 "."
         |> String.concat "")

    printfn "###%s###" (state.rooms |> Seq.map (getRoomLineSegment 0) |> String.concat "#")

    for item in 1 .. (roomSize - 1) do
        printfn "  #%s#" (state.rooms |> Seq.map (getRoomLineSegment item) |> String.concat "#")

    printfn "  #########"
    printfn ""

let fHeuristic roomSize (state: State) =
    state.rooms
    |> Seq.indexed
    |> Seq.sumBy (fun (i, room) ->
        let size = List.length room

        // cost of filling the room with correct amphipods
        let countToGo =
            roomSize - (room |> Seq.filter (getTargetRoomIndex >> (=) i) |> Seq.length)

        let fillingCost =
            countToGo * (countToGo - 1) * (roomIndexToAmphipod i |> getStepCost)

        // cost of removing unnecessary amphipods
        let costOfRemoval =
            room
            |> Seq.indexed
            |> Seq.filter (snd >> getTargetRoomIndex >> (<>) i)
            |> Seq.sumBy (fun (i, amph) -> (roomSize - size + i + 1) * getStepCost amph)

        fillingCost + costOfRemoval)

let solve1 input =
    let finish =
        { hallway = Array.replicate 7 None
          rooms =
            [| List.replicate 2 Amber
               List.replicate 2 Bronze
               List.replicate 2 Copper
               List.replicate 2 Desert |] }

    Graph.aStar (fHeuristic 2) (getNeighbors 2) ((=) finish) [ input ] |> snd

let solve2 input =
    let finish =
        { hallway = Array.replicate 7 None
          rooms =
            [| List.replicate 4 Amber
               List.replicate 4 Bronze
               List.replicate 4 Copper
               List.replicate 4 Desert |] }

    let newStart =
        { input with
            rooms =
                [| List.insertManyAt 1 [ Desert; Desert ] input.rooms.[0]
                   List.insertManyAt 1 [ Copper; Bronze ] input.rooms.[1]
                   List.insertManyAt 1 [ Bronze; Amber ] input.rooms.[2]
                   List.insertManyAt 1 [ Amber; Copper ] input.rooms.[3] |] }

    Graph.aStar (fHeuristic 4) (getNeighbors 4) ((=) finish) [ newStart ] |> snd

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let private parse input =
        match FParsec.CharParsers.run parser input with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwith err

    let input =
        [| "#############"
           "#...........#"
           "###B#C#B#D###"
           "  #A#D#C#A#"
           "  #########" |]

    [<Fact>]
    let ``Generates steps from room to the left`` () =
        // from following state
        //
        // #############
        // #.....B.....#
        // ###A#D#.#.###
        //   #A#D#.#.#
        //   #########"

        let state =
            { hallway = [| None; None; None; Some Bronze; None; None; None |]
              rooms = [| [ Amber; Amber ]; [ Desert; Desert ]; []; [] |] }

        // we expect to go states with topmost D in places 1, 2, and 3 respectively
        //
        // #############
        // #32.1.B.....#
        // ###A#.#.#.###
        //   #A#D#.#.#
        //   #########"

        let newRooms = state.rooms |> Array.updateAt 1 (List.tail state.rooms.[1])

        getNeighbors 2 state
        |> List.ofSeq
        |> should
            equal
            [ ({ hallway = [| None; None; Some Desert; Some Bronze; None; None; None |]
                 rooms = newRooms },
               2000)
              ({ hallway = [| None; Some Desert; None; Some Bronze; None; None; None |]
                 rooms = newRooms },
               4000)
              ({ hallway = [| Some Desert; None; None; Some Bronze; None; None; None |]
                 rooms = newRooms },
               5000) ]

    [<Fact>]
    let ``Generates step from room to the right`` () =
        // from following state
        //
        // #############
        // #.....B.....#
        // ###A#B#.#D###
        //   #A#B#A#D#
        //   #########"

        let state =
            { hallway = [| None; None; None; Some Bronze; None; None; None |]
              rooms = [| [ Amber; Amber ]; [ Bronze; Bronze ]; [ Amber ]; [ Desert; Desert ] |] }

        // we expect to go states with A in places 1, 2, and 3 respectively
        //
        // #############
        // #.....B.1.23#
        // ###A#B#.#D###
        //   #A#B#.#D#
        //   #########"

        let newRooms = state.rooms |> Array.updateAt 2 (List.tail state.rooms.[2])

        getNeighbors 2 state
        |> List.ofSeq
        |> should
            equal
            [ ({ hallway = [| None; None; None; Some Bronze; Some Amber; None; None |]
                 rooms = newRooms },
               3)
              ({ hallway = [| None; None; None; Some Bronze; None; Some Amber; None |]
                 rooms = newRooms },
               5)
              ({ hallway = [| None; None; None; Some Bronze; None; None; Some Amber |]
                 rooms = newRooms },
               6) ]

    [<Fact>]
    let ``Generates step to the room`` () =
        // from following state
        //
        // #############
        // #.......B...#
        // ###.#.#.#.###
        //   #.#.#.#.#
        //   #########"

        let state =
            { hallway = [| None; None; None; None; Some Bronze; None; None |]
              rooms = [| []; []; []; [] |] }

        // we expect just the following state
        //
        // #############
        // #...........#
        // ###.#.#.#D###
        //   #.#B#.#D#
        //   #########"


        getNeighbors 2 state
        |> List.ofSeq
        |> should
            equal
            [ ({ hallway = [| None; None; None; None; None; None; None |]
                 rooms = state.rooms |> Array.updateAt 1 (Bronze :: state.rooms.[1]) },
               50) ]

    [<Fact>]
    let ``Does not travel through other aphipods`` () =
        // from following state
        //
        // #############
        // #...D.A.....#
        // ###.#.#.#.###
        //   #.#.#.#.#
        //   #########"

        let state =
            { hallway = [| None; None; Some Desert; Some Amber; None; None; None |]
              rooms = [| []; []; []; [] |] }

        // we expect no transitions
        getNeighbors 2 state |> should be Empty

    [<Fact>]
    let ``Example Part 1`` () =
        testPart1 solution input |> should equal 12521

    [<Fact>]
    let ``Example Part 2`` () =
        testPart2 solution input |> should equal 44169
