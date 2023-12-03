module AoC202012

open AdventOfCode
open FSharpPlus
open FParsec

type Action =
    | North
    | South
    | East
    | West
    | Left
    | Right
    | Forward

let parser =
    let paction =
        choice [ charReturn 'N' North
                 charReturn 'S' South
                 charReturn 'E' East
                 charReturn 'W' West
                 charReturn 'L' Left
                 charReturn 'R' Right
                 charReturn 'F' Forward ]
        .>>. pint32

    sepEndBy paction (pchar '\n')

let easy input =
    let rec f (east, north, head) (action, arg) =
        match action with
        | North -> (east, north + arg, head)
        | South -> (east, north - arg, head)
        | East -> (east + arg, north, head)
        | West -> (east - arg, north, head)
        | Left -> (east, north, (head + 360 - arg) % 360)
        | Right -> (east, north, (head + arg) % 360)
        | Forward ->
            match head with
            | 0 -> f (east, north, head) (East, arg)
            | 90 -> f (east, north, head) (South, arg)
            | 180 -> f (east, north, head) (West, arg)
            | 270 -> f (east, north, head) (North, arg)
            | _ -> failwith "Invalid heading"

    let (east, north, _) = input |> List.fold f (0, 0, 0)
    abs east + abs north

let hard input =
    let rec f (east, north, waypointEast, waypointNorth) (action, arg) =
        match action with
        | North -> (east, north, waypointEast, waypointNorth + arg)
        | South -> (east, north, waypointEast, waypointNorth - arg)
        | East -> (east, north, waypointEast + arg, waypointNorth)
        | West -> (east, north, waypointEast - arg, waypointNorth)
        | Forward -> (east + arg * waypointEast, north + arg * waypointNorth, waypointEast, waypointNorth)
        | Left -> f (east, north, waypointEast, waypointNorth) (Right, -arg)
        | Right ->
            match (arg + 360) % 360 with
            | 0 -> (east, north, waypointEast, waypointNorth)
            | 90 -> (east, north, waypointNorth, -waypointEast)
            | 180 -> (east, north, -waypointEast, -waypointNorth)
            | 270 -> (east, north, -waypointNorth, waypointEast)
            | _ -> failwith "Invalid heading"

    let (east, north, _, _) = input |> List.fold f (0, 0, 10, 1)
    abs east + abs north

let solution = makeSolution () parser easy hard
