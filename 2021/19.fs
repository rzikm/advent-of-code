module AoC202119

open AdventOfCode
open FSharpPlus
open FParsec

type Vector = (struct (int * int * int))
type Scanner = int * (Vector list)
type OrientedScanner = int * (Vector list) * Vector

let parser =
    let pNewline = pchar '\n'

    let pBeacon =
        pint32 .>> pchar ',' .>>. pint32 .>> pchar ',' .>>. pint32 |>> (fun ((x1, x2), x3) -> Vector(x1, x2, x3))

    let pScanner =
        pstring "--- scanner " >>. pint32 .>> pstring " ---" .>> pNewline .>>. (sepEndBy pBeacon pNewline |>> List.sort)
        |>> Scanner

    sepEndBy pScanner pNewline

let allOrientations: (Vector -> Vector) list =
    [
      // facing X
      fun struct (x, y, z) -> (x, y, z)
      fun struct (x, y, z) -> (x, -z, y)
      fun struct (x, y, z) -> (x, -y, -z)
      fun struct (x, y, z) -> (x, z, -y)

      // facing -X
      fun struct (x, y, z) -> (-x, z, y)
      fun struct (x, y, z) -> (-x, -y, z)
      fun struct (x, y, z) -> (-x, -z, -y)
      fun struct (x, y, z) -> (-x, y, -z)

      // facing Y
      fun struct (x, y, z) -> (y, z, x)
      fun struct (x, y, z) -> (y, -x, z)
      fun struct (x, y, z) -> (y, -z, -x)
      fun struct (x, y, z) -> (y, x, -z)

      // facing -Y
      fun struct (x, y, z) -> (-y, x, z)
      fun struct (x, y, z) -> (-y, -z, x)
      fun struct (x, y, z) -> (-y, -x, -z)
      fun struct (x, y, z) -> (-y, z, -x)

      // facing Z
      fun struct (x, y, z) -> (z, x, y)
      fun struct (x, y, z) -> (z, -y, x)
      fun struct (x, y, z) -> (z, -x, -y)
      fun struct (x, y, z) -> (z, y, -x)

      // facing -Z
      fun struct (x, y, z) -> (-z, y, x)
      fun struct (x, y, z) -> (-z, -x, y)
      fun struct (x, y, z) -> (-z, -y, -x)
      fun struct (x, y, z) -> (-z, x, -y) ]

let vectorAdd ((x0, y0, z0): Vector) ((x1, y1, z1): Vector) = Vector(x0 + x1, y0 + y1, z0 + z1)
let vectorSub ((x0, y0, z0): Vector) ((x1, y1, z1): Vector) = Vector(x0 - x1, y0 - y1, z0 - z1)
let vectorMannhattan ((x0, y0, z0): Vector) = abs x0 + abs y0 + abs z0

let compare ((x0, y0, z0): Vector) ((x1, y1, z1): Vector) =
    let cx = x0.CompareTo(x1)

    if cx <> 0 then
        cx
    else
        let cy = y0.CompareTo(y1)
        if cy <> 0 then cy else z0.CompareTo(z1)

let tryOrientScanner (_, scanner0, _) (s1i, scanner1) =
    allOrientations
    |> List.tryPick (fun orient ->
        let scanner1Sorted = scanner1 |> Seq.map orient |> Seq.sort |> Seq.toList

        // we try to brute-force match elements from the start since we need at least 12 matches, we
        // can drop last 11 from each list from the search
        let probes0 = Seq.take (List.length scanner0 - 11) scanner0
        let probes1 = Seq.take (List.length scanner1 - 11) scanner1Sorted

        Seq.allPairs probes0 probes1
        |> Seq.tryPick (fun (b0, b1) ->
            // delta from scanner0 to scanner1
            let delta = vectorSub b0 b1

            // shift all beacons from scanner1 in direction of scanner0
            let scanner1Shifted = scanner1Sorted |> List.map (fun b -> vectorAdd b delta)

            // since both lists are sorted, we can use more efficient intersecting algorithm in O(n + m)
            let rec intersectCount (left: Vector list) (right: Vector list) count =
                match left, right with
                | [], _ -> count
                | _, [] -> count
                | l :: lt, r :: rt ->
                    match compare l r with
                    | 0 -> intersectCount lt rt (count + 1)
                    | x when x < 0 -> intersectCount lt right count
                    | _ -> intersectCount left rt count

            if intersectCount scanner0 scanner1Shifted 0 >= 12 then
                Some(s1i, scanner1Shifted, delta)
            else
                None))

let collectRemove picker list =
    let rec collectRemove' picked notPicked picker =
        function
        | [] -> picked, notPicked
        | h :: tail ->
            match picker h with
            | Some r -> collectRemove' (r :: picked) notPicked picker tail
            | None -> collectRemove' picked (h :: notPicked) picker tail

    collectRemove' [] [] picker list

let orientAll (scanners: Scanner list) =
    let rec orientAll' (orientUsing: OrientedScanner list) (oriented: OrientedScanner list) (toOrient: Scanner list) =
        match orientUsing, toOrient with
        | _, [] -> orientUsing @ oriented
        | [], _ -> failwith "Failed to orient all scanners"
        | scanner :: nextOrientUsing, _ ->
            let newlyOriented, newToOrient =
                toOrient |> collectRemove (tryOrientScanner scanner)

            orientAll' (nextOrientUsing @ newlyOriented) (scanner :: oriented) newToOrient

    let (s0, beacons0) = List.head scanners
    orientAll' [ (s0, beacons0, Vector(0, 0, 0)) ] [] (List.tail scanners)

let solve1 (input: Scanner list) =
    input |> orientAll |> List.map (fun (_, beacons, _) -> Set.ofList beacons) |> Set.unionMany |> Set.count

let solve2 (input: Scanner list) =
    let oriented = orientAll input

    List.allPairs oriented oriented
    |> List.map (fun ((_, _, a), (_, _, b)) -> vectorSub a b |> vectorMannhattan)
    |> List.max

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let private parse input =
        match run parser (String.concat "\n" input) with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwith err

    let input =
        [| "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14" |]

    [<Fact>]
    let ``Orients example scanners correctly`` () =
        let expectedPositions =
            [ (0, struct (0, 0, 0))
              (1, struct (68, -1246, -43))
              (2, struct (1105, -1205, 1229))
              (3, struct (-92, -2380, -20))
              (4, struct (-20, -1133, 1061)) ]

        input
        |> parse
        |> orientAll
        |> List.sort
        |> List.map (fun (i, _, pos) -> (i, pos))
        |> should equal expectedPositions

    [<Fact>]
    let ``Example Part 1`` () =
        testPart1 solution input |> should equal 79

    [<Fact>]
    let ``Example Part 2`` () =
        testPart2 solution input |> should equal 3621
