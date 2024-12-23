module Map

let merge f m1 m2 =
    (m1, m2)
    ||> Map.fold (fun m k v1 ->
        m
        |> Map.change k (fun e ->
            match e with
            | Some v2 -> Some(f v1 v2)
            | None -> Some v1))

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Merge`` () =
        let m1 = Map.ofList [ "a", 1; "b", 2 ]
        let m2 = Map.ofList [ "b", 3; "c", 4 ]

        let m = merge (+) m1 m2

        m |> should equal (Map.ofList [ "a", 1; "b", 5; "c", 4 ])
