module Array

open FSharpPlus

let mapAt i f array =
    Array.updateAt i (Array.item i array |> f) array

let item2d x y array = array |> Array.item y |> Array.item x

let item2dp (x, y) array = item2d x y array

let tryItem2d x y array =
    array |> Array.tryItem y |> Option.bind (Array.tryItem x)

let tryItem2dp (x, y) array = tryItem2d x y array

let neighbors2d8 x y array =
    Tuple2.neighbors8 (x, y) |> Seq.choose (fun (x, y) -> tryItem2d x y array)

let item3d x y z array =
    array |> Array.item z |> Array.item y |> Array.item x

let tryItem3d x y z array =
    array |> Array.tryItem z |> Option.bind (Array.tryItem y) |> Option.bind (Array.tryItem x)

let neighbors3d26 x y z array =
    Tuple3.neighbors26 (x, y, z) |> Seq.choose (fun (x, y, z) -> tryItem3d x y z array)

let item4d x y z w array =
    array |> Array.item w |> Array.item z |> Array.item y |> Array.item x

let tryItem4d x y z w array =
    array
    |> Array.tryItem w
    |> Option.bind (Array.tryItem z)
    |> Option.bind (Array.tryItem y)
    |> Option.bind (Array.tryItem x)

let neighbors4d x y z w array =
    seq {
        for ww in w - 1 .. w + 1 do
            for zz in z - 1 .. z + 1 do
                for yy in y - 1 .. y + 1 do
                    for xx in x - 1 .. x + 1 do
                        if xx <> x || yy <> y || zz <> z || ww <> w then
                            yield (xx, yy, zz, ww)
    }
    |> Seq.choose (fun (x, y, z, w) -> tryItem4d x y z w array)

let init2d lenX lenY f =
    Array.init lenY (fun y -> Array.init lenX (fun x -> f x y))

let map2d f array =
    let leny = Array.length array
    let lenx = Array.item 0 array |> Array.length

    init2d lenx leny (fun x y -> f x y (item2d x y array))

let rotate array =
    // Rotates 2d array clockwise
    let leny = Array.length array
    let lenx = Array.item 0 array |> Array.length

    init2d leny lenx (fun x y -> item2d y (leny - 1 - x) array)


module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Array.rotate rotates correctly`` () =
        let original = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |]

        let expected = [| [| 4; 1 |]; [| 5; 2 |]; [| 6; 3 |] |]

        rotate original |> should equal expected
