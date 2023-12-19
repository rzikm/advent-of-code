module Lens

open FSharpPlus.Lens

module Internals =
    [<Struct>]
    type VMap<'t, 'u> = VMap of 't * ('t -> 'u)

    module VMap =
        let run (VMap (x, _)) = x

    type VMap<'t, 'u> with

        static member Return x = VMap x

        static member inline Map(VMap (x, ff), f: 'T -> 'U) =
            VMap(ff x |> FSharpPlus.Control.Map.Invoke f, (fun _ -> failwith "not implemented"))

open Internals

let lensMap (optic: ('a -> VMap<'a, 'c>) -> _ -> VMap<'t, 'u>) f source =
    VMap.run (optic (fun c -> VMap(c, f)) source)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``lensMap`` () =
        let f =
            function
            | 0 -> None
            | x -> Some(2 * x)

        lensMap _1 f (1, 2) |> should equal <| Some(2, 2)
        lensMap _1 f (0, 2) |> should equal None
