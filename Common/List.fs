module List

let intersect list1 list2 =
    list1 |> List.filter (fun i -> List.contains i list2)

let rotate count list =
    List.skip count list @ List.take count list
