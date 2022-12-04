module List

let intersect list1 list2 =
    list1 |> List.filter (fun i -> List.contains i list2)
