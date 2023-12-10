module List

let intersect list1 list2 =
    list1 |> List.filter (fun i -> List.contains i list2)

let rotate count list =
    List.skip count list @ List.take count list

let count predicate list =
    List.fold (fun count item -> if predicate item then count + 1 else count) 0 list
    
