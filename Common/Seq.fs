module Seq

let loop s =
    seq {
        while true do
            yield! s
    }
