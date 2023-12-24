module MatrixMath

let matrixMultiply a b =
    let (aCols, aRows) = Array.bounds2d a
    let (bCols, bRows) = Array.bounds2d b

    if aCols <> bRows then
        failwith "matrixMultiply: dimensions don't match"

    Array.init2d bCols aRows (fun row col -> seq { 0 .. aCols - 1 } |> Seq.sumBy (fun k -> a.[row].[k] * b.[k].[col]))

let matrixMultiplyVec a b =
    let (aCols, _) = Array.bounds2d a

    if aCols <> Array.length b then
        failwith "matrixMultiplyVec: dimensions don't match"

    Array.init aCols (fun row -> seq { 0 .. aCols - 1 } |> Seq.sumBy (fun k -> a.[row].[k] * b.[k]))

let findRoot (a: float array array) (b: float array) =
    // Ax = b

    let swap arr i1 i2 =
        let tmp = Array.item i1 arr
        arr.[i1] <- arr.[i2]
        arr.[i2] <- tmp

    for row = 0 to Array.length a - 1 do
        // find nonzero pivot
        let rec findPivot col row =
            if row >= Array.length a then
                failwith "findPivot: no pivot found"
            else if a.[row].[col] <> LanguagePrimitives.GenericZero then
                col
            else
                findPivot col (row + 1)

        let pivotRow = findPivot row row
        swap a row pivotRow
        swap b row pivotRow

        let pivot = Array.item2d row row a

        for col = row to Array.length a - 1 do
            a.[row].[col] <- a.[row].[col] / pivot

        b.[row] <- b.[row] / pivot

        for row2 = 0 to Array.length a - 1 do
            if row <> row2 then
                let factor = a.[row2].[row]

                for col = row to Array.length a - 1 do
                    a.[row2].[col] <- a.[row2].[col] - factor * a.[row].[col]

                b.[row2] <- b.[row2] - factor * b.[row]
