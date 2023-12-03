module AoC202004

open AdventOfCode
open FSharpPlus
open FParsec
open System.Text.RegularExpressions

let parser =
    let pnonwhite = satisfy (fun c -> System.Char.IsWhiteSpace c |> not)

    let pword = many1Satisfy (fun c -> System.Char.IsWhiteSpace c |> not)

    let pfield = (parray 3 pnonwhite |>> String.ofArray) .>> pchar ':' .>>. pword

    let ppassport = sepEndBy1 pfield (pchar ' ' <|> pchar '\n')

    sepEndBy1 ppassport (pchar '\n')

let isValidEasy fields =
    (fields |> filter (fst >> (<>) "cid") |> length) = 7

let fieldValid (field: string, value: string) =
    let validRange (num: string) (min, max) =
        match System.Int32.TryParse num with
        | true, v -> min <= v && v <= max
        | false, _ -> false

    let validHeight (h: string) =
        if h.Length < 3 then
            false
        else
            match h.Substring(h.Length - 2) with
            | "cm" -> validRange (h.Substring(0, h.Length - 2)) (150, 193)
            | "in" -> validRange (h.Substring(0, h.Length - 2)) (59, 76)
            | _ -> false

    match field with
    | "byr" -> validRange value (1920, 2002)
    | "iyr" -> validRange value (2010, 2020)
    | "eyr" -> validRange value (2020, 2030)
    | "hgt" -> validHeight value
    | "hcl" -> Regex.IsMatch(value, "^#[0-9a-f]{6}$")
    | "ecl" -> [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ] |> List.contains value
    | "pid" -> Regex.IsMatch(value, "^[0-9]{9}$")
    | "cid" -> true
    | _ -> false

let isValidHard fields =
    isValidEasy fields && (fields |> List.forall fieldValid)

let solve validator input =
    input |> List.filter validator |> length

let solution = makeSolution () parser (solve isValidEasy) (solve isValidHard)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Example part 1`` () =
        let input =
            [| "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
               "byr:1937 iyr:2017 cid:147 hgt:183cm"
               ""
               "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
               "hcl:#cfa07d byr:1929"
               ""
               "hcl:#ae17e1 iyr:2013"
               "eyr:2024"
               "ecl:brn pid:760753108 byr:1931"
               "hgt:179cm"
               ""
               "hcl:#cfa07d eyr:2025 pid:166559648"
               "iyr:2011 ecl:brn hgt:59in" |]

        testPart1 solution input |> should equal 2

    [<Theory>]
    [<InlineData("byr", "2002", true)>]
    [<InlineData("byr", "2003", false)>]
    [<InlineData("hgt", "60in", true)>]
    [<InlineData("hgt", "190cm", true)>]
    [<InlineData("hgt", "190in", false)>]
    [<InlineData("hgt", "190", false)>]
    [<InlineData("hcl", "#123abc", true)>]
    [<InlineData("hcl", "#123abz", false)>]
    [<InlineData("hcl", "123abc", false)>]
    [<InlineData("ecl", "brn", true)>]
    [<InlineData("ecl", "wat", false)>]
    [<InlineData("pid", "000000001", true)>]
    [<InlineData("pid", "0123456789", false)>]
    let ``Field validation`` field value expected =
        fieldValid (field, value) |> should equal expected

    [<Fact>]
    let ``Example part 2`` () =
        let input =
            [| "eyr:1972 cid:100"
               "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
               ""
               "iyr:2019"
               "hcl:#602927 eyr:1967 hgt:170cm"
               "ecl:grn pid:012533040 byr:1946"
               ""
               "hcl:dab227 iyr:2012"
               "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
               ""
               "hgt:59cm ecl:zzz"
               "eyr:2038 hcl:74454a iyr:2023"
               "pid:3556412378 byr:2007"
               ""
               "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
               "hcl:#623a2f"
               ""
               "eyr:2029 ecl:blu cid:129 byr:1989"
               "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
               ""
               "hcl:#888785"
               "hgt:164cm byr:2001 iyr:2015 cid:88"
               "pid:545766238 ecl:hzl"
               "eyr:2022"
               ""
               "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" |]

        testPart2 solution input |> should equal 4
