module PEFS.Problem_0019

(*
Problem 19 †
次の情報が与えられている。

1900年1月1日は月曜日である。
9月、4月、6月、11月は30日まであり、2月を除く他の月は31日まである。
2月は28日まであるが、うるう年のときは29日である。
うるう年は西暦が4で割り切れる年に起こる。しかし、西暦が400で割り切れず100で割り切れる年はうるう年でない。
20世紀（1901年1月1日から2000年12月31日）で月の初めが日曜日になるのは何回あるか。
*)

let uruu year =
    match (year % 400 <> 0) && (year % 100 = 0) with
        | true  -> false
        | false -> (year % 4) = 0

let rec dateOfYearOf1stDayOfMonth year month =
    match month with
        | 1 -> 0
        | 3 -> dateOfYearOf1stDayOfMonth year (month - 1) +
                match uruu year with
                    | true  -> 29
                    | false -> 28
        | 5 | 7 | 10 | 12 -> 30 + dateOfYearOf1stDayOfMonth year (month - 1)
        | _ -> 31 + dateOfYearOf1stDayOfMonth year (month - 1)
//        | 2 | 4 | 6 | 8 | 9 | 11 -> 31 + dateOfYearOf1stDayOfMonth year (month - 1)

let d1900 y m d =
    d - 1 +
        dateOfYearOf1stDayOfMonth y m
        + (y - 1900) * 365 + (y - 1897) / 4 - (y - 1801) / 100 + (y - 1601) / 400






//    let month2day = if uruu y then 29 else 28
//        match m with
//            | 1 -> 0
//            | 2 -> 31
//            | 3 -> 31 + month2day
//            | 4 -> 31 + month2day + 31
//            | 5 -> 31 + month2day + 31+30
//            | 6 -> 31 + month2day + 31+30+31
//            | 7 -> 31 + month2day + 31+30+31+30
//            | 8 -> 31 + month2day + 31+30+31+30+31
//            | 9 -> 31 + month2day + 31+30+31+30+31+31
//            | 10 -> 31 + month2day + 31+30+31+30+31+31+30
//            | 11 -> 31 + month2day + 31+30+31+30+31+31+30+31
//            | 12 -> 31 + month2day + 31+30+31+30+31+31+30+31+30

//      (y - 1900) * 365 + (y - 1897) / 4 - (y - 1801) / 100 + (y - 1601) / 400
//open System;;
//let a y は y年1月1日の1900年1月1日からの経過日数
let a y = (y - 1900) * 365 + (y - 1897) / 4 - (y - 1801) / 100 + (y - 1601) / 400
//let b y = DateTime.Parse(string(y)+"/1/1");;
//seq { 1901..2000 } |> Seq.filter (fun y -> a y <> b y) |> Seq.iter (printfn "%d");;

open System
let b y = (DateTime.Parse(string(y)+"/1/1") - DateTime.Parse("1900/1/1")).Days

//let rec _df year =
//    match year with
//        | 1900 -> 0
////        | y when y < 1900 -> 
//        | _ -> _df (year - 1) + 
//                match uruu (year - 1) with
//                    | true  -> 366
//                    | false -> 365
//
//let from1900Date year month day =
//    _df year + dateOfYearOf1stDayOfMonth year month + day - 1

let solve (problem : int) =
    seq { for y in 1901..2000 do
            for m in 1..12 -> (y, m) }
//    |> Seq.map (fun (y, m) -> if (from1900Date y m 1) % 7 = 6 then 1 else 0)
    |> Seq.map (fun (y, m) -> if (d1900 y m 1) % 7 = 6 then 1 else 0)
    |> Seq.fold (+) 0

let run () =
    solve 0
