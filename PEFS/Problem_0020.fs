module PEFS.Problem_0020

//let rec sumOfColumns (sum : int) (n : int) =
////    let radix = 10
//    match n with
//        | 0 -> sum
//        | _ -> sumOfColumns (sum + n % 10) (n / 10)

let rec factorialBigint n =
    match (n = 0I) || (n = 1I) with
        | true -> 1I
//        | 1I -> 1I
        | _ -> n * factorialBigint (n - 1I)

//let rec sumOfColumns (zero:'a) (ten:'a) sum n =
////    let radix = 10
//    match n = zero with
//        | true -> sum
//        | false -> sumOfColumns zero ten (sum + n % ten) (n / ten)
//

let rec sumOfColumns (zero : bigint) (ten : bigint) sum n =
//    let radix = 10
    match n = zero with
        | true -> sum
        | false -> sumOfColumns zero ten (sum + n % ten) (n / ten)

let solve (problem : bigint) =
    sumOfColumns 0I 10I 0I (factorialBigint problem)

open PEFS.Library
let run () =
//    seq { 1I..100I }
//    |> Seq.map (fun n -> (
//        n |> int, 
//        sumOfColumns 0I 10I 0I (factorialBigint n) |> int,
//        seq { 2M..decimal(n) } |> Seq.map (pf) |> Seq.concat, 
////        pf (n |> decimal), 
//        factorialBigint n))
//    |> Seq.iter (fun (n, x, f, m) -> printfn "%4d %4d %A" x n (f |> Seq.fold (fun s d -> s + (sprintf "%A, " d)) ""))

    solve 100I

//    let n = 30
//    let a = seq { for m in 0..n -> seq { 0..n } }

//    a |> Seq.iter (fun s -> Seq.iter(printf "%A ") s
//                            printfn "");;

//    a |> Seq.iteri (fun i s -> 
//                                s |> Seq.map ((*) i) |> Seq.map (sumOfColumns 0) |> Seq.iter(printf "%4A ")
//                                printfn "");;

//    |> Seq.
//    solve 100
    //seq { 1I..100I } |> Seq.fold (*) 1I;;
    //seq [ 1..20 ] |> Seq.map(fun n -> sumOfColumns 0 (n*2)) |> Seq.iter (printfn "%A");;
