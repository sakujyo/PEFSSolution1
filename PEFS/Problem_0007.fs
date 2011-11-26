module PEFS.Problem_0007

open PEFS.Library
open System

(*
Problem 7 †
素数を小さい方から6つ並べると 2, 3, 5, 7, 11, 13 であり、6番目の素数は 13 である。

10001 番目の素数を求めよ。
*)

let solve (problem : int) =
//    let generator = fun x ->
//        if (Math.Log(x |> int) * problem < state) then None
//        else Some (state + state / 2) 2

    // generator 2 は、
    let rec generator problem x =
        match Math.Log(x |> float) * float(problem) |> int < x with
            | true -> x
            | false -> generator problem (x + x / 2)

    // 部分適用の練習
    let generatorP = generator problem

    let x = generatorP 2

    primes x
    |> Seq.nth(problem - 1)     // シーケンスのnthインデックスは0開始なので

//    let x =
//        Seq.unfold (fun state ->
//            if (Math.Log(state |> int) * problem < state) then None
//            else Some (state + state / 2) 2

let run () =
    solve 10001