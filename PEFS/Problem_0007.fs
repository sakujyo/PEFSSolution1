module PEFS.Problem_0007

open PEFS.Library
open System
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
    solve 6