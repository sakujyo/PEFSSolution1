module PEFS.Problem_0021

(*
Problem 21 †
d(n)をnの真の約数の和と定義する。（真の約数とはn以外の約数のことである。）
もし、d(a) = b かつ d(b) = a （a ≠ b）を満たすとき、aとbは友愛数（親和数）であるという。

例えば、220の約数は1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110なのでd(220) = 284である。
また、284の約数は1, 2, 4, 71, 142なのでd(284) = 220である。

それでは10000未満の友愛数の合計を求めよ。
*)

// nの約数(nや1を含めない)の和を再帰的に求める
let rec _d (n : int) (x : int) (sum : int) =
    match x with
        | x when x * x > n -> sum
        | x when x * x = n -> sum + x
        | _ ->
            match n % x = 0 with
                | true  -> _d n (x + 1) sum + x + (n / x)
                | false -> _d n (x + 1) sum
// nの真の約数(nは含めないが1は含める)の和を求める
let d (n : int) =
    match n with
        | 0 -> 0
        | 1 -> 0
        | _ -> 1 + _d n 2 0
    
(*
let max = 100000;;
let a = seq { 1..max } |> Seq.map (fun n -> n |> pf |> sd |> (fun x -> x-n));;
let b = seq { 1..max } |> Seq.map (d);;
Seq.zip a b |> Seq.filter (fun (ta, tb) -> ta <> tb);;
*)

let yuuaisuu (n : int) =
    match d (d n) = n with
        | true -> n <> d n
        | false -> false
//    let a = d n
//    if d a = n then true else false
// n = problem 以下の友愛数の総和を求める
let solve (problem : int) =
    seq { 1..problem }
    |> Seq.filter (yuuaisuu)
    |> Seq.sum

let run () =
    solve 9999
