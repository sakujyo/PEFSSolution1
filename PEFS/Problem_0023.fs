module PEFS.Problem_0023

(*
Problem 23 †
完全数とは, その数の真の約数の和がそれ自身と一致する数のことである. 
たとえば, 28の真の約数の和は, 1 + 2 + 4 + 7 + 14 = 28であるので, 28は完全数である.
真の約数の和がその数よりも少ないものを不足数といい, 真の約数の和がその数よりも大きいものを過剰数と呼ぶ.
12は, 1+2+3+4+6=16となるので, 最小の過剰数である. よって2つの過剰数の和で書ける最少の数は24である. 
数学的な解析により, 28123より大きい任意の整数は2つの過剰数の和で書けることが知られている. 
2つの過剰数の和で表せない最大の数がこの上限よりも小さいことは分かっているのだが, この上限を減らすことが出来ていない.
2つの過剰数の和で書き表せない正の整数の総和を求めよ.
*)

let inline isAbundant n =
    // n を d で割っていき、約数の和を求める
    let rec sf n sum d =
        match n with
            | _ when d * d > n -> sum
            | _ when d * d = n -> sum + d
            | _ when n % d = 0 -> sf n (sum + d + n/d) (d + 1)
            | _ -> sf n sum (d + 1)
    (sf n 0 1) > n + n

let solve (yoy: int) =
    let an = [| 0..yoy |] |> Array.filter isAbundant
    let af = [| 0..yoy |] |> Array.map isAbundant

    seq { 1..yoy }
    |> Seq.filter (
        fun n -> an |> Seq.filter (fun a -> a < n)                  // n より小さい an のすべての要素について、
                    |> Seq.forall (fun a -> af.[n - a] = false)     // n - a が過剰数となるような a が存在しない
    )
    |> Seq.sum

let run () =
    solve 28123






















//old
////let rec ia (n : int) sum d =
////    match n with
////        | 1 -> 0
////        | _ when d * d = n -> sum + d
////        | _ when d * d > n -> sum
////        | _ when n % d = 0 -> 
////            match d with
////                | 1 -> ia n (sum + d) (d + 1)           // 自分自身は真の約数に含めないので
////                | _ -> ia n (sum + d + (n/d)) (d + 1)
////        | _ -> ia n sum (d + 1)
//
//// n を d で割っていき、真の約数の和を求める
//let rec ia (n : int) sum d =
//    match n with
//        | 1 -> false
//        | _ when d * d = n -> if (sum + d) > n then true else false
//        | _ when d * d > n -> if (sum) > n then true else false
//        | _ when n % d = 0 -> 
//            match d with
//                | 1 -> ia n (sum + d) (d + 1)           // 自分自身は真の約数に含めないので
//                | _ -> if (sum + d + (n/d)) > n then true else
//                            ia n (sum + d + (n/d)) (d + 1)
//        | _ -> ia n sum (d + 1)
//
//let isAbundant (n : int) =
//    match n > 1 with
//        | true  -> ia n 0 1 
//        | false -> false
//
////let isAbundant (n : int) =
////    match n > 1 with
////        | true  -> if (ia n 0 1) > n then true else false 
////        | false -> false
//
//// seq { 1..28123 } |> Seq.map (isAbundant) |> Seq.filter (fun n -> n = true) |> Seq.fold (fun s b -> s + 1) 1 |> printfn "%A";;
//let solve (problem : int) =
////    12  //1+2+3+4+6=16となるので, 最小の過剰数
//
////    let a1 = Array.create(28124) false
////    for i in 1..28123 do if isAbundant(i) then a1.[i] <- true else () done
//
//    let limit = 28123
//
////    let an = seq { 1..problem } |> Seq.filter isAbundant    // 時間がかかる
////    let an = [| 1..limit |] |> Array.filter isAbundant
//
//    let an = (seq { 1..limit }
//                |> Seq.filter isAbundant
////                |> Seq.filter (fun n -> isAbundant(n) = true)
//                )|> Seq.toArray // |> Seq.iter (printf "%A ")
////    let anflag  = [| for n in 1..limit -> isAbundant(n) |]
//    let anflag = Array.create(limit) false
//    for n in an do anflag.[n] <- true
//
//    printfn "step 2"
////    let s1 = new Set<int>(seq[])
//
////    let s = seq { for i in an do
//////                    printf "%d " i 
////                    for j in (an |> Seq.filter (fun n -> n <= 28123 - i)) do
//////                        if j = 28122 then printf "%d+%d " i j else ()
////                        yield i + j     // 二つの過剰数の和で書き表せる正の整数
////                } 
////                |> Seq.sort |> Seq.distinct 
//////                |> Seq.toArray
////    let s1 = new Set<int>(s)     // 二つの過剰数の和で書き表せる正の整数
//
//    seq { 1..limit }
//    |> Seq.filter (fun n -> an |> Seq.filter (fun a -> a < n) |>Seq.exists (fun a -> anflag.[n - a]) = false)
//    |> Seq.sum
//                        
////    let s1 = seq { for n in 1..limit do
////                    for a in an do
////                        let complement = n - a
////                        an |> if (Seq.forall (fun x -> x <> n - a)) then yield n else ()
//////                        if (Seq.exists (fun x -> x = n - a) an) then yield n else ()
////                        }
//////                        for b in (an |> Seq.filter (limit - i)) do
//
//
//
//
//
////    let s1 = new Set<int>(seq { for i in an do
//////                    printf "%d " i 
////                                    for j in (an |> Seq.filter (fun n -> n <= limit - i)) do
////                //                        if j = 28122 then printf "%d+%d " i j else ()
////                                        yield i + j     // 二つの過剰数の和で書き表せる正の整数
////                }|> Seq.sort |> Seq.distinct) 
////
//////    s |> Seq.min |> printfn "min(%d)" 
//////    s.Length |> printfn "count(%d)" 
////    printfn "step 3"
////    seq { 1..limit }
////    |> Seq.filter (fun n -> Set.contains n s1 = false)
//////    |> Seq.filter (fun n -> Seq.tryFind(fun m -> n = m) s1 = None)
////    |> Seq.sum
//
//
//
//
//
//let run () =
//    solve 0
