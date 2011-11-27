module PEFS.Problem_0015

(*
Problem 15 †
2 × 2 のマス目の左上からスタートした場合、引き返しなしで右下にいくルートは 6 つある。



では、20 × 20 のマス目ではいくつのルートがあるか。
*)

//open PEFS.Library
//let comb n r =
//
//    let rec pow x y =
//        match y with
//            | 0 -> 1M
//            | _ -> x * pow x (y - 1)
//
//    let a = Seq.concat [
//                seq { n-r+1..n }
//                    |> Seq.collect(decimal >> pf)   //素因数分解
//                    |> Seq.map (fun i -> (i, +1));  //分子
//                seq { 1..r }
//                    |> Seq.collect(decimal >> pf)   //素因数分解
//                    |> Seq.map (fun i -> (i, -1));  //分母
//            ]
//    let yakubun = a |> Seq.groupBy(fun (x, y) -> x)
//                    |> Seq.map (fun (t, u) ->
//                        (t, u
//                            |> Seq.map (fun (c, d) -> d)
//                            |> Seq.sum )
//                        )
//    yakubun |> Seq.fold (fun s (x, y) -> s * pow x y) 1M

//ここにDecimalの限界をしるす
//Decimal.MaxValue;;
//val it : decimal = 79228162514264337593543950335M
let mutable maxMemo = 256   // 257*257*sizeof(bigint) bigintは可変長
let routeMemo = Array2D.create (maxMemo + 1) (maxMemo + 1) 0I

let rec route x y =
    let a = routeMemo.[x, y]
    match a = 0I with
        | false -> a
        | true  ->
            let ans =
                match x > 0 with
                    | true ->
                        match y > 0 with
                            | true  -> route (x - 1) y + route x (y - 1)
                            | false -> route (x - 1) 0  // 1に決まってるけどｗ
                    | false ->
                        match y > 0 with
                            | true  -> route (y - 1) 0   // まあ1
                            | false -> 1I
            routeMemo.[x, y] <- ans
            ans

let solve px py =
    route px py

let run () =
    solve 20 20
    









//    comb (problem * 2) problem


//    System.Runtime.InteropServices.Marshal.SizeOf(int)
//    let o = route 100 100
//    o.ToByteArray().Length
//(route 100 100).ToByteArray().Length;;
//    System.Runtime.InteropServices.Marshal.SizeOf(o.ToByteArray);;


//// int n = 12 程度までしか正しい値は返らない
//// decimal n = 27M 程度までしか正しい値は返らない
//let rec factorial n =
//    match n with
//        | 0M -> 1M
//        | 1M -> 1M
//        | _ -> n * factorial (n - 1M)

//// int n = 12 程度までしか正しい値は返らない
//// decimal n = 39M, a = 21M 程度までしか正しい値は返らない
//let rec partialFactorial n a =
//    match n >= a with
//        | true  -> n * partialFactorial (n - 1M) a
//        | false -> 1M

// int n = 12 程度までしか正しい値は返らない
// decimal n = 39M, a = 21M 程度までしか正しい値は返らない
//let rec combination n r =
//    (partialFactorial n (n - r + 1M) / factorial (r |> decimal))
//
//let rec partFact n a =
//    match a with
//        | 1M -> factorial n
//        | _ -> partFact (n - 1M) (a - 1M) / (a - 1M) * n
//
//let rec _comb n a r =
//    match a with
//        | 1M -> factorial n / factorial r
//        | _ -> _comb (n - 1M) (a - 1M) r / (a - 1M) * n



//let comb n r = _comb n (n - r + 1M) r

//let listdiff a b =
//    let c = a |> Seq.map (fun i -> (i, +1))
//    let d = b |> Seq.map (fun i -> (i, -1))
//    let e = Seq.concat [c; d]
//    let f = e |> Seq.sortBy (fun (a, b) -> a)
//    f

//open PEFS.Library
//let calc n r =
//    let a = seq { r+1..n }
//            |> Seq.collect(decimal >> pf)   //素因数分解
//            |> Seq.sort
//    let b = seq { 1..r }
//            |> Seq.collect(decimal >> pf)   //素因数分解
//            |> Seq.sort
//    let c = listdiff a b
//    c |> Seq.fold (
//        fun s (a, b) ->
//            printfn "%M * %M ^ %d " s a b
//            match b with
//                | 1  -> s * a
//                | -1 -> s / a
//            ) 1M

//open PEFS.Library
//let comb n r =
//    let a = seq { r+1..n }
//            |> Seq.collect(decimal >> Library.pf)   //素因数分解
//            |> Seq.sort
//            |> Seq.groupBy(fun n -> n)
//    let b = seq { 1..r }
//            |> Seq.collect(decimal >> Library.pf)   //素因数分解
    //このあと、方法1：(素数、次数)のタプルにする

//    let a = seq { r+1..n }
//            |> Seq.collect(decimal >> Library.pf)   //素因数分解
//            |> Seq.sort
//    let b = seq { 1..r }
//            |> Seq.collect(decimal >> Library.pf)   //素因数分解
//            |> Seq.sort
    
//    let debug b s a =
//        let o = match b 2M 2M = 4M with
//                | true -> "*"
//                | false -> "/"
//        printf "(%M %s %M = %A) " s o a (b s a) 
//        b s a

//    let groupSum (s : seq<'a>) keyProjection (valueProjection) =
//        seq { for p in (s |> Seq.map (keyProjection) |> Seq.distinct) do
////                yield (1, 1)
//
//                    yield (p, s |> Seq.filter (fun (x, y) -> x = p)
//                                |> Seq.map (fun (x, y) -> y)
//                                |> Seq.sum)
//        } 






    // (2, 1) (5, 1) ... のような素因数とその次数のタプルのシーケンスに写像したい
//    |> Seq.fold (
//        fun s (a, b) -> debug b s a) 1M


//            match b with
//                | 1  -> s * a
//                | -1 -> s / a
//            ) 1M












//open PEFS.Library
//let comb n r =
//
//    let a = Seq.concat [
//                seq { n-r+1..n }
//                    |> Seq.collect(decimal >> pf)   //素因数分解
//                    |> Seq.sort
////                    |> Seq.map (fun i -> (i, (*)));
//                    |> Seq.map (fun i -> (i, +1));
//                seq { 1..r }
//                    |> Seq.collect(decimal >> pf)   //素因数分解
//                    |> Seq.sort
////                    |> Seq.map (fun i -> (i, (/)));]
//                    |> Seq.map (fun i -> (i, -1));]
//            |> Seq.sortBy (fun (a, b) -> a)
////    seq { for p in (a |> Seq.map (fun (x, y) -> x) |> Seq.distinct) do
//////            yield (1, 1)
////
////            yield (p, a |> Seq.filter (fun (x, y) -> x = p)
////                        |> Seq.map (fun (x, y) -> y)
////                        |> Seq.sum)
//////            |> fun n -> yield (p, n)//Seq.iter (fun n -> yield (p, n))
////    } 
//    let rec pow x y =
//        match y with
//            | 0 -> 1M
//            | _ -> x * pow x (y - 1)
////      気に入ったパターン
////    let x = a |> Seq.groupBy(fun (x, y) -> x)
////                    |> Seq.map ()
//    let yakubun = a |> Seq.groupBy(fun (x, y) -> x)
//                    |> Seq.map (fun (t, u) -> (t, u |> Seq.map (fun (c, d) -> d) |> Seq.sum ) )
//    yakubun |> Seq.fold (fun s (x, y) -> s * pow x y) 1M
