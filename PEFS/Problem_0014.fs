module PEFS.Problem_0014

//メモ化
let mutable maxMemo = 1024 - 1
let lenArray = ref (Array.create (maxMemo + 1 |> int) 0)  // memo

// problem 以下の数から開始して漸化式の繰り返しの結果1になるまでの操作の回数(+1)
let solve (problem : int) =

    let extend n = //memo extension
        if n > maxMemo then
            let newArray = Array.create ((n |> int) + 1) 0
            Array.blit !lenArray 0 newArray 0 (maxMemo + 1)
            lenArray := newArray
            maxMemo <- n

//    extend problem
    match problem with
        | size when size <= 1024 * 1024 -> extend (size - 1)
        | _ -> extend (1024 * 1024 - 1)
//    extend (1024 * 1024 * 16 - 1)
    (!lenArray).[1] <- 1

    //_resolver 再帰的に解く1までのノード距離
    let rec _resolver s =
        let inline answer s =
            match s % 2M = 0M with     // 偶数か奇数か？
            | true  -> _resolver (s / 2M)
            | false -> _resolver (3M * s + 1M)
            |> (+) 1
        match s with
            | n when n > decimal(maxMemo) ->
                answer s
            | _ ->
                if (!lenArray).[int(s)] = 0 then
                    let ans = answer s
                    (!lenArray).[int(s)] <- ans // |> int
                    ans
                else
                    (!lenArray).[int(s)]    //(s, (!lenArray).[s])
    let resolver n =
        (n, _resolver (n |> decimal))
    seq { for i in 1..problem -> i }
    |> Seq.map (resolver)
    |> Seq.maxBy (snd)

let run () =
//    solve 9999999
    solve 999999
//    |> Seq.iter (fun ((a, b), c) ->
//                    printfn "(%A, %A)" a b
//                    printfn "%A" c)








////
//// problem 以下の数から開始して漸化式の繰り返しの結果1になるまでの操作の回数(+1)
//let solve (problem : int) =
//
//    let extend n = //memo extension
//        if n > memoLength - 1 then
//            let newArray = Array.create (n + 1) 0
//            Array.blit !lenArray 0 newArray 0 memoLength
//            lenArray := newArray
//            memoLength <- n + 1
//
////    extend problem
//    extend (1024 * 1024 * 4 - 1)
//    (!lenArray).[1] <- 1
//    //_resolver 再帰的に解く1までのノード距離
//    let rec _resolver (*start : int*) (*length*) (*l : int list*) s =
//        match s with
////            | n when n > memoLength ->
////                extend n
////                _resolver n
////            | 1M -> (1M, 1M)(*start, length*)(*, l@[s]*)
//            | n when n > (memoLength - 1|> decimal) ->
//                let subAnswer =
//                    match s % 2M = 0M with     // 偶数か奇数か？
//                    | true  -> _resolver (*start*) (*length + 1*) (*l@[s]*) (s / 2M)
//                    | false -> _resolver (*start*) (*length + 1*) (*l@[s]*) (3M * s + 1M)
//                (s, subAnswer |> snd |> (+) 1M)
//            | _ ->
////                let answer =
//                    if (!lenArray).[int(s)] = 0 then
//                        let answer =
//                            match s % 2M = 0M with      // 偶数か奇数か？
//                            | true  -> _resolver (*start*) (*length + 1*) (*l@[s]*) (s / 2M)
//                            | false -> _resolver (*start*) (*length + 1*) (*l@[s]*) (3M * s + 1M)
//                            |> snd |> (+) 1M
//                        (!lenArray).[int(s)] <- answer |> int
////                        (s, subAnswer |> snd |> (+) 1M)
////                        (s, (!lenArray).[int(s)] |> decimal)
//                        (s, answer)
//                    else
//                        (s, (!lenArray).[int(s)] |> decimal)       //(s, (!lenArray).[s])
////                (!lenArray).[int(s)] <- answer |> snd |> int   // |> snd
////                answer
////                match s % 2 = 0 with     // 偶数か奇数か？
////                    | true  -> _generator start (length + 1) (*l@[s]*) (s / 2)
////                    | false -> _generator start (length + 1) (*l@[s]*) (3 * s + 1)
//    //recurse adapt and memorization
////    let resolver start =
////        _resolver (*start*) (*1*) (*[]*) start// |> snd  //(fst >> snd)
////        let answer =
////            if (!lenArray).[start] = 0 then
////                _generator start 1 (*[]*) start |> snd  //(fst >> snd)
////            else
////                (!lenArray).[start]
////        (!lenArray).[start] <- answer
//
//    seq { for i in 1..problem -> decimal i }
////    seq { problem..-1..1 }
//    |> Seq.map (_resolver)
//    |> Seq.maxBy (snd)
////    |> Seq.maxBy (fst >> snd)
////    |> fst
