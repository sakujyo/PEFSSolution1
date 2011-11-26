module PEFS.Problem_0014

//メモ化
let mutable memoLength = 1024
let lenArray = ref (Array.create memoLength 0)  // memo

// problem 以下の数から開始して漸化式の繰り返しの結果1になるまでの操作の回数(+1)
let solve (problem : int) =

    let extend n = //memo extension
        if n > memoLength - 1 then
            let newArray = Array.create (n + 1) 0
            Array.blit !lenArray 0 newArray 0 memoLength
            lenArray := newArray
            memoLength <- n + 1

    extend problem
    //reducer (初期値 : int) 0
    let rec _resolver (*start : int*) (*length*) (*l : int list*) s =
        match s with
//            | n when n > memoLength ->
//                extend n
//                _resolver n
            | 1M -> (1M, 1M)(*start, length*)(*, l@[s]*)
            | n when n > (problem |> decimal) ->
                let subAnswer =
                    match s % 2M = 0M with     // 偶数か奇数か？
                    | true  -> _resolver (*start*) (*length + 1*) (*l@[s]*) (s / 2M)
                    | false -> _resolver (*start*) (*length + 1*) (*l@[s]*) (3M * s + 1M)
                (s, subAnswer |> snd |> (+) 1M)
            | _ ->
                let answer =
                    if (!lenArray).[int(s)] = 0 then
                        let subAnswer =
                            match s % 2M = 0M with      // 偶数か奇数か？
                            | true  -> _resolver (*start*) (*length + 1*) (*l@[s]*) (s / 2M)
                            | false -> _resolver (*start*) (*length + 1*) (*l@[s]*) (3M * s + 1M)
                        (s, subAnswer |> snd |> (+) 1M)
                    else
                        (s, (!lenArray).[int(s)] |> decimal)       //(s, (!lenArray).[s])
                (!lenArray).[int(s)] <- answer |> snd |> int   // |> snd
                answer
//                match s % 2 = 0 with     // 偶数か奇数か？
//                    | true  -> _generator start (length + 1) (*l@[s]*) (s / 2)
//                    | false -> _generator start (length + 1) (*l@[s]*) (3 * s + 1)
    //recurse adapt and memorization
    let resolver start =
        _resolver (*start*) (*1*) (*[]*) start// |> snd  //(fst >> snd)
//        let answer =
//            if (!lenArray).[start] = 0 then
//                _generator start 1 (*[]*) start |> snd  //(fst >> snd)
//            else
//                (!lenArray).[start]
//        (!lenArray).[start] <- answer

    seq { for i in 1..problem -> decimal i }
//    seq { problem..-1..1 }
    |> Seq.map (resolver)
    |> Seq.maxBy (snd)
//    |> Seq.maxBy (fst >> snd)
//    |> fst

// 動的計画法が使える？
let run () =
    solve 999999
//    solve 9999999
//    |> Seq.iter (fun ((a, b), c) ->
//                    printfn "(%A, %A)" a b
//                    printfn "%A" c)

