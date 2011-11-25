namespace PEFS
module Problem_0004 =
    /// n が 10進で回文数なら、 palindromicNumber n n 0 は true を返します。
//    let rec palindromicNumber (n : int) (x : int) (y : int) : bool =
    open System
    let rec palindromicNumber (n : decimal) (x : decimal) (y : decimal) : bool =
        match x with
            | 0M -> (y = n)
            | _ ->
//                let y = y * 10 + (x % 10)
                palindromicNumber n (x / 10M |> Math.Floor) (y * 10M + (x % 10M))
//    let rec palindromicNumber (n : int) (x : int) (y : int) : bool =
//        let mutable xm = x
//        let mutable ym = y
//        while xm <> 0 do
//            ym <- ym * 10 + xm % 10
//            xm <- xm / 10
//        (ym = n)

    open System
    let solve (problem : int) =
        let lower = 
            seq { for i in 1 .. problem - 1 do yield 10M }
            |> Seq.fold(fun s n ->
                s * n) 1M
        let upper =
            seq { for i in 1 .. problem do yield 10M }
            |> Seq.fold(fun s n ->
                s * n) 1M
            |> (fun n -> n - 1M)
//        seq {
////            for x in lower .. upper do
////                for y in x .. upper do
////                    yield x * y
//            for x in upper ..(-1).. lower do
//                for y in upper ..(-1).. x do
////                    printfn "%d " (x * y)
//                    yield x * y
//                printf "%d " x
//        }
////        |> Seq.sortBy((-) 0)
////        |> Seq.find(fun n -> palindromicNumber n n 0)
//        |> Seq.filter (fun n -> palindromicNumber n n 0)
//        |> Seq.max
////        |> Seq.length

        seq {
            for i in upper ..(-1M).. lower do
//                printfn "%M" i
                yield (seq {
                        // i*iの双曲線以遠にある積(チーズケーキ)をトラバース
                        for x in upper ..(-1M).. i do
                            let c = Math.Ceiling(float(i*i) / (float x)) |> decimal
                            //前回の始点
                            let preC = (Math.Ceiling(float((i+1M)*(i+1M)) / (float x)) |> decimal) - 1M
                            //小さいほう(対角線上)まで
                            let clip = if i < preC then i else preC
                            for y in c..clip do
                                yield (x*y, x, y)
                } //
                |> Seq.filter(fun (n, x, y) -> palindromicNumber n n 0M)
                )
        }
        |> Seq.find(fun n -> n |> Seq.isEmpty = false)
//        |> Seq.max
        |> Seq.maxBy(fun (n, x, y) -> n)
//        |> printfn "%d "

    open System
    let run () =
//        seq {
//            for i in 9999 ..(-1).. 1 do
//                yield (seq {
//                        // i*iの双曲線以遠にある積(チーズケーキ)をトラバース
//                        for x in 9999 ..(-1).. i do
//                            let c = Math.Ceiling(float(i*i) / (float x)) |> int
//                            //前回の始点
//                            let preC = (Math.Ceiling(float((i+1)*(i+1)) / (float x)) |> int) - 1
//                            //小さいほう(対角線上)まで
//                            let clip = if i < preC then i else preC
//                            for y in c..clip do
//        //                        yield (x, y, x*y)
//                                yield (x*y)
////                        printfn ""
//        //        } |> Seq.iter(printf "(%A) ")
//                } //
//                |> Seq.filter(fun n -> palindromicNumber n n 0)
////                |> Seq.max
////                |> Seq.sortBy ((-) 0)
//                )
//                printf "%d " i
//        }
////        |> Seq.filter(fun n -> palindromicNumber n n 0)
////        |> Seq.head
//        |> Seq.find(fun n -> n |> Seq.isEmpty = false)
//        |> Seq.max
//        |> printfn "%d "
        solve 4
