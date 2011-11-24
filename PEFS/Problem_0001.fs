namespace PEFS
module Problem_0001 =

    let solve (problem : int) =
        let seq = [1..problem - 1]
        let seq3m = seq |> Seq.filter(fun x -> x % 3 = 0)
        let seq5m = seq |> Seq.filter(fun x -> x % 5 = 0)
        Seq.append seq3m seq5m |> Seq.distinct |> Seq.sum

    let run () =
        solve 10    // 10未満の自然数
