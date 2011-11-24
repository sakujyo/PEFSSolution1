namespace PEFS
module Problem_0002 =
    // フィボナッチ数列 1, 2, 3, 5, 8, ... とする

    let solve (problem : int) =
        let generator ((n2 : int), (n1 : int)) =
            match n2 + n1 with
                | x when x > problem -> None
                | x -> Some(x, (n1, x))
        Seq.unfold generator (0, 1)
        |> Seq.takeWhile(fun x -> x <= problem)
        |> Seq.filter(fun x -> x % 2 = 0)
        |> Seq.sum

    let run () =
        solve 89
