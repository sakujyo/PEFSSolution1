namespace PEFS
module Problem_0012 =

    /// n 番目の三角数を返す
    let sankakusu n =
        (n * (n + 1) / 2)

    /// yakusu a n count: 
    /// a を n = 1 から順に割っていき、約数の個数を count = 0 から数えて返す
    let rec yakusu a n count =
        match n with
            | 0 -> 0    // 関数が誤った用法で呼び出された
            | _ ->
            match n * n with
                // ちょうど a が n の2乗の場合も終了
                | x when x = a -> count * 2 + 1
                // 終了条件  a < n * n ならば、相方の約数を数え始めるので、既知の約数の2倍を返して終了
                | x when a < x -> (count * 2)
                | _ ->
                match a % n with
                    // a % n = 0 なら a を n で割り切れることを意味する
                    | 0 -> yakusu a (n + 1) (count + 1)
                    | _ -> yakusu a (n + 1) count

    /// n 番目の 三角数 s について、約数の個数が y 以上の最初の数を再帰的に探す
    //  戻り値:
    //      見つかった三角数
    let rec sankakuyakusu s n y =
        match yakusu s 1 0 with
            // count = 0, 除数 1 から約数を数えはじめて、約数の個数が y 以上の最初の数を探す
            | x when x >= y -> s
            | _ -> sankakuyakusu (sankakusu (n + 1)) (n + 1) y

    let n = 1
    let solve (problem : int) =
        sankakuyakusu (sankakusu n) n problem
        // [1..10] |> Seq.map(sankakusu) |> Seq.iter(printf "%d ");;
        // [1..10] |> Seq.map(sankakusu) |> Seq.map(fun n -> yakusu n 1 0) |> Seq.iter(printf "%d ");;

    let run () =

//        sankakuyakusu (sankakusu n) n 501
//        yakusu 2 1 0
        solve 501

        //76576500

