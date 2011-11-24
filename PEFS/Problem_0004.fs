namespace PEFS
module Problem_0004 =
    /// n が 10進で回文数なら、 palindromicNumber n n 0 は true を返します。
    let rec palindromicNumber (n : int) (x : int) (y : int) : bool =
        match x with
            | 0 -> (y = n)
            | _ ->
//                let y = y * 10 + (x % 10)
                palindromicNumber n (x / 10) (y * 10 + (x % 10))

    let solve (problem : int) =
        let lower = 
            seq { for i in 1 .. problem - 1 do yield 10 }
            |> Seq.fold(fun s n ->
                s * n) 1
        let upper =
            seq { for i in 1 .. problem do yield 10 }
            |> Seq.fold(fun s n ->
                s * n) 1
            |> (fun n -> n - 1)
        seq {
            for x in lower .. upper do
                for y in x .. upper do
                    yield x * y
        }
        |> Seq.filter (fun n -> palindromicNumber n n 0)
        |> Seq.max

    let run () =
        solve 3