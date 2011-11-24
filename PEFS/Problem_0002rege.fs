namespace PEFS
module Problem_0002rege =
    let fib n m =
        Seq.unfold (fun state ->
            if (snd state > m) then None
            else Some(fst state + snd state, (snd state, fst state + snd state))) (n, n)

    let solve (problem : int * int) =
        let n, m = problem
        fib (n |> decimal) (m |> decimal)
            |> Seq.filter (fun x -> x % 2M = 0M)
            |> Seq.fold (+) 0M

//    let run () =
//        fib 1M 89M
    let run () =
        fib 1M 89M
            |> Seq.filter (fun x -> x % 2M = 0M)
            |> Seq.fold (+) 0M
