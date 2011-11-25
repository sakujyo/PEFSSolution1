module PEFS.Problem_0002rege

//52~53ms 4613732M
let fib n m =
    Seq.unfold (fun (n1, n2) ->
        if (n1 < m) then Some(n1, (n2, n1 + n2))
        else None) (n, n)

let run () =
    fib 1M 89M
        |> Seq.filter (fun x -> x % 2M = 0M)
        |> Seq.fold (+) 0M
//let fib n m =
//    Seq.unfold (fun state ->
//        if (snd state > m) then None
//        else Some(fst state + snd state, (snd state, fst state + snd state))) (n, n)

let solve (problem : int * int) =
    let n, m = problem
    fib (n |> decimal) (m |> decimal)
        |> Seq.filter (fun x -> x % 2M = 0M)
        |> Seq.fold (+) 0M

//    let run () =
//        fib 1M 89M
//    let run () =
//        fib 1M 89M
//            |> Seq.filter (fun x -> x % 2M = 0M)
//            |> Seq.fold (+) 0M
