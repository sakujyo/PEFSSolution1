module PEFS.Problem_0009
// 我想你

// a < b < c
let solve (problem : decimal) =
//    seq { 1M..problem }     // c のつもり
//    |> 
    seq {
        for c in 3M..problem - 2M do
            for b in 2M..c - 1M ->
                (problem - b - c, b, c)
    }
    |> Seq.filter (fun (a, b, c) ->
                    a < b                   // && b < c // は自明？
                    && a > 0M
                    && a * a + b * b = c * c)
    |> Seq.map (fun (a, b, c) -> a * b * c)
    |> Seq.toList
                
//    |> Seq.filter (fun a, b, c -> a + b + c = problem
//    1M

let run () =
    solve 1000M