module PEFS.Problem_0006

let solve (problem : int) =
//    let squareSeq = Seq.initInfinite (fun i -> (i+1)*(i+1))
//    //squareSeq |> Seq.take(problem) |> Seq.sum
//    let sumOfs = squareSeq |> Seq.take(problem) |> Seq.sum
//    let sum = Seq.initInfinite (fun i -> i + 1)
//            |> Seq.take(problem) |> Seq.sum
//    let squareOfs = sum * sum
//    (squareOfs - sumOfs, sumOfs, squareOfs)

    let sumOfSquare = seq { 1..problem }
                    |> Seq.map (fun n -> n * n)
                    |> Seq.sum
    let squareOfSum = seq { 1..problem }
                    |> Seq.sum
                    |> fun n -> n * n
    (squareOfSum - sumOfSquare, sumOfSquare, squareOfSum)

    //regerege code
//    let list = [1..problem]
//    List.zip list list
//    |> List.map (fun (x,y) -> (x,y*y))
//    |> List.unzip
//    |> (fun (a,b) ->
//        let sum = (a |> List.sum)
//        (sum * sum) - (b |> List.sum))

let run () =
    solve 1000
