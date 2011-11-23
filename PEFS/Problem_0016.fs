namespace PEFS
module Problem_0016 =
    open System

    let solve (n : int) =

        let column = Array.create((n |> float) * Math.Log10(2.0) |> Math.Ceiling |> int) 0
        column.[0] <- 1;
//        let mutable kuriagari = 0
//        let carry = ref 0
        let twiceWithCarry =
            let carry = ref 0
            (fun n ->
                let n2 = n * 2 + !carry
                carry := n2 / 10
                n2 % 10)
        for j in [1..n] do (
                            column
                            |> Array.iteri(fun i x ->
                                column.[i] <- twiceWithCarry column.[i]
                            )
        )
        
        column |> Array.sum

    let run () =
//        solve 8
        solve 1000
