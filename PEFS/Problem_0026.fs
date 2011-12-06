module PEFS.Problem_0026

(*
Problem 26 †
単位分数とは分子が1の分数である。分母が2から10の単位分数を10進数で表記すると次のようになる。

1/2 = 0.5
1/3 = 0.(3)
1/4 = 0.25
1/5 = 0.2
1/6 = 0.1(6)
1/7 = 0.(142857)
1/8 = 0.125
1/9 = 0.(1)
1/10 = 0.1
0.1(6)は 0.166666... という数字であり、1桁の循環節を持つ。1/7 の循環節は6桁ある。

d < 1000 なる 1/d の中で循環節が最も長くなるような d を求めよ。
*)

//分数の除算部分の剰余のリストを作り、同じものが見つかった時のリストの長さを返す
//(循環節の長さ)
let rec findSameModulo n d (l : int list) =
    match n % d = 0 with
        | true -> (d, 0, [])
        | _ ->
            match List.exists(fun x -> x = n % d) l with
                | true -> (d, 1 + (List.findIndex(fun x -> x = n % d) l), l)    //TODO: Length + 1 - foundIndex
                | _ -> findSameModulo ((n % d) * 10) d ((n % d)::l)

let solve (problem : int) =
    [1..problem]
    |> Seq.map (fun d -> findSameModulo 1 d [])
    |> Seq.maxBy (fun (a, b, c) -> b)

let run () =
    solve 999








//            match n % d with
//                | x when List.exists (fun n -> n = x) l -> (d, l.Length + 1 - List.findIndex(x))    //TODO: Length + 1 - foundIndex
//                | x -> findSameModulo (x * 10) d (x::l)


//    |> Seq.maxBy (fun (d, l) -> l)

//    |> fst
