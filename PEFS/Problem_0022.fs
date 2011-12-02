module PEFS.Problem_0022

(*
Problem 22 †
5000個以上の名前が書かれている46Kのテキストファイルnames.txt を用いる. まずアルファベット順にソートせよ.

のち, 各名前についてアルファベットに値を割り振り, リスト中の出現順の数と掛け合わせることで, 名前のスコアを計算する.

たとえば, リストがアルファベット順にソートされているとすると, COLINはリストの938番目にある. またCOLINは3 + 15 + 12 + 9 + 14 = 53という値を持つ. よってCOLINは938 × 53 = 49714というスコアを持つ.

ファイル中の全名前のスコアの合計を求めよ.
*)

open System

let ascore (s : string) =
    s |> Seq.map (fun c -> int(c) - int('A') + 1) |> Seq.fold (+) 0

let solve (problem : int) =
    
    let filename = @"E:\m\VS2010Project\PEFSSolution1\PEFS\names.txt";

    let f = new IO.FileStream(filename, IO.FileMode.Open, System.IO.FileAccess.Read)
    let fs = new System.IO.StreamReader(f)

    let s = fs.ReadToEnd()
    //System.Console.WriteLine(s)
    let a = s.Split([|',';'"'|], System.StringSplitOptions.RemoveEmptyEntries)
//    a |> Seq.sort |> Seq.iter (printf "%s:")
    a |> Seq.sort |> Seq.map(ascore) |> Seq.mapi (fun i n -> (i + 1) * n) |> Seq.fold (+) 0

let run () =
    solve 0
