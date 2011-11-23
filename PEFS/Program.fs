// Learn more about F# at http://fsharp.net


module PEFS.Main

//open PEFSTest.PEFSTestModule
//循環依存となるためNG

// モジュール名を変える事で別の問題の解答を取得出来る
let getAnswer () =

    let cp = System.Diagnostics.Process.GetCurrentProcess()
    let t1 = cp.TotalProcessorTime

//    let answer = Problem_0012.run()
//    let answer = Problem_0016.run(1000)
//    let answer = Problem_0016.run(5)
//    let answer = Problem_0067.run()
    let answer = Problem_0000.run(1234)

    let t2 = cp.TotalProcessorTime
    printfn "CPU Time: %A" (t2 - t1)

//    let answer = Problem_0012.run()
//    let answer = NSPEULAR.Problem_0003.test 37366090917157M
//    let answer = NSPEULAR.Problem_0003.run()
    printfn "%A" answer
(* ブロックコメント。ネストできる。
    (*
    　printfだと改行無しになります
    *)
*)
(* ←このコメントをON/OFFする
	printfn "hello world";;
// *)
//not true;;		//論理値の否定。bool値のみ

open System.Diagnostics
let main() =
    let stopWatch = new Stopwatch()
    stopWatch.Start()

//            let input, result = data in
//            Assert.That (test input, Is.EqualTo(result), sprintf "prime factors of %A = %A" input result)
    // 実際の処理
    getAnswer()

    stopWatch.Stop();

//    printf "%d ms" stopWatch.ElapsedMilliseconds
    printfn "%d ms" stopWatch.ElapsedMilliseconds
    ()

main()
