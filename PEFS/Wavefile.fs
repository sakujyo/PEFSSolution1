module Wavefile

open System

let readFile filename =
//    let f = new IO.BufferedStream(new IO.FileStream(filename, IO.FileMode.Open, System.IO.FileAccess.Read))
    let f = new IO.FileStream(filename, IO.FileMode.Open, System.IO.FileAccess.Read)

    let fileSize = f.Length |> int
    let buf = Array.create(fileSize) 0uy        // 符号なし 8 ビット自然数?? http://msdn.microsoft.com/ja-jp/library/dd233193.aspx

    let mutable remain = fileSize;
    let mutable bufPos = 0;
    while remain > 0 do
        let readSize = f.Read(buf, bufPos, System.Math.Min(1024, remain))
        bufPos <- bufPos + readSize
        remain <- remain - readSize

    buf

let filter1 (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
    // 涼リル・フィルタ
    match (index / 400) % 2 = 0 with
        | true  -> (0, 0)
        | false -> (lv, rv)

//    (lv, rv)

//    let lt = Array.create(16) 0
//    let rt = Array.create(16) 0

let inline filter2 (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
    (lv * 8, rv * 8)

let inline filter3 (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
    // ディストーション的な。。処理
    let t = 8192
    let ln = lv * 16;
    let rn = lv * 16;
    (
    match ln with
        | _ when ln > t -> t
        | _ when ln < -t -> -t
        | _ -> ln
    ,
    match rn with
        | _ when rn > t -> t
        | _ when rn < -t -> -t
        | _ -> rn
    )

let inline filter4 (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
    // ソフト化。。的(ローパスフィルタ)
    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length

let inline filter (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
//    let la = Array.average(ls)
//    let ra = Array.average(rs)
//    (Array.average(ls), Array.average(rs))
    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length
//    (ls |> Array.sum) / 2, (rs |> Array.sum) / 2
//    let la = Array.average(ls)
//    let ra = Array.average(rs)

let convert (buf : byte array) (index : int) f =
//    for i in 0..buf.Length - 1 do
//        buf.[i] <- match buf.[i] with
//                    | 'b'B -> 'x'B
//                    | x -> x 
    // 16ビットリニアPCM44.1kHz、２ちゃんねる(ステレオ)
    let len = (buf.Length - 44) / 2 / 2     // 2ch, 2byte / sample
//    for i in dindex..4..buf.Length - 1 do
//        let l = int(buf.[i]) + 256 * int(buf.[i + 1])
//        let r = int(buf.[i + 2]) + 256 * int(buf.[i + 3])
//        let a = l
//        let b = r * 0
//        buf.[i] <- a % 256 |> byte
//        buf.[i + 1] <- a / 256 |> byte
//        buf.[i + 2] <- b % 256 |> byte
//        buf.[i + 3] <- b / 256 |> byte
    let la = Array.create (len) 0
    let ra = Array.create (len) 0
    for j in 0..len - 1 do
        let i = 44 + 4 * j
        //注意: signed byte
        la.[j] <- int(buf.[i]) + 256 * int(buf.[i + 1])
        if buf.[i + 1] < 128uy then () else la.[j] <- la.[j] - 65536
        ra.[j] <- int(buf.[i + 2]) + 256 * int(buf.[i + 3])    
        if buf.[i + 3] < 128uy then () else ra.[j] <- ra.[j] - 65536

        //ディストーション的処理
//        la.[j] <- int(buf.[i]) + 256 * int(buf.[i + 1])
//        ra.[j] <- int(buf.[i + 2]) + 256 * int(buf.[i + 3])    

    let ln = Array.create (len) 0
    let rn = Array.create (len) 0
//    for i in 0..len - 1 do
//        let a, b = filter la.[i] ra.[i] i
//        ln.[i] <- a
//        rn.[i] <- b
//

    let wsize = 32
//    let ld = Array.create (16) 0
//    let rd = Array.create (16) 0
    let ld = Array.create (wsize - 1) 0
    let rd = Array.create (wsize - 1) 0

    let ls = Array.concat [ld; la] |> Array.toSeq |> Seq.windowed (wsize)
    let rs = Array.concat [rd; ra] |> Array.toSeq |> Seq.windowed (wsize)
//    let ls = Array.concat [ld; la] |> Array.toSeq |> Seq.windowed (2)
//    let rs = Array.concat [rd; ra] |> Array.toSeq |> Seq.windowed (2)

    printfn "変換開始..."
    let le = ls.GetEnumerator()
    let re = ls.GetEnumerator()
    for i in 0..len - 1 do
        //変換はここ
//        let lv, rv = f la.[i] ra.[i] i (Seq.nth i ls) (Seq.nth i rs)
        // IEnumerable に対する nth は Enumerable の利点を無駄にするかも(アクセス時にEnumerateしてしまう？)
//        let lv, rv = f la.[i] ra.[i] i [|1|] [|1|]
        le.MoveNext() |> ignore
        re.MoveNext() |> ignore
//        let lv, rv = f la.[i] ra.[i] i (le.Current) (re.Current)
        f |> Seq.iter (fun filter ->
            let lv, rv = filter la.[i] ra.[i] i (le.Current) (re.Current)

            ln.[i] <- lv
            rn.[i] <- rv
        )

//        printf (if (i % 1024 = 0) then "1K Sample処理終了..." else "")
        printf (if (i % 1024 = 0) then "." else "")

//    f la ra
//    (ln, rn)
    let buf2 = Array.copy(buf)
    
    for j in 0..len - 1 do
        let i = 44 + 4 * j
        buf2.[i] <- ln.[j] % 256 |> byte
        buf2.[i + 1] <- ln.[j] / 256 |> byte
        buf2.[i + 2] <- rn.[j] % 256 |> byte
        buf2.[i + 3] <- rn.[j] / 256 |> byte
//    Array.Parallel.
    buf2

let writeFile filename (buf : byte array) =
    // 注意：ファイルが既に存在する場合は上書きする(IO.FileMode.Create)
    let f = new IO.FileStream(filename, IO.FileMode.Create, System.IO.FileAccess.Write)
    f.Write(buf, 0, buf.Length)
//    f.Flush()
    f.Close()
            
let run () =
    let buf = readFile @"D:\t\Den_en.wav"
    writeFile @"D:\t\Den_en_out.wav" (convert buf 44 ([filter4; filter3]))
//    let buf = readFile @"D:\t\Complicated.wav"
//    writeFile @"D:\t\Complicated_out.wav" (convert buf 44 filter2)
