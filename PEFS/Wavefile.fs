module Wavefile

open System

let wsize = 44100  // 時間処理を行うときの現時点から過去の標本数

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

let inline filter5 (j : int) (ls : int array) (rs : int array) =
    // ステレオコーラス
    // 左:原音+30ms遅れ程度 右:原音+60ms遅れ程度
    let c = 2646
    let delay = match c > wsize with
                | true  -> wsize
                | false -> c
    let i = j + (wsize - 1) - delay

    (ls.[i + 0] + ls.[i + 1] + ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 4, (rs.[i + 0] + rs.[i + 1] + rs.[i + delay - 1] + rs.[i + delay - 2]) / 4

let inline filter6 (j : int) (ls : int array) (rs : int array) =
    // ドナルド化
    //ls, rs (末尾がそれぞれ lv, rv)から新しい標本を合成(変換)する関数

//    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length
//    (ls.[0] + ls.[wsize - 1]) / 2, (rs.[0] + rs.[wsize - 1]) / 2
    // 左:原音+30ms遅れ程度 右:原音+60ms遅れ程度
    let c = 2646
    let delay0 = match c > wsize with
                    | true  -> wsize
                    | false -> c
    let cycle = System.Math.Sin(System.Math.PI * float(j) / float(delay0)) / 2.0 + 0.5                   
    let delay = (float(delay0) * cycle) |> int
    let i = j + (wsize - 1) - delay

    (ls.[i + 0] + ls.[i + 1] + ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 4, (rs.[i + 0] + rs.[i + 1] + rs.[i + delay - 1] + rs.[i + delay - 2]) / 4
//    (ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 2, (rs.[i + delay - 1] + rs.[i + delay - 2]) / 2

let inline filter7 (j : int) (ls : int array) (rs : int array) =
    // チアリーダー化
    //ls, rs (末尾がそれぞれ lv, rv)から新しい標本を合成(変換)する関数

//    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length
//    (ls.[0] + ls.[wsize - 1]) / 2, (rs.[0] + rs.[wsize - 1]) / 2
    // 左:原音+30ms遅れ程度 右:原音+60ms遅れ程度
    let c = 2646
    let delay0 = match c > wsize with
                    | true  -> wsize
                    | false -> c
//    let cycle = System.Math.Sin(System.Math.PI * float(j) / float(delay0)) / 2.0 + 0.5                   
    let i = j + (wsize - 1) - delay0
    let cycle0 = (float(ls.[i + delay0 - 1]) - float(ls.[i + delay0 - 2])) / 3000.0
    let cycle = if cycle0 > 1.0 then 1.0 else cycle0
    let delay = (float(delay0) * cycle) |> int

    (ls.[i + 0] + ls.[i + 1] + ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 4, (rs.[i + 0] + rs.[i + 1] + rs.[i + delay - 1] + rs.[i + delay - 2]) / 4
//    (ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 2, (rs.[i + delay - 1] + rs.[i + delay - 2]) / 2

let inline filter8 (j : int) (ls : int array) (rs : int array) =
    // 微分キラキラ化
    //ls, rs (末尾がそれぞれ lv, rv)から新しい標本を合成(変換)する関数

//    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length
//    (ls.[0] + ls.[wsize - 1]) / 2, (rs.[0] + rs.[wsize - 1]) / 2
    // 左:原音+30ms遅れ程度 右:原音+60ms遅れ程度
    let c = 2646
    let delay0 = match c > wsize with
                    | true  -> wsize
                    | false -> c
//    let cycle = System.Math.Sin(System.Math.PI * float(j) / float(delay0)) / 2.0 + 0.5                   
    let i = j + (wsize - 1) - delay0
    let lcycle0 = (float(ls.[i + delay0 - 1]) - float(ls.[i + delay0 - 2])) / 3000.0
    let rcycle0 = (float(rs.[i + delay0 - 1]) - float(rs.[i + delay0 - 2])) / 3000.0
    let lcycle1 = if lcycle0 > 1.0 then 1.0 else lcycle0
    let rcycle1 = if rcycle0 > 1.0 then 1.0 else rcycle0
    let lcycle  = if lcycle1 < -1.0 then -1.0 else lcycle1
    let rcycle  = if rcycle1 < -1.0 then -1.0 else rcycle1
    let ldif = int(32767.0 * lcycle)
    let rdif = int(32767.0 * rcycle)
//    let delay = (float(delay0) * cycle) |> int
    ldif, rdif

//let inline filter (lv : int) (rv : int) (index : int) (ls : int array) (rs : int array) =
let inline filter (j : int) (ls : int array) (rs : int array) =
    //ls, rs (末尾がそれぞれ lv, rv)から新しい標本を合成(変換)する関数

//    (ls |> Array.sum) / ls.Length, (rs |> Array.sum) / rs.Length
//    (ls.[0] + ls.[wsize - 1]) / 2, (rs.[0] + rs.[wsize - 1]) / 2
    // 左:原音+30ms遅れ程度 右:原音+60ms遅れ程度
    let c = 2646
    let delay0 = match c > wsize with
                    | true  -> wsize
                    | false -> c
//    let cycle = System.Math.Sin(System.Math.PI * float(j) / float(delay0)) / 2.0 + 0.5                   
    let i = j + (wsize - 1) - delay0
    let lcycle0 = (float(ls.[i + delay0 - 1]) - float(ls.[i + delay0 - 2])) / 3000.0
    let rcycle0 = (float(rs.[i + delay0 - 1]) - float(rs.[i + delay0 - 2])) / 3000.0
    let lcycle1 = if lcycle0 > 1.0 then 1.0 else lcycle0
    let rcycle1 = if rcycle0 > 1.0 then 1.0 else rcycle0
    let lcycle  = if lcycle1 < -1.0 then -1.0 else lcycle1
    let rcycle  = if rcycle1 < -1.0 then -1.0 else rcycle1
    let ldif = int(32767.0 * lcycle)
    let rdif = int(32767.0 * rcycle)
//    let delay = (float(delay0) * cycle) |> int
    ldif, rdif
//    (ls.[i + 0] + ls.[i + 1] + ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 4, (rs.[i + 0] + rs.[i + 1] + rs.[i + delay - 1] + rs.[i + delay - 2]) / 4
//    (ls.[i + (delay/2) - 1] + ls.[i + (delay/2) - 2]) / 2, (rs.[i + delay - 1] + rs.[i + delay - 2]) / 2

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

//    let ld = Array.create (16) 0
//    let rd = Array.create (16) 0
    let ld = Array.create (wsize - 1) 0
    let rd = Array.create (wsize - 1) 0

    //windowed を列挙するとヒープにwindowサイズのこまぎれの領域を取るので良くない
//    let ls = Array.concat [ld; la] |> Array.toSeq |> Seq.windowed (wsize)
//    let rs = Array.concat [rd; ra] |> Array.toSeq |> Seq.windowed (wsize)
//    let ls = Array.concat [ld; la] |> Array.toSeq |> Seq.windowed (2)
//    let rs = Array.concat [rd; ra] |> Array.toSeq |> Seq.windowed (2)
    let lc = Array.concat [ld; la]
    let rc = Array.concat [rd; ra]

    printfn "変換開始..."
//    let le = ls.GetEnumerator()
//    let re = ls.GetEnumerator()
    for i in 0..len - 1 do
        //変換はここ
//        let lv, rv = f la.[i] ra.[i] i (Seq.nth i ls) (Seq.nth i rs)
        // IEnumerable に対する nth は Enumerable の利点を無駄にするかも(アクセス時にEnumerateしてしまう？)
//        let lv, rv = f la.[i] ra.[i] i [|1|] [|1|]

//        le.MoveNext() |> ignore
//        re.MoveNext() |> ignore

//        let lv, rv = f la.[i] ra.[i] i (le.Current) (re.Current)
        f |> Seq.iter (fun filter ->
//            let lv, rv = filter la.[i] ra.[i] i (le.Current) (re.Current)
            let lv, rv = filter i lc rc

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

open WMPLib            
let run () =
//    let outfile = @"D:\t\Den_en_out.wav"
    let infile = @"D:\t\BMSakasama.wav"
    let outfile = @"D:\t\BMSakasama_out.wav"
    let buf = readFile infile
//    writeFile @"D:\t\Den_en_out.wav" (convert buf 44 ([filter4; filter3]))
    writeFile outfile (convert buf 44 ([filter;]))
//    let buf = readFile @"D:\t\Complicated.wav"
//    writeFile @"D:\t\Complicated_out.wav" (convert buf 44 filter2)
    let wm = new WMPLib.WindowsMediaPlayerClass()
            //wm.settings.volume = 70;
            //wm.URL = @"C:\m\work\doraque\dq04.wav";
            //wm.controls.play();
            //wm.settings.setMode("loop", true);
    wm.settings.volume <- 40;
    wm.URL <- outfile
    wm.controls.play()
