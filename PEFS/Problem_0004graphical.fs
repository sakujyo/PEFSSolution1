module PEFS.Problem_0004graphical

open System
open System.Windows.Forms
open System.Drawing

/// n が 10進で回文数なら、 palindromicNumber n n 0 は true を返します。
let rec palindromicNumber (n : int) (x : int) (y : int) : bool =
    match x with
        | 0 -> (y = n)
        | _ ->
//                let y = y * 10 + (x % 10)
            palindromicNumber n (x / 10) (y * 10 + (x % 10))

[<STAThread>]
let solve (problem : int) =
    let lower = 
        seq { for i in 1 .. problem - 1 do yield 10 }
        |> Seq.fold(fun s n ->
            s * n) 1
    let upper =
        seq { for i in 1 .. problem do yield 10 }
        |> Seq.fold(fun s n ->
            s * n) 1
        |> (fun n -> n - 1)
//        let f = new System.Windows.Forms.Form()
    
    let f = new Form()
    let p = new PictureBox()
//    let g = p.CreateGraphics()
    let b = new Bitmap(1000, 1000)
//    p.Visible <- true
//    p.Enabled <- true
//    g.FillRectangle(Drawing.Brushes.Black, 5, 5, 100, 100)
//    g.DrawLine(Drawing.Pens.Black, 0, 0, 100, 100)
//    p.Refresh()
//    p.
        
    let s = seq {
        for x in lower .. upper do
            for y in lower .. upper do
                yield x * y
    }
//    b.SetPixel(10, 10, Color.Black)
    for x in 0 .. upper do
        for y in 0 .. upper do
//            let br = (x * y - 10000) / 3860
            let br = (x * y) / 3899
//            if br > 250 then printf "%d " br
            b.SetPixel(x, y, Color.FromArgb(br, br, br))
            if palindromicNumber(x * y) (x * y) 0 then b.SetPixel(x , y , Color.FromArgb(255 - br, 255 - br, 255 - br))
//            if br = 20 then b.SetPixel(x, y, Color.Ivory)
//            if br = 128 then b.SetPixel(x , y , Color.Ivory)
    p.Image <- b
    //this.pictureBox1.Size = new System.Drawing.Size(404, 253);
    p.Size <- new Size(1000, 1000)
    //this.pictureBox1.Location = new System.Drawing.Point(13, 13);
    p.Location <- new Point(0, 0)
    f.Size <- new Size(1020, 1040)
    f.Controls.Add(p)
    do System.Windows.Forms.Application.Run(f)
    s
    |> Seq.filter (fun n -> palindromicNumber n n 0)
    |> Seq.max
//        seq {lower .. upper}
//        |> List.ofSeq
//        |> List.
//        (lower, upper)
    // 900x900画素の、積を輝度とする絵を描画してみたい
let run () =
    solve 3
