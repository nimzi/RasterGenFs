// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing


open System.Numerics

// we can use vectors in this way...
let xx = Vector2(3f,4f)


// A Cell is either Empty or Full
type Cell = E | F

// Rules are functions which take in the values of 3 Cells, and return a single Cell value
type Rule = (Cell * Cell * Cell) -> Cell

// Rule 222 implemented in code
let rule222: Rule = function 
    | (F, F, F) -> F
    | (F, F, E) -> F
    | (F, E, F) -> E
    | (F, E, E) -> F
    | (E, F, F) -> F
    | (E, F, E) -> F
    | (E, E, F) -> F
    | (E, E, E) -> E

let rule90: Rule = function
    | (F, F, F) -> E
    | (F, F, E) -> F
    | (F, E, F) -> E
    | (F, E, E) -> F
    | (E, F, F) -> F
    | (E, F, E) -> E
    | (E, E, F) -> F
    | (E, E, E) -> E

let rule30: Rule = function
    | (F, F, F) -> E
    | (F, F, E) -> E
    | (F, E, F) -> E
    | (F, E, E) -> F
    | (E, F, F) -> F
    | (E, F, E) -> F
    | (E, E, F) -> F
    | (E, E, E) -> E

let generateStandardFirstRow (width: int) =
    
    if width % 2 = 0 then invalidArg "width" (sprintf "Value must be an odd number. Value passed was %d." width)
    
    Seq.init width (function
        | n when n = (width / 2) -> F
        | _ -> E
    )

let generateNextRow (rule: Rule) (row: Cell seq) =
    let generatedCells =
        row
        |> Seq.windowed 3
        |> Seq.map (fun v ->
            rule (v.[0], v.[1], v.[2])
        )
        
    // the first and last Cells of each row are Empty since we didn't generate a value for them above
    seq {
        yield E
        yield! generatedCells
        yield E
    }

let generatePattern (rule: Rule) (firstRow: Cell seq) = 
    firstRow
    |> Seq.unfold (fun row ->
        Some(row, (generateNextRow rule row))
        )


let cols = 501;
let rows = 250;


let white = Rgba32(1f, 1f, 1f, 1f)
let black = Rgba32(0f, 0f, 0f, 1f)


let grid rule =
    generateStandardFirstRow cols
    |> generatePattern rule
    |> Seq.take rows

let withIndexes x = x |> Seq.mapi (fun index item -> (index, item))


let makeImage rule name = 
    let image = new Image<Rgba32>(cols, rows)
    for (rowIndex, row) in withIndexes (grid rule) do
        for (cellIndex, cell) in withIndexes row do
            let colour = match cell with | F -> black | E -> white
            image.[cellIndex, rowIndex] <- colour

    image.Save(name + ".png")


let makeCircle cx cy radius (image:Image<Rgba32>) = 
    let rn = System.Random()
    //let go() = rn.Next(0, rows * cols / 10) = 0

    let radiusSq = float32(radius * radius)
    for y in 0..(rows-1) do
        for x in 0..(cols-1) do
            let dy = float32(y) - cy
            let dx = float32(x) - cx 
            let distSquared = dy*dy + dx*dx
            let dist = Math.Sqrt (float distSquared)
            
            let l = float32(dist) / radius
            //if go() then
                //printfn $"({x},{y}) -> {l}"
            let color = Rgba32(1f - l, 0f, 0f, 1f)
            //let c = if distSquared < float(radius*radius) then color else white 
            //image.[x, y] <- c
            if distSquared < radiusSq then
                image.[x,y] <- color

let sqrt (x:float32) = Math.Sqrt (float x) |> float32
let abs (x:float32) = Math.Abs (float x) |> float32

let makeRing cx cy radius radius2 (image:Image<Rgba32>) = 
    let rn = System.Random()
    //let go() = rn.Next(0, rows * cols / 10) = 0

    let (radSmall,radBig) = if radius < radius2 then radius, radius2 else radius2, radius
    let radSmallSq = radSmall * radSmall
    let radBigSq = radBig * radBig 
    let radDiff = float32(radBig - radSmall)
    let halfRadDiff = radDiff / 2f
    let radMid = radSmall + halfRadDiff
    for y in 0..(rows-1) do
        for x in 0..(cols-1) do
            let dy = float32(y) - cy
            let dx = float32(x) - cx 
            let distSquared = dy*dy + dx*dx
            let dist = sqrt distSquared
            
            let l = abs(dist - radMid) / halfRadDiff
            //if go() then
                //printfn $"({x},{y}) -> {l}"
            let color = Rgba32(1f - l, 1f - l, 0f, 1f)
            //let c = if distSquared < float(radius*radius) then color else white 
            //image.[x, y] <- c
            if distSquared < radBigSq && distSquared > radSmallSq then
                image.[x,y] <- color


let makeCircles name frameNum =
    let image = new Image<Rgba32>(cols, rows)
    let cy = float32(rows) / 2f 
    let cx = float32(cols) / 2f 
    let offset = float32(frameNum) * 2f
    let offset' = float32(frameNum) 
    image.Mutate(fun ctx -> ctx.BackgroundColor(Color.Blue) |> ignore)
    makeCircle cx cy 100f image
    makeCircle (cx+20f)  (cy+30f) 50f image
    makeRing (cx-50f) (cy+ 10f) (80f + offset') (100f + offset) image
    
    //image.Mutate(fun ctx -> ctx.BinaryDither(KnownDitherings.Sierra3) |> ignore)
    //let options = GraphicsOptions()
    image.Mutate(fun ctx -> ctx.GaussianBlur(3f + offset' * 0.2f) |> ignore)
    let fileindex = sprintf "%07i" frameNum
    image.Save(name + $"_{fileindex}.png")

open System.IO

[<EntryPoint>]
let main argv =
    let img = Image.Load<Rgba32>("pills.png")

    //printfn $"Frames: {img.Frames.Count}"

    
    
    // let stream = File.OpenRead("pills.png")
    // let task = Image.LoadWithFormatAsync(stream)

    // let struct (img,format) = 
    //     async {
    //         return! Async.AwaitTask(task)
    //     } |> Async.RunSynchronously
    
    
    // let img = img.CloneAs<Rgba32>()

    let p = PointF(100f,100f)
    let s = Star  (p, 5, 60f, 130f)
    img.Mutate(fun (ctx:IImageProcessingContext) -> ctx.Invert() |> ignore; ctx.Fill(Color.BlueViolet,s) |> ignore)
    
    img.Save("pills_inv.png")

    //makeImage rule30 (nameof rule30)
    for i in 0..100 do
        makeCircles "circle" i
        printfn "%A" i


    // after images are generated use imagemagic's convert
    // convert -delay 3 circle*.png after.mpeg

    0 // return an integer exit code