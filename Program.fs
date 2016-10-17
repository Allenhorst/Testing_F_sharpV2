// Дополнительные сведения о F# см. на http://fsharp.net
open Microsoft.FSharp.Math
open System
open System.Drawing
open System.Windows.Forms
open System.Numerics
open System.Threading.Tasks

let maxIteration = 50 //Количество итераций

let modSquared (c : Complex) = c.Real * c.Real + c.Imaginary * c.Imaginary

type MandelbrotResult = 
    | DidNotEscape
    | Escaped of int 

type Point = {X : int; Y: int}

type MandelbrotForm() =
    inherit Form()
    do base.DoubleBuffered<-true

    let mutable zoom = 100.0
    let mutable center = {X=0; Y=0;}
    let mutable moving = false
    let mutable drawing = false
    let mutable startPoint = {X=0;Y=0}
    let mutable endPoint = {X=0;Y=0}

    member self.Zoom
        with get() = zoom
        and set(v) = zoom <- v

    member self.Center
        with get() = center
        and set(v) = center <- v

    member self.Moving
        with get() = moving
        and set(v) = moving <- v

    member self.Drawing
        with get() = drawing
        and set(v) = drawing <- v

    member self.StartPoint
        with get() = startPoint
        and set(v) = startPoint <- v

    member self.EndPoint
        with get() = endPoint
        and set(v) = endPoint <- v

    member self.MiddleX
        with get() = (int)(self.Width/2)

    member self.MiddleY
        with get() = (int)(self.Height/2)

    member self.Title =
        String.Format("Mandelbrot, Zoom = {0}", self.Zoom)
    
let mandelbrot c = 
    let rec mandelbrotInner z iterations = 
        //if(modSquared z >= 4.0)
        if(modSquared z >= 15.0)
            then Escaped iterations
        elif iterations = maxIteration
            then DidNotEscape
        //else mandelbrotInner (( z * z *  z ) + c) (iterations + 1) //Комплексные числа
        else mandelbrotInner ((c / cos(z))*(c / cos (z))) (iterations + 1) 
 
    mandelbrotInner c 0

let min a b = if (a > b) then b
              else a

let max a b = if (a < b) then b
              else a

//let mandelbrotcolor dx dy =
//      match mandelbrot (Complex(dx, dy)) with
//        | DidNotEscape -> System.Drawing.Color.AntiqueWhite
//        | Escaped e -> System.Drawing.Color.FromArgb((min 255 (10*e)), (min 255 3*e),0)

let mandelbrotcolor dx dy =
      match mandelbrot (Complex(dx, dy)) with
        | DidNotEscape -> System.Drawing.Color.AntiqueWhite
        | Escaped e -> if (e < 1) then  System.Drawing.Color.FromArgb(0, 0 ,0)
                        elif (e < 8 ) then  System.Drawing.Color.FromArgb(174, 20 ,20)
                        elif (e < 13 ) then  System.Drawing.Color.FromArgb(240, 50  ,50)
                       // elif (e < 25 ) then  System.Drawing.Color.FromArgb(225, 225 ,36)
                        else System.Drawing.Color.FromArgb(225, 225 ,36) //System.Drawing.Color.FromArgb(255,255 ,255)
       
let pixelcolor x y newX newY zoom xmid ymid =
    mandelbrotcolor (((float)(x - xmid + newX))/zoom) (((float)(y - ymid + newY))/zoom)

let newfractal zoom width height newX newY xmid ymid = 
    let bm = new Bitmap(width, height, Imaging.PixelFormat.Format24bppRgb)
    for y0 in [0..height-1] do
        for x0 in [0..width-1] do
            bm.SetPixel(x0, y0, (pixelcolor x0 y0 newX newY zoom xmid ymid))
    bm

let drawfractal (form : MandelbrotForm) = 
    form.Cursor <- Cursors.WaitCursor
    form.Drawing <- true

    form.BackgroundImage <- newfractal form.Zoom form.Width form.Height form.Center.X form.Center.Y form.MiddleX form.MiddleY
    form.Invalidate()
    
    form.Drawing <- false
    form.Cursor <- Cursors.Default

let form = new MandelbrotForm (Text="Mandelbrot", Width=800, Height=614)

do
    form.MouseWheel.Add(fun e -> 
        if not form.Drawing then
            form.Zoom <- form.Zoom + (float)e.Delta/12.0; 
            form.Text <- form.Title
            drawfractal form
    )

    form.MouseDown.Add (fun e -> 
        if not form.Drawing then
            form.Moving <- true
            form.Cursor <- Cursors.Hand
            form.StartPoint <- {X=e.X; Y=e.Y}
    )

    form.MouseUp.Add (fun _ -> 
        if form.Moving then
            form.Moving <- false
            form.Cursor <- Cursors.Default
            form.Center <- {X=form.Center.X-form.EndPoint.X+form.StartPoint.X; 
                            Y=form.Center.Y-form.EndPoint.Y+form.StartPoint.Y}
            drawfractal form
    )

    form.MouseMove.Add (fun e -> 
        if form.Moving then
            form.EndPoint <- {X=e.X; Y=e.Y}
    )

    form.Text <- form.Title
    drawfractal form
    
[<STAThread>]    
do Application.Run(form)

