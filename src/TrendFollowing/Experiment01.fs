module Experiment01

open System
open System.IO
open TrendFollowing.Data

//-------------------------------------------------------------------------------------------------

let paramTicker = "BRK.A"
let paramRes = 200
let paramSup = 50
let paramSarAfInc = 0.02m
let paramSarAfMax = 0.20m

//-------------------------------------------------------------------------------------------------

type SarDirection = Pos | Neg

type Metric =
    { Date         : DateTime
      Close        : decimal
      Hi           : decimal
      Lo           : decimal
      Res          : decimal
      Sup          : decimal
      SarEp        : decimal
      SarAf        : decimal
      SarDirection : SarDirection
      Sar          : decimal }

//-------------------------------------------------------------------------------------------------

let computeMetrics (metrics : Metric list) (quote : Quote) =

    let length = List.length metrics
    let lookbackRes = min length (paramRes - 1)
    let lookbackSup = min length (paramSup - 1)

    let res =
        let items = metrics |> List.take lookbackRes |> List.map (fun x -> x.Hi)
        let items = quote.Hi :: items
        items |> List.max

    let sup =
        let items = metrics |> List.take lookbackSup |> List.map (fun x -> x.Lo)
        let items = quote.Lo :: items
        items |> List.min

    let sarEp, sarAf, sarDirection, sar =
        if length = 0 then
            let sarEp = quote.Lo
            let sarAf = paramSarAfInc
            let sar = quote.Hi
            sarEp, sarAf, Neg, sar
        else
            match metrics.Head.SarDirection with
            | Pos
                ->
                if quote.Lo <= metrics.Head.Sar then
                    let sarEp = quote.Lo
                    let sarAf = paramSarAfInc
                    let sar = metrics.Head.SarEp
                    sarEp, sarAf, Neg, sar
                else
                    let sarEp = max metrics.Head.SarEp quote.Hi
                    let sarAf = min paramSarAfMax (metrics.Head.SarAf + paramSarAfInc)
                    let sar = metrics.Head.Sar + (sarAf * (sarEp - metrics.Head.Sar))
                    sarEp, sarAf, Pos, sar
            | Neg
                ->
                if quote.Hi >= metrics.Head.Sar then
                    let sarEp = quote.Hi
                    let sarAf = paramSarAfInc
                    let sar = metrics.Head.SarEp
                    sarEp, sarAf, Pos, sar
                else
                    let sarEp = min metrics.Head.SarEp quote.Lo
                    let sarAf = min paramSarAfMax (metrics.Head.SarAf + paramSarAfInc)
                    let sar = metrics.Head.Sar + (sarAf * (sarEp - metrics.Head.Sar))
                    sarEp, sarAf, Neg, sar

    let metric =
        { Date = quote.Date
          Close = quote.Close
          Hi = quote.Hi
          Lo = quote.Lo
          Res = res
          Sup = sup
          SarEp = sarEp
          SarAf = sarAf
          SarDirection = sarDirection
          Sar = sar }

    metric :: metrics

let metricToStringHeaders =
    "Date, Close, Hi, Lo, Res, Sup, SarEp, SarAf, SarDirection, Sar"

let metricToString metric =
    let date = metric.Date.ToString("yyyy-MM-dd")
    sprintf "%s, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %A, %.2f"
        date
        metric.Close
        metric.Hi
        metric.Lo
        metric.Res
        metric.Sup
        metric.SarEp
        metric.SarAf
        metric.SarDirection
        metric.Sar

let writeToFile filename metrics =
    let path = Environment.GetEnvironmentVariable("UserProfile") + @"\Desktop\" + filename
    use writer = File.CreateText(path)
    writer.WriteLine(metricToStringHeaders)
    for metric in metrics do
        let line = metricToString metric
        writer.WriteLine(line)

let execute () =
    getQuotes paramTicker
    |> List.ofSeq
    |> List.fold computeMetrics List.empty
    |> List.rev
    |> writeToFile "output.csv"
