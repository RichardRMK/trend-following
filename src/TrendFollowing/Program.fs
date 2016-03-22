module Program

open System
open System.IO
open FSharp.Data

//-------------------------------------------------------------------------------------------------

[<Literal>]
let connectionName = @"name=database"

[<Literal>]
let configFile = @"..\..\private\App.config"

[<Literal>]
let sqlGetQuote = @"..\..\sql\GetQuotes.sql"

type GetQuoteCommandProvider = SqlCommandProvider<sqlGetQuote, connectionName, ConfigFile = configFile>

type Quote = GetQuoteCommandProvider.Record

type Metric =
    { Date  : DateTime
      Close : decimal
      Hi    : decimal
      Lo    : decimal
      Res   : decimal
      Sup   : decimal }

//-------------------------------------------------------------------------------------------------

let paramTicker = "BRK.A"
let paramRes = 200
let paramSup = 50

//-------------------------------------------------------------------------------------------------

let getQuotes ticker =
    use command = new GetQuoteCommandProvider()
    command.Execute(ticker)

let computeMetrics (metrics : Metric list) (quote : Quote) =

    let length = List.length metrics
    let lookbackRes = min length (paramRes - 1)
    let lookbackSup = min length (paramSup - 1)

    let res =
        let items = metrics |> List.take lookbackRes |> List.map (fun x -> x.Hi)
        let items = quote.High :: items
        items |> List.max

    let sup =
        let items = metrics |> List.take lookbackSup |> List.map (fun x -> x.Lo)
        let items = quote.Low :: items
        items |> List.min

    let metric =
        { Date = quote.Date
          Close = quote.Close
          Hi = quote.High
          Lo = quote.Low
          Res = res
          Sup = sup }

    metric :: metrics

let metricToStringHeaders =
    "Date, Close, Hi, Lo, Res, Sup"

let metricToString metric =
    let date = metric.Date.ToString("yyyy-MM-dd")
    sprintf "%s, %.2f, %.2f, %.2f, %.2f, %.2f"
        date
        metric.Close
        metric.Hi
        metric.Lo
        metric.Res
        metric.Sup

let writeToFile filename metrics =
    let path = Environment.GetEnvironmentVariable("UserProfile") + @"\Desktop\" + filename
    use stream = File.OpenWrite(path)
    use writer = new StreamWriter(stream)
    writer.WriteLine(metricToStringHeaders)
    for metric in metrics do
        let line = metricToString metric
        writer.WriteLine(line)

getQuotes paramTicker
|> List.ofSeq
|> List.fold computeMetrics List.empty
|> List.rev
|> writeToFile "output.csv"
