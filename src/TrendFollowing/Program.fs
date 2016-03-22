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
    { Date : DateTime
      Hi : decimal
      Lo : decimal
      Close : decimal }

//-------------------------------------------------------------------------------------------------

let getQuotes ticker =
    use command = new GetQuoteCommandProvider()
    command.Execute(ticker)

let quoteToMetric (quote : Quote) =

    { Date = quote.Date
      Hi = quote.High
      Lo = quote.Low
      Close = quote.Close }

let metricToStringHeaders =
    "Date, Hi, Lo, Close"

let metricToString metric =
    let date = metric.Date.ToString("yyyy-MM-dd")
    sprintf "%s, %.2f, %.2f, %.2f"
        date
        metric.Hi
        metric.Lo
        metric.Close

let writeToFile filename metrics =
    let path = Environment.GetEnvironmentVariable("UserProfile") + @"\Desktop\" + filename
    use stream = File.OpenWrite(path)
    use writer = new StreamWriter(stream)
    writer.WriteLine(metricToStringHeaders)
    for metric in metrics do
        let line = metricToString metric
        writer.WriteLine(line)

getQuotes "BRK.A"
|> Seq.map quoteToMetric
|> Seq.toArray
|> writeToFile "output.csv"
