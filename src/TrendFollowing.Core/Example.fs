module TrendFollowing.Example

open System
open System.IO
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let toQuote ticker (date, hi, lo, dividend, splitNew, splitOld) : Quote =

    { Date = date
      Ticker = ticker
      Hi = hi
      Lo = lo
      Close = (hi + lo) / 2m
      Dividend = dividend
      SplitNew = splitNew
      SplitOld = splitOld }

let quotesA1 =

    [| DateTime(2016, 01, 04), 102.00m, 100.00m, None, None, None
       DateTime(2016, 01, 05), 103.00m, 101.00m, None, None, None
       DateTime(2016, 01, 06), 104.00m, 102.00m, None, None, None
       DateTime(2016, 01, 07), 105.00m, 103.00m, None, None, None
       DateTime(2016, 01, 08), 106.00m, 104.00m, None, None, None
       DateTime(2016, 01, 11), 107.00m, 105.00m, None, None, None
       DateTime(2016, 01, 12), 108.00m, 106.00m, None, None, None
       DateTime(2016, 01, 13), 109.00m, 107.00m, None, None, None
       DateTime(2016, 01, 14), 110.00m, 108.00m, None, None, None
       DateTime(2016, 01, 15), 111.00m, 109.00m, None, None, None |]
    |> Array.map (toQuote "A1")

let quotesB1 =

    [| DateTime(2016, 01, 11), 100.00m,  98.00m, None, None, None
       DateTime(2016, 01, 12),  99.00m,  97.00m, None, None, None
       DateTime(2016, 01, 13),  98.00m,  96.00m, None, None, None
       DateTime(2016, 01, 14),  97.00m,  95.00m, None, None, None
       DateTime(2016, 01, 15),  98.00m,  96.00m, None, None, None
       DateTime(2016, 01, 18),  99.00m,  97.00m, None, None, None
       DateTime(2016, 01, 19), 100.00m,  98.00m, None, None, None
       DateTime(2016, 01, 20), 101.00m,  99.00m, None, None, None
       DateTime(2016, 01, 21), 102.00m, 100.00m, None, None, None
       DateTime(2016, 01, 22), 103.00m, 101.00m, None, None, None |]
    |> Array.map (toQuote "B1")

let quotes =
    Array.concat [ quotesA1; quotesB1 ]

let dateSequence =
    quotes
    |> Array.map (fun x -> x.Date)
    |> Array.distinct
    |> Array.sort

let getQuotes date =
    quotes
    |> Array.filter (fun x -> x.Date = date)

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { Res        : decimal
      Sup        : decimal
      IsTrending : bool }

let private computeMetricsInit (recordsLog : RecordsLog) =

    { Res = recordsLog.Hi
      Sup = recordsLog.Lo
      IsTrending = false }

let private computeMetricsNext (recordsLog : RecordsLog) (prevMetricsLog : MetricsLog) =

    let res = max prevMetricsLog.Res recordsLog.Hi
    let sup = min prevMetricsLog.Sup recordsLog.Lo

    let isTrending =
        match prevMetricsLog with
        | prevMetricsLog when recordsLog.Lo <= prevMetricsLog.Sup -> false
        | prevMetricsLog when recordsLog.Hi >= prevMetricsLog.Res -> true
        | prevMetricsLog -> prevMetricsLog.IsTrending

    { Res = res
      Sup = sup
      IsTrending = isTrending }

let computeMetrics (recordsLog : RecordsLog) = function
    | None      -> computeMetricsInit recordsLog
    | Some prev -> computeMetricsNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let generateOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) =

    let takeOrders =
        match summaryLog.Date with
        | date when date = DateTime(2016, 01, 06) -> [| Take { Ticker = "A1"; Shares = 100 } |]
        | _ -> Array.empty

    let exitOrders =
        match summaryLog.Date with
        | date when date = DateTime(2016, 01, 06) -> [| Exit { Ticker = "A1"; Shares = 100; StopLoss = 100.50m } |]
        | date when date = DateTime(2016, 01, 07) -> [| Exit { Ticker = "A1"; Shares = 100; StopLoss = 102.50m } |]
        | date when date = DateTime(2016, 01, 08) -> [| Exit { Ticker = "A1"; Shares = 100; StopLoss = 104.50m } |]
        | date when date = DateTime(2016, 01, 11) -> [| Exit { Ticker = "A1"; Shares = 100; StopLoss = 106.50m } |]
        | _ -> Array.empty

    Array.concat [ takeOrders; exitOrders ]

//-------------------------------------------------------------------------------------------------

let outputPath = Environment.GetEnvironmentVariable("UserProfile") + @"\Desktop\Example\"

let headerElementLog = "Date, Ticker, Count, Hi, Lo, Close, Dividend, SplitNew, SplitOld, DeltaHi, DeltaLo, Shares, StopLoss, Res, Sup, IsTrending"
let headerSummaryLog = "Date, Cash, Equity, ExitValue, Peak, Drawdown, Leverage"
let headerTradingLog = "Date, Ticker, Shares, Price"

let emitElementLog (elementLog : ElementLog<MetricsLog>) =
    let path = outputPath + "ElementLog-" + elementLog.RecordsLog.Ticker + ".csv"
    if (Directory.Exists(outputPath) = false) then Directory.CreateDirectory(outputPath) |> ignore
    if (File.Exists(path) = false) then File.WriteAllLines(path, [ headerElementLog ])
    let content =
        sprintf "%s, %s, %i, %.2f, %.2f, %.2f, %.5f, %i, %i, %.10f, %.10f, %i, %.2f, %.2f, %.2f, %A"
            (elementLog.RecordsLog.Date.ToString("yyyy-MM-dd"))
            elementLog.RecordsLog.Ticker
            elementLog.RecordsLog.Count
            elementLog.RecordsLog.Hi
            elementLog.RecordsLog.Lo
            elementLog.RecordsLog.Close
            elementLog.RecordsLog.Dividend
            elementLog.RecordsLog.SplitNew
            elementLog.RecordsLog.SplitOld
            elementLog.RecordsLog.DeltaHi
            elementLog.RecordsLog.DeltaLo
            elementLog.RecordsLog.Shares
            elementLog.RecordsLog.StopLoss
            elementLog.MetricsLog.Res
            elementLog.MetricsLog.Sup
            elementLog.MetricsLog.IsTrending
    File.AppendAllLines(path, [ content ])

let emitSummaryLog (summaryLog : SummaryLog) =
    let path = outputPath + "SummaryLog.csv"
    if (Directory.Exists(outputPath) = false) then Directory.CreateDirectory(outputPath) |> ignore
    if (File.Exists(path) = false) then File.WriteAllLines(path, [ headerSummaryLog ])
    let content =
        sprintf "%s, %.2f, %.2f, %.2f, %.2f, %10f, %10f"
            (summaryLog.Date.ToString("yyyy-MM-dd"))
            summaryLog.Cash
            summaryLog.Equity
            summaryLog.ExitValue
            summaryLog.Peak
            summaryLog.Drawdown
            summaryLog.Leverage
    File.AppendAllLines(path, [ content ])

let emitTradingLog (tradingLog : TradingLog) =
    let path = outputPath + "TradingLog.csv"
    if (Directory.Exists(outputPath) = false) then Directory.CreateDirectory(outputPath) |> ignore
    if (File.Exists(path) = false) then File.WriteAllLines(path, [ headerTradingLog ])
    let content =
        sprintf "%s, %s, %i, %.2f"
            (tradingLog.Date.ToString("yyyy-MM-dd"))
            tradingLog.Ticker
            tradingLog.Shares
            tradingLog.Price
    File.AppendAllLines(path, [ content ])

//-------------------------------------------------------------------------------------------------

let system =
    { Principal      = 100000m
      DateSequence   = dateSequence
      GetQuotes      = getQuotes
      ComputeMetrics = computeMetrics
      GenerateOrders = generateOrders
      EmitElementLog = emitElementLog
      EmitSummaryLog = emitSummaryLog
      EmitTradingLog = emitTradingLog }
