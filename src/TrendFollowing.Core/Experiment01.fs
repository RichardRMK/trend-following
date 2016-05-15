module TrendFollowing.Experiment01

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { Res            : decimal
      Sup            : decimal
      TrendDirection : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { Res            = recordsLog.Hi
      Sup            = recordsLog.Lo
      TrendDirection = Negative }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let res = max recordsLog.Hi (computeAdjustedAmount prevElementLog.MetricsLog.Res)
    let sup = min recordsLog.Lo (computeAdjustedAmount prevElementLog.MetricsLog.Sup)

    let trendDirection =
        match prevElementLog.MetricsLog with
        | prevMetricsLog when recordsLog.Lo <= prevMetricsLog.Sup -> Negative
        | prevMetricsLog when recordsLog.Hi >= prevMetricsLog.Res -> Positive
        | prevMetricsLog -> prevMetricsLog.TrendDirection

    { Res            = res
      Sup            = sup
      TrendDirection = trendDirection }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    match summaryLog.Date with

    | date when date = DateTime(2016, 01, 04)
        ->
        [| { Ticker = "A1"; Shares = 100u }
           { Ticker = "A2"; Shares = 100u }
           { Ticker = "A3"; Shares = 100u }
           { Ticker = "A4"; Shares = 100u } |]

    | date when date = DateTime(2016, 01, 11)
        ->
        [| { Ticker = "B1"; Shares = 100u }
           { Ticker = "B2"; Shares = 100u } |]

    | _ -> Array.empty

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sup

//-------------------------------------------------------------------------------------------------

let toQuote ticker (date, hi, lo, close, dividend, splitNew, splitOld) : Quote =

    { Date = date
      Ticker = ticker
      Hi = hi
      Lo = lo
      Close = close
      Dividend = dividend
      SplitNew = splitNew
      SplitOld = splitOld }

let quotesA1 =
    [| DateTime(2016, 01, 04), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 05), 103.00m, 101.00m, 102.00m, None       , None   , None
       DateTime(2016, 01, 06), 104.00m, 102.00m, 103.00m, None       , None   , None
       DateTime(2016, 01, 07), 105.00m, 103.00m, 104.00m, None       , None   , None
       DateTime(2016, 01, 08), 106.00m, 104.00m, 105.00m, None       , None   , None
       DateTime(2016, 01, 11), 107.00m, 105.00m, 106.00m, None       , None   , None
       DateTime(2016, 01, 12), 108.00m, 106.00m, 107.00m, None       , None   , None
       DateTime(2016, 01, 13), 109.00m, 107.00m, 108.00m, None       , None   , None
       DateTime(2016, 01, 14), 110.00m, 108.00m, 109.00m, None       , None   , None
       DateTime(2016, 01, 15), 111.00m, 109.00m, 110.00m, None       , None   , None |]
    |> Array.map (toQuote "A1")

let quotesA2 =
    [| DateTime(2016, 01, 04), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 05), 103.00m, 101.00m, 102.00m, None       , None   , None
       DateTime(2016, 01, 06), 104.00m, 102.00m, 103.00m, None       , None   , None
       DateTime(2016, 01, 07), 105.00m, 103.00m, 104.00m, None       , None   , None
       DateTime(2016, 01, 08), 106.00m, 104.00m, 105.00m, None       , None   , None
       DateTime(2016, 01, 11),  96.30m,  94.50m,  95.40m, Some 10.50m, None   , None
       DateTime(2016, 01, 12),  97.20m,  95.40m,  96.30m, None       , None   , None
       DateTime(2016, 01, 13),  98.10m,  96.30m,  97.20m, None       , None   , None
       DateTime(2016, 01, 14),  99.00m,  97.20m,  98.10m, None       , None   , None
       DateTime(2016, 01, 15),  99.90m,  98.10m,  99.00m, None       , None   , None |]
    |> Array.map (toQuote "A2")

let quotesA3 =
    [| DateTime(2016, 01, 04), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 05), 103.00m, 101.00m, 102.00m, None       , None   , None
       DateTime(2016, 01, 06), 104.00m, 102.00m, 103.00m, None       , None   , None
       DateTime(2016, 01, 07), 105.00m, 103.00m, 104.00m, None       , None   , None
       DateTime(2016, 01, 08), 106.00m, 104.00m, 105.00m, None       , None   , None
       DateTime(2016, 01, 11),  53.50m,  52.50m,  53.00m, None       , Some 2u, Some 1u
       DateTime(2016, 01, 12),  54.00m,  53.00m,  53.50m, None       , None   , None
       DateTime(2016, 01, 13),  54.50m,  53.50m,  54.00m, None       , None   , None
       DateTime(2016, 01, 14),  55.00m,  54.00m,  54.50m, None       , None   , None
       DateTime(2016, 01, 15),  55.50m,  54.50m,  55.00m, None       , None   , None |]
    |> Array.map (toQuote "A3")

let quotesA4 =
    [| DateTime(2016, 01, 04), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 05), 103.00m, 101.00m, 102.00m, None       , None   , None
       DateTime(2016, 01, 06), 104.00m, 102.00m, 103.00m, None       , None   , None
       DateTime(2016, 01, 07), 105.00m, 103.00m, 104.00m, None       , None   , None
       DateTime(2016, 01, 08), 106.00m, 104.00m, 105.00m, None       , None   , None
       DateTime(2016, 01, 11),  48.15m,  47.25m,  47.70m, Some 10.50m, Some 2u, Some 1u
       DateTime(2016, 01, 12),  48.60m,  47.70m,  48.15m, None       , None   , None
       DateTime(2016, 01, 13),  49.05m,  48.15m,  48.60m, None       , None   , None
       DateTime(2016, 01, 14),  49.50m,  48.60m,  49.05m, None       , None   , None
       DateTime(2016, 01, 15),  49.95m,  49.05m,  49.50m, None       , None   , None |]
    |> Array.map (toQuote "A4")

let quotesB1 =
    [| DateTime(2016, 01, 11), 100.00m,  98.00m,  99.00m, None       , None   , None
       DateTime(2016, 01, 12),  99.00m,  97.00m,  98.00m, None       , None   , None
       DateTime(2016, 01, 13),  98.00m,  96.00m,  97.00m, None       , None   , None
       DateTime(2016, 01, 14),  97.00m,  95.00m,  96.00m, None       , None   , None
       DateTime(2016, 01, 15),  98.00m,  96.00m,  97.00m, None       , None   , None
       DateTime(2016, 01, 18),  99.00m,  97.00m,  98.00m, None       , None   , None
       DateTime(2016, 01, 19), 100.00m,  98.00m,  99.00m, None       , None   , None
       DateTime(2016, 01, 20), 101.00m,  99.00m, 100.00m, None       , None   , None
       DateTime(2016, 01, 21), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 22), 103.00m, 101.00m, 102.00m, None       , None   , None |]
    |> Array.map (toQuote "B1")

let quotesB2 =
    [| DateTime(2016, 01, 11), 100.00m,  98.00m,  99.00m, None       , None   , None
       DateTime(2016, 01, 12), 101.00m,  99.00m, 100.00m, None       , None   , None
       DateTime(2016, 01, 13), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 14), 103.00m, 101.00m, 102.00m, None       , None   , None
       DateTime(2016, 01, 15), 102.00m, 100.00m, 101.00m, None       , None   , None
       DateTime(2016, 01, 18), 101.00m,  99.00m, 100.00m, None       , None   , None
       DateTime(2016, 01, 19), 100.00m,  98.00m,  99.00m, None       , None   , None
       DateTime(2016, 01, 20),  99.00m,  97.00m,  98.00m, None       , None   , None
       DateTime(2016, 01, 21),  98.00m,  96.00m,  97.00m, None       , None   , None
       DateTime(2016, 01, 22),  97.00m,  95.00m,  96.00m, None       , None   , None |]
    |> Array.map (toQuote "B2")

let quotes =
    Array.concat [ quotesA1; quotesA2; quotesA3; quotesA4; quotesB1; quotesB2 ]

let getQuotes date =
    quotes
    |> Array.filter (fun x -> x.Date = date)

let dates =
    quotes
    |> Array.map (fun x -> x.Date)
    |> Array.distinct
    |> Array.sort

//-------------------------------------------------------------------------------------------------

let reportAgent = ReportAgent()

let model =
    { ComputeMetricsLog = computeMetricsLog
      ComputeTakeOrders = computeTakeOrders
      CalculateExitStop = calculateExitStop }

let simulation =
    { Principal         = 1000000.00m
      GetQuotes         = getQuotes
      Dates             = dates
      Model             = model
      ReportElementLog  = reportAgent.ReportElementLog
      ReportSummaryLog  = reportAgent.ReportSummaryLog
      ReportJournalLog  = reportAgent.ReportJournalLog
      ReportCompletion  = reportAgent.ReportCompletion }
