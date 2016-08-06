module TrendFollowing.Experiment15

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramInc = 0.002m
let private paramMult = 1.0m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { Extreme        : decimal
      Reverse        : decimal
      TrendDirection : TrendDirection
      Start          : decimal
      Diff           : decimal
      Mult           : decimal }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { Extreme        = recordsLog.Lo
      Reverse        = recordsLog.Hi
      TrendDirection = Negative
      Start          = recordsLog.Lo
      Diff           = recordsLog.Lo - recordsLog.Hi
      Mult           = 0m }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevExtreme = computeAdjustedAmount prevMetricsLog.Extreme
    let prevReverse = computeAdjustedAmount prevMetricsLog.Reverse
    let prevStart = computeAdjustedAmount prevMetricsLog.Start
    let prevDiff = computeAdjustedAmount prevMetricsLog.Diff

    let (extreme, reverse, trendDirection, start, diff) =
        match prevMetricsLog.TrendDirection with
        | Positive when recordsLog.Lo <= prevReverse
            ->
            let extreme = recordsLog.Lo
            let reverse = prevExtreme
            let diff = extreme - reverse
            (extreme, reverse, Negative, extreme, diff)
        | Negative when recordsLog.Hi >= prevReverse
            ->
            let extreme = recordsLog.Hi
            let reverse = prevExtreme
            let diff = extreme - reverse
            (extreme, reverse, Positive, extreme, diff)
        | Positive
            ->
            let extreme = max prevExtreme recordsLog.Hi
            let reverse = prevReverse + (paramInc * (extreme - prevReverse))
            (extreme, reverse, Positive, prevStart, prevDiff)
        | Negative
            ->
            let extreme = min prevExtreme recordsLog.Lo
            let reverse = 1m / ((1m / prevReverse) + (paramInc / extreme) - (paramInc / prevReverse))
            (extreme, reverse, Negative, prevStart, prevDiff)

    let mult = (extreme - start) / diff

    { Extreme        = extreme
      Reverse        = reverse
      TrendDirection = trendDirection
      Start          = start
      Diff           = diff
      Mult           = mult }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let extreme = elementLog.MetricsLog.Extreme
        let reverse = elementLog.MetricsLog.Reverse
        let shares = (summaryLog.ExitValue * paramRisk) / (extreme - reverse)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.filter (fun x -> x.MetricsLog.Mult >= paramMult)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Reverse

//-------------------------------------------------------------------------------------------------

let reportAgent = ReportAgent()

let dateStart = DateTime(1960, 01, 01)
let dateFinal = DateTime.Today
let getQuotes = Data.getQuotes dateStart dateFinal
let dates = Data.getDates dateStart dateFinal

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
