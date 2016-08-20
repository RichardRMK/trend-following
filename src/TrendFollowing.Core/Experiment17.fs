module TrendFollowing.Experiment17

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramDays = 250u
let private paramLevels = 10

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { Extreme        : decimal
      Reverse        : decimal
      TrendDirection : TrendDirection
      Start          : decimal
      Level          : int
      Signal         : bool }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { Extreme        = recordsLog.Lo
      Reverse        = recordsLog.Hi
      TrendDirection = Negative
      Start          = recordsLog.Lo
      Level          = 0
      Signal         = false }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevExtreme = computeAdjustedAmount prevMetricsLog.Extreme
    let prevReverse = computeAdjustedAmount prevMetricsLog.Reverse
    let prevStart = computeAdjustedAmount prevMetricsLog.Start
    let prevLevel = prevMetricsLog.Level

    let (extreme, reverse, trendDirection, start, level, signal) =
        match prevMetricsLog.TrendDirection with
        | Positive when recordsLog.Lo <= prevReverse
            ->
            let extreme = recordsLog.Lo
            let reverse = prevExtreme
            let start = extreme
            let level = 0
            (extreme, reverse, Negative, start, level, true)
        | Negative when recordsLog.Hi >= prevReverse
            ->
            let extreme = recordsLog.Hi
            let reverse = prevExtreme
            let start = extreme
            let level = 0
            (extreme, reverse, Positive, start, level, true)
        | Positive
            ->
            let extreme = max prevExtreme recordsLog.Hi
            let inc = ((float (extreme / prevReverse)) ** (1.0 / float paramDays) |> decimal) - 1m
            let reverse = prevReverse + (inc * (extreme - prevReverse))
            let start, level, signal =
                if reverse >= prevStart && prevLevel < paramLevels then
                    let start = extreme
                    let level = prevLevel + 1
                    let signal = true
                    (start, level, signal)
                else
                    let start = prevStart
                    let level = prevLevel
                    let signal = false
                    (start, level, signal)
            (extreme, reverse, Positive, start, level, signal)
        | Negative
            ->
            let extreme = min prevExtreme recordsLog.Lo
            let inc = ((float (prevReverse / extreme)) ** (1.0 / float paramDays) |> decimal) - 1m
            let reverse = 1m / ((1m / prevReverse) + (inc / extreme) - (inc / prevReverse))
            let start, level, signal =
                if reverse <= prevStart && prevLevel < paramLevels then
                    let start = extreme
                    let level = prevLevel + 1
                    let signal = true
                    (start, level, signal)
                else
                    let start = prevStart
                    let level = prevLevel
                    let signal = false
                    (start, level, signal)
            (extreme, reverse, Negative, start, level, signal)

    { Extreme        = extreme
      Reverse        = reverse
      TrendDirection = trendDirection
      Start          = start
      Level          = level
      Signal         = signal }

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
    |> Array.filter (fun x -> x.MetricsLog.Signal)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
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
