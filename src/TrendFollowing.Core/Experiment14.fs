module TrendFollowing.Experiment14

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramInc1 = 0.008m
let private paramInc2 = 0.002m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { Extreme1        : decimal
      Reverse1        : decimal
      TrendDirection1 : TrendDirection
      Extreme2        : decimal
      Reverse2        : decimal
      TrendDirection2 : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { Extreme1        = recordsLog.Lo
      Reverse1        = recordsLog.Hi
      TrendDirection1 = Negative
      Extreme2        = recordsLog.Lo
      Reverse2        = recordsLog.Hi
      TrendDirection2 = Negative }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevExtreme1 = computeAdjustedAmount prevMetricsLog.Extreme1
    let prevReverse1 = computeAdjustedAmount prevMetricsLog.Reverse1
    let prevExtreme2 = computeAdjustedAmount prevMetricsLog.Extreme2
    let prevReverse2 = computeAdjustedAmount prevMetricsLog.Reverse2

    let (extreme1, reverse1, trendDirection1) =
        match prevMetricsLog.TrendDirection1 with
        | Positive when recordsLog.Lo <= prevReverse1
            ->
            let extreme = recordsLog.Lo
            let reverse = prevExtreme1
            (extreme, reverse, Negative)
        | Negative when recordsLog.Hi >= prevReverse1
            ->
            let extreme = recordsLog.Hi
            let reverse = prevExtreme1
            (extreme, reverse, Positive)
        | Positive
            ->
            let extreme = max prevExtreme1 recordsLog.Hi
            let reverse = prevReverse1 + (paramInc1 * (extreme - prevReverse1))
            (extreme, reverse, Positive)
        | Negative
            ->
            let extreme = min prevExtreme1 recordsLog.Lo
            let reverse = 1m / ((1m / prevReverse1) + (paramInc1 / extreme) - (paramInc1 / prevReverse1))
            (extreme, reverse, Negative)

    let (extreme2, reverse2, trendDirection2) =
        match prevMetricsLog.TrendDirection2 with
        | Positive when recordsLog.Lo <= prevReverse2
            ->
            let extreme = recordsLog.Lo
            let reverse = prevExtreme2
            (extreme, reverse, Negative)
        | Negative when recordsLog.Hi >= prevReverse2
            ->
            let extreme = recordsLog.Hi
            let reverse = prevExtreme2
            (extreme, reverse, Positive)
        | Positive
            ->
            let extreme = max prevExtreme2 recordsLog.Hi
            let reverse = prevReverse2 + (paramInc2 * (extreme - prevReverse2))
            (extreme, reverse, Positive)
        | Negative
            ->
            let extreme = min prevExtreme2 recordsLog.Lo
            let reverse = 1m / ((1m / prevReverse2) + (paramInc2 / extreme) - (paramInc2 / prevReverse2))
            (extreme, reverse, Negative)

    { Extreme1        = extreme1
      Reverse1        = reverse1
      TrendDirection1 = trendDirection1
      Extreme2        = extreme2
      Reverse2        = reverse2
      TrendDirection2 = trendDirection2 }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let extreme = elementLog.MetricsLog.Extreme1
        let reverse = elementLog.MetricsLog.Reverse1
        let shares = (summaryLog.ExitValue * paramRisk) / (extreme - reverse)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection1 = Positive)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection2 = Positive)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Reverse1

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
