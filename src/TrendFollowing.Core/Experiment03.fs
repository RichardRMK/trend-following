module TrendFollowing.Experiment03

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramSarAfInc = 0.002m
let private paramSarAfMax = 0.020m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { SarEp    : decimal
      SarAf    : decimal
      Sar      : decimal
      Trending : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { SarEp    = recordsLog.Lo
      SarAf    = paramSarAfInc
      Sar      = recordsLog.Hi
      Trending = Negative }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevSarEp = computeAdjustedAmount prevMetricsLog.SarEp
    let prevSarAf = prevMetricsLog.SarAf
    let prevSar = computeAdjustedAmount prevMetricsLog.Sar

    match prevMetricsLog.Trending with
    | Positive when recordsLog.Lo <= prevSar
        ->
        { SarEp    = recordsLog.Lo
          SarAf    = paramSarAfInc
          Sar      = prevSarEp
          Trending = Negative }
    | Negative when recordsLog.Hi >= prevSar
        ->
        { SarEp    = recordsLog.Hi
          SarAf    = paramSarAfInc
          Sar      = prevSarEp
          Trending = Positive }
    | Positive
        ->
        let incAf = if recordsLog.Hi > prevSarEp then paramSarAfInc else 0m
        { SarEp    = max recordsLog.Hi prevSarEp
          SarAf    = min paramSarAfMax (prevSarAf + incAf)
          Sar      = prevSar + (prevSarAf * (prevSarEp - prevSar))
          Trending = Positive }
    | Negative
        ->
        let incAf = if recordsLog.Lo < prevSarEp then paramSarAfInc else 0m
        { SarEp    = min recordsLog.Lo prevSarEp
          SarAf    = min paramSarAfMax (prevSarAf + incAf)
          Sar      = prevSar + (prevSarAf * (prevSarEp - prevSar))
          Trending = Negative }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let sarEp = elementLog.MetricsLog.SarEp
        let sar = elementLog.MetricsLog.Sar
        let shares = (summaryLog.ExitValue * paramRisk) / (sarEp - sar)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.Trending = Positive)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sar

//-------------------------------------------------------------------------------------------------

let dateStart = DateTime(1990, 01, 01)
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
      ReportElementLog  = reportElementLog
      ReportSummaryLog  = reportSummaryLog
      ReportJournalLog  = reportJournalLog }
