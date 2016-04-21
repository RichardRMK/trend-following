module TrendFollowing.Experiment03

open System
open TrendFollowing.Types
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramSarAfInc = 0.002m
let private paramSarAfMax = 0.020m

//-------------------------------------------------------------------------------------------------

type TrendDirection = Pos | Neg

type MetricsLog =
    { SarEp    : decimal
      SarAf    : decimal
      Sar      : decimal
      Trending : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { SarEp    = recordsLog.Lo
      SarAf    = paramSarAfInc
      Sar      = recordsLog.Hi
      Trending = Neg }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevSarEp = computeAdjustedAmount prevMetricsLog.SarEp
    let prevSarAf = prevMetricsLog.SarAf
    let prevSar = computeAdjustedAmount prevMetricsLog.Sar

    match prevMetricsLog.Trending with
    | Pos when recordsLog.Lo <= prevSar
        ->
        { SarEp    = recordsLog.Lo
          SarAf    = paramSarAfInc
          Sar      = prevSarEp
          Trending = Neg }
    | Neg when recordsLog.Hi >= prevSar
        ->
        { SarEp    = recordsLog.Hi
          SarAf    = paramSarAfInc
          Sar      = prevSarEp
          Trending = Pos }
    | Pos
        ->
        let incAf = if recordsLog.Hi > prevSarEp then paramSarAfInc else 0m
        { SarEp    = max recordsLog.Hi prevSarEp
          SarAf    = min paramSarAfMax (prevSarAf + incAf)
          Sar      = prevSar + (prevSarAf * (prevSarEp - prevSar))
          Trending = Pos }
    | Neg
        ->
        let incAf = if recordsLog.Lo < prevSarEp then paramSarAfInc else 0m
        { SarEp    = min recordsLog.Lo prevSarEp
          SarAf    = min paramSarAfMax (prevSarAf + incAf)
          Sar      = prevSar + (prevSarAf * (prevSarEp - prevSar))
          Trending = Neg }

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
    |> Array.filter (fun x -> x.MetricsLog.Trending = Pos)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sar

//-------------------------------------------------------------------------------------------------

let getDates dateFrom dateTo =

    let holidays = Data.getHolidays dateFrom dateTo

    let generator = function
        | date when date > dateTo-> None
        | date -> Some (date, date + TimeSpan.FromDays(1.0))

    let isWeekend (date : DateTime) =
        match date.DayOfWeek with
        | DayOfWeek.Saturday -> true
        | DayOfWeek.Sunday   -> true
        | _ -> false

    let isHoliday date =
        holidays |> Array.contains date

    dateFrom
    |> Seq.unfold generator
    |> Seq.filter (not << isWeekend)
    |> Seq.filter (not << isHoliday)

let dates = getDates (DateTime(1990, 01, 01)) (DateTime(2016, 04, 17))

//-------------------------------------------------------------------------------------------------

let model =
    { GetQuotes         = Data.getQuotes
      ComputeMetricsLog = computeMetricsLog
      ComputeTakeOrders = computeTakeOrders
      CalculateExitStop = calculateExitStop }

let simulation =
    { Principal         = 1000000.00m
      Dates             = dates
      Model             = model
      ReportElementLog  = reportElementLog
      ReportSummaryLog  = reportSummaryLog
      ReportJournalLog  = reportJournalLog }
