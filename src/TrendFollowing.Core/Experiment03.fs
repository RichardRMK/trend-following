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

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevMetricsLog : MetricsLog) =

    match prevMetricsLog.Trending with
    | Pos when recordsLog.Lo <= prevMetricsLog.Sar
        ->
        { SarEp    = recordsLog.Lo
          SarAf    = paramSarAfInc
          Sar      = prevMetricsLog.SarEp
          Trending = Neg }
    | Neg when recordsLog.Hi >= prevMetricsLog.Sar
        ->
        { SarEp    = recordsLog.Hi
          SarAf    = paramSarAfInc
          Sar      = prevMetricsLog.SarEp
          Trending = Pos }
    | Pos
        ->
        let incAf = if recordsLog.Hi > prevMetricsLog.SarEp then paramSarAfInc else 0m
        { SarEp    = max recordsLog.Hi prevMetricsLog.SarEp
          SarAf    = min paramSarAfMax (prevMetricsLog.SarAf + incAf)
          Sar      = prevMetricsLog.Sar + (prevMetricsLog.SarAf * (prevMetricsLog.SarEp - prevMetricsLog.Sar))
          Trending = Pos }
    | Neg
        ->
        let incAf = if recordsLog.Lo < prevMetricsLog.SarEp then paramSarAfInc else 0m
        { SarEp    = min recordsLog.Lo prevMetricsLog.SarEp
          SarAf    = min paramSarAfMax (prevMetricsLog.SarAf + incAf)
          Sar      = prevMetricsLog.Sar + (prevMetricsLog.SarAf * (prevMetricsLog.SarEp - prevMetricsLog.Sar))
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
