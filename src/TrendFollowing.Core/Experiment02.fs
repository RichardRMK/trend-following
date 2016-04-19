module TrendFollowing.Experiment02

open System
open TrendFollowing.Types
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramRes = 200
let private paramSup = 50

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback : decimal[]
      SupLookback : decimal[]
      Res         : decimal
      Sup         : decimal
      Trending    : bool }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback = Array.create paramRes recordsLog.Hi
      SupLookback = Array.create paramSup recordsLog.Lo
      Res         = recordsLog.Hi
      Sup         = recordsLog.Lo
      Trending    = false }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevMetricsLog : MetricsLog) =

    let resLookback =
        prevMetricsLog.ResLookback
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRes

    let supLookback =
        prevMetricsLog.SupLookback
        |> Array.append [| recordsLog.Lo |]
        |> Array.take paramSup

    let res = resLookback |> Array.max
    let sup = supLookback |> Array.min

    let trending =
        match prevMetricsLog with
        | prevMetricsLog when recordsLog.Hi >= prevMetricsLog.Res -> true
        | prevMetricsLog when recordsLog.Lo <= prevMetricsLog.Sup -> false
        | prevMetricsLog -> prevMetricsLog.Trending

    { ResLookback = resLookback
      SupLookback = supLookback
      Res         = res
      Sup         = sup
      Trending    = trending }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let res = elementLog.MetricsLog.Res
        let sup = elementLog.MetricsLog.Sup
        let shares = (summaryLog.ExitValue * paramRisk) / (res - sup)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.Trending)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sup

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
