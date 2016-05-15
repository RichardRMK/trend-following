module TrendFollowing.Experiment04

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramRes = 200
let private paramSup = 200
let private paramEma = 200
let private paramRoc = 0.25m
let private paramLag = 200

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback    : decimal[]
      SupLookback    : decimal[]
      Res            : decimal
      Sup            : decimal
      TrendDirection : TrendDirection
      Ema            : decimal
      Rps            : decimal
      Roc            : decimal
      RocAdj         : decimal
      RocAdjLag      : decimal }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback    = Array.create paramRes recordsLog.Hi
      SupLookback    = Array.create paramSup recordsLog.Lo
      Res            = recordsLog.Hi
      Sup            = recordsLog.Lo
      TrendDirection = Negative
      Ema            = recordsLog.Close
      Rps            = recordsLog.Hi - recordsLog.Lo
      Roc            = 0m
      RocAdj         = 0m
      RocAdjLag      = 0m }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let resLookback =
        prevElementLog.MetricsLog.ResLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRes

    let supLookback =
        prevElementLog.MetricsLog.SupLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Lo |]
        |> Array.take paramSup

    let res = resLookback |> Array.max
    let sup = supLookback |> Array.min

    let trendDirection =
        match prevElementLog.MetricsLog with
        | prevMetricsLog when recordsLog.Hi >= computeAdjustedAmount prevMetricsLog.Res -> Positive
        | prevMetricsLog when recordsLog.Lo <= computeAdjustedAmount prevMetricsLog.Sup -> Negative
        | prevMetricsLog -> prevMetricsLog.TrendDirection

    let prevEma = computeAdjustedAmount prevElementLog.MetricsLog.Ema
    let ema = prevEma + ((recordsLog.Close - prevEma) / decimal paramEma)

    let rps = (res - sup)
    let roc = (recordsLog.Close - prevEma) / (prevEma)
    let roc = ((1.0 + float roc) ** (250.0 / float paramEma)) - 1.0 |> decimal
    let rocAdj = roc * (res / rps)

    let prevRocAdjLag = prevElementLog.MetricsLog.RocAdjLag
    let rocAdjLag = prevRocAdjLag + ((rocAdj - prevRocAdjLag) / decimal paramLag)

    { ResLookback    = resLookback
      SupLookback    = supLookback
      Res            = res
      Sup            = sup
      TrendDirection = trendDirection
      Ema            = ema
      Rps            = rps
      Roc            = roc
      RocAdj         = rocAdj
      RocAdjLag      = rocAdjLag }

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
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.filter (fun x -> x.MetricsLog.RocAdjLag >= paramRoc)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sup

//-------------------------------------------------------------------------------------------------

let reportAgent = ReportAgent()

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
      ReportElementLog  = reportAgent.ReportElementLog
      ReportSummaryLog  = reportAgent.ReportSummaryLog
      ReportJournalLog  = reportAgent.ReportJournalLog
      ReportCompletion  = reportAgent.ReportCompletion }
