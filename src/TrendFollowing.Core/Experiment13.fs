module TrendFollowing.Experiment13

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 1.0m
let private paramWait = 200u
let private paramRocLookback = 200
let private paramRocI = +0.25m
let private paramRocO = -0.25m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { RocLookback    : decimal[]
      Roc            : decimal
      Stop           : decimal }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { RocLookback    = Array.create paramRocLookback recordsLog.Close
      Roc            = 0m
      Stop           = 0m }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let rocLookback =
        prevElementLog.MetricsLog.RocLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Close |]
        |> Array.take paramRocLookback

    let roc = (recordsLog.Close / (rocLookback |> Array.last)) - 1m
    let stop = (rocLookback |> Array.last) * (1m + paramRocO)

    { RocLookback    = rocLookback
      Roc            = roc
      Stop           = stop }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let hi = elementLog.RecordsLog.Hi
        let cash = summaryLog.Cash
        let shares = (cash * paramRisk) / hi

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.Roc >= paramRocI)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    match elementLog.RecordsLog.ExitStop with
    | None -> elementLog.MetricsLog.Stop
    | Some stop -> max elementLog.MetricsLog.Stop stop

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
