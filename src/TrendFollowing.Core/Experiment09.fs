module TrendFollowing.Experiment09

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramRes = 200
let private paramSup = 200
let private paramInc = 0.002m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback    : decimal[]
      SupLookback    : decimal[]
      Res            : decimal
      Sup            : decimal
      Stop           : decimal
      Signal         : bool
      TrendDirection : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback    = Array.create paramRes recordsLog.Hi
      SupLookback    = Array.create paramSup recordsLog.Lo
      Res            = recordsLog.Hi
      Sup            = recordsLog.Lo
      Stop           = recordsLog.Lo
      Signal         = false
      TrendDirection = Negative }

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

    let stop =
        match recordsLog.ExitStop with
        | None -> sup
        | Some exitStop -> exitStop + (paramInc * (prevElementLog.MetricsLog.Res - exitStop))

    let signal = recordsLog.Hi > computeAdjustedAmount prevElementLog.MetricsLog.Res

    let trendDirection =
        match recordsLog.ExitStop, signal with
        | Some exitStop, _ when recordsLog.Lo <= exitStop -> Negative
        | _, true -> Positive
        | _ -> prevElementLog.MetricsLog.TrendDirection

    { ResLookback    = resLookback
      SupLookback    = supLookback
      Res            = res
      Sup            = sup
      Stop           = stop
      Signal         = signal
      TrendDirection = trendDirection }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let res = elementLog.MetricsLog.Res
        let stop = elementLog.MetricsLog.Stop
        let shares = (summaryLog.ExitValue * paramRisk) / (res - stop)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.Signal)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Stop

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
