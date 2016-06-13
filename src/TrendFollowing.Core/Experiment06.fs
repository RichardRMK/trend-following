module TrendFollowing.Experiment06

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramRes = 200
let private paramAtr = 20
let private paramAtrMult = 10.0m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback    : decimal[]
      Res            : decimal
      Atr            : decimal
      Stop           : decimal
      TrendDirection : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback    = Array.create paramRes recordsLog.Hi
      Res            = recordsLog.Hi
      Atr            = recordsLog.Hi - recordsLog.Lo
      Stop           = recordsLog.Lo - (paramAtrMult * (recordsLog.Hi - recordsLog.Lo))
      TrendDirection = Negative }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let resLookback =
        prevElementLog.MetricsLog.ResLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRes

    let res = resLookback |> Array.max

    let tr1 = recordsLog.Hi - recordsLog.Lo
    let tr2 = recordsLog.Hi - (computeAdjustedAmount prevElementLog.RecordsLog.Close)
    let tr3 = recordsLog.Lo - (computeAdjustedAmount prevElementLog.RecordsLog.Close)
    let tr = [ tr1; tr2; tr3 ] |> List.max
    
    let prevAtr = computeAdjustedAmount prevElementLog.MetricsLog.Atr
    let atr = prevAtr + ((tr - prevAtr) / decimal paramAtr)

    let stop = recordsLog.Lo - (paramAtrMult * atr)
    let stop =
        match recordsLog.ExitStop with
        | Some prevStop -> max prevStop stop
        | None -> stop

    let trendDirection =
        match prevElementLog.MetricsLog.Res with
        | res when recordsLog.Hi >= computeAdjustedAmount res -> Positive
        | res -> Negative

    { ResLookback    = resLookback
      Res            = res
      Atr            = atr
      Stop           = stop
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
