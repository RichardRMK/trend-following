module TrendFollowing.Output

open System
open System.IO
open System.Text.RegularExpressions
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let private folder = Environment.GetEnvironmentVariable("UserProfile") + @"\Desktop\Output\"

let private filename<'T> =
    typeof<'T>.Name
    |> Seq.takeWhile (fun x -> x <> '`')
    |> Seq.toArray
    |> String

let private extension = ".csv"

//-------------------------------------------------------------------------------------------------

let private commaDelimited x y =
    x + "," + y

let private replace pattern replacement input =
    Regex.Replace(input, pattern, (replacement : string))

let rec private format (value : obj) =
    match value with
    | :? DateTime as value -> value.ToString("yyyy-MM-dd")
    | :? Option<obj> -> ""
    | :? Option<decimal> as value -> value |> Option.get |> format
    | :? JournalDetail -> value |> sprintf "%A" |> replace "\s+" " "
    | value -> value.ToString()

let private getFields log =
    log.GetType()
    |> Reflection.FSharpType.GetRecordFields
    |> Seq.map (fun info -> info, log)

let private getTitles logs =
    logs
    |> Seq.map getFields
    |> Seq.concat
    |> Seq.map (fun (info, log) -> info.Name)
    |> Seq.reduce commaDelimited

let private getValues logs =
    logs
    |> Seq.map getFields
    |> Seq.concat
    |> Seq.map (fun (info, log) -> info.GetValue(log))
    |> Seq.map format
    |> Seq.reduce commaDelimited

//-------------------------------------------------------------------------------------------------

let private report logs filename =

    let titles = getTitles logs
    let values = getValues logs
    let output = folder + filename + extension

    if (Directory.Exists(folder) = false) then
        Directory.CreateDirectory(folder) |> ignore

    if (File.Exists(output) = false) then
        File.WriteAllLines(output, [ titles ])

    File.AppendAllLines(output, [ values ])

//-------------------------------------------------------------------------------------------------

let reportElementLog (elementLog : ElementLog<_>) =
    let logs : obj list = [ elementLog.RecordsLog; elementLog.MetricsLog ]
    let filename = filename<ElementLog<_>> + "-" + elementLog.RecordsLog.Ticker
    report logs filename

let reportSummaryLog (summaryLog : SummaryLog) =
    let logs : obj list = [ summaryLog ]
    let filename = filename<SummaryLog>
    report logs filename

let reportTradingLog (tradingLog : TradingLog) =
    let logs : obj list = [ tradingLog ]
    let filename = filename<TradingLog>
    report logs filename
