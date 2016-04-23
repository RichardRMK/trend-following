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

let private getFields types =
    types
    |> Seq.map Reflection.FSharpType.GetRecordFields
    |> Seq.toArray

let private getTitles (fields : Reflection.PropertyInfo[][]) =
    fields
    |> Seq.concat
    |> Seq.map (fun field -> field.Name)
    |> Seq.reduce commaDelimited

let private getValues (fields : Reflection.PropertyInfo[][]) logs =
    let mapping fields log = Array.map (fun field -> field, log) fields
    Seq.map2 mapping fields logs
    |> Seq.concat
    |> Seq.map (fun (field, log) -> field.GetValue(log))
    |> Seq.map format
    |> Seq.reduce commaDelimited

//-------------------------------------------------------------------------------------------------

let private report fields logs filename =

    let titles = getTitles fields
    let values = getValues fields logs
    let output = folder + filename + extension

    if (Directory.Exists(folder) = false) then
        Directory.CreateDirectory(folder) |> ignore

    if (File.Exists(output) = false) then
        File.WriteAllLines(output, [ titles ])

    File.AppendAllLines(output, [ values ])

//-------------------------------------------------------------------------------------------------

let reportElementLog<'T> =
    let fields = getFields [ typeof<RecordsLog>; typeof<'T> ]
    fun (elementLog : ElementLog<'T>) ->
        let logs : obj list = [ elementLog.RecordsLog; elementLog.MetricsLog ]
        let filename = filename<ElementLog<_>> + "-" + elementLog.RecordsLog.Ticker
        report fields logs filename

let reportSummaryLog =
    let fields = getFields [ typeof<SummaryLog> ]
    fun (summaryLog : SummaryLog) ->
        let logs : obj list = [ summaryLog ]
        let filename = filename<SummaryLog>
        report fields logs filename

let reportJournalLog =
    let fields = getFields [ typeof<JournalLog> ]
    fun (journalLog : JournalLog) ->
        let logs : obj list = [ journalLog ]
        let filename = filename<JournalLog>
        report fields logs filename
