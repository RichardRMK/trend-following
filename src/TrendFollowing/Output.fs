module TrendFollowing.Output

open System
open System.IO
open System.Text.RegularExpressions
open TrendFollowing.Types
open TrendFollowing.Metrics

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

let private formatDateTime (value : DateTime) =
    value.ToString("yyyy-MM-dd")

let private formatJournalDetail (value : JournalDetail) =
    value
    |> sprintf "%A"
    |> replace "\s+" " "

let private formatTrendDirection = function
    | Positive -> "Positive"
    | Negative -> "Negative"

let rec private format (value : obj) =
    match value with
    | :? Option<obj>     -> ""
    | :? Option<decimal> as value -> value |> Option.get |> format
    | :? DateTime        as value -> value |> formatDateTime
    | :? JournalDetail   as value -> value |> formatJournalDetail
    | :? TrendDirection  as value -> value |> formatTrendDirection
    | _                  -> value.ToString()

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

type private ReportAgentMessage<'T> =
    | Report of 'T
    | Finish of AsyncReplyChannel<unit>

let private createReportAgent fields getLogs getFilename = MailboxProcessor.Start(fun inbox ->
    async  {
        while true do
            let! message = inbox.Receive()
            match message with
            | Report log -> report fields (getLogs log) (getFilename log)
            | Finish channel -> channel.Reply()
    })

type ReportAgent<'T>() =

    let fieldsElementLog = getFields [ typeof<RecordsLog>; typeof<'T> ]
    let fieldsSummaryLog = getFields [ typeof<SummaryLog> ]
    let fieldsJournalLog = getFields [ typeof<JournalLog> ]

    let getLogsElementLog elementLog : obj list = [ elementLog.RecordsLog; elementLog.MetricsLog ]
    let getLogsSummaryLog summaryLog : obj list = [ summaryLog ]
    let getLogsJournalLog journalLog : obj list = [ journalLog ]

    let getFilenameElementLog elementLog = filename<ElementLog<_>> + "-" + elementLog.RecordsLog.Ticker
    let getFilenameSummaryLog summaryLog = filename<SummaryLog>
    let getFilenameJournalLog journalLog = filename<JournalLog>

    let agentElementLog = createReportAgent fieldsElementLog getLogsElementLog getFilenameElementLog
    let agentSummaryLog = createReportAgent fieldsSummaryLog getLogsSummaryLog getFilenameSummaryLog
    let agentJournalLog = createReportAgent fieldsJournalLog getLogsJournalLog getFilenameJournalLog

    member this.ReportElementLog(elementLog : ElementLog<'T>) =
        agentElementLog.Post(Report elementLog)

    member this.ReportSummaryLog(summaryLog : SummaryLog) =
        agentSummaryLog.Post(Report summaryLog)

    member this.ReportJournalLog(journalLog : JournalLog) =
        agentJournalLog.Post(Report journalLog)

    member this.ReportCompletion() =
        agentElementLog.PostAndReply(Finish)
        agentSummaryLog.PostAndReply(Finish)
        agentJournalLog.PostAndReply(Finish)
