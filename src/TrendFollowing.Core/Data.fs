module TrendFollowing.Data

open System
open FSharp.Data
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

[<Literal>]
let private connectionName = @"name=database"

[<Literal>]
let private configFile = @"..\..\private\App.config"

//-------------------------------------------------------------------------------------------------

module private GetQuotes =

    [<Literal>]
    let private sql = @"..\..\sql\GetQuotes.sql"

    type private CommandProvider = SqlCommandProvider<sql, connectionName, ConfigFile = configFile>

    let private ofRecord (record : CommandProvider.Record) : Quote =

        { Date     = record.Date
          Ticker   = record.Ticker
          Hi       = record.Hi
          Lo       = record.Lo
          Close    = record.Close
          Dividend = record.Dividend
          SplitNew = record.SplitNew |> Option.map uint32
          SplitOld = record.SplitOld |> Option.map uint32 }

    let execute dateStart dateFinal =
        use command = new CommandProvider()
        let records = command.Execute(dateStart, dateFinal)
        records
        |> Seq.map ofRecord
        |> Seq.toArray

let private getQuoteLookup quotes date =
    quotes
    |> Map.tryFind date
    |> Option.fold (fun _ -> id) Array.empty

let getQuotes dateStart dateFinal =
    GetQuotes.execute dateStart dateFinal
    |> Array.groupBy (fun quote -> quote.Date)
    |> Map.ofArray
    |> getQuoteLookup

//-------------------------------------------------------------------------------------------------

module private GetHolidays =

    [<Literal>]
    let private sql = @"..\..\sql\GetHolidays.sql"

    type private CommandProvider = SqlCommandProvider<sql, connectionName, ConfigFile = configFile>

    let execute dateStart dateFinal =
        use command = new CommandProvider()
        let records = command.Execute(dateStart, dateFinal)
        records
        |> Seq.toArray

let getDates dateStart dateFinal =

    let holidays = GetHolidays.execute dateStart dateFinal

    let generator = function
        | date when date > dateFinal -> None
        | date -> Some (date, date + TimeSpan.FromDays(1.0))

    let isWeekend (date : DateTime) =
        match date.DayOfWeek with
        | DayOfWeek.Saturday -> true
        | DayOfWeek.Sunday   -> true
        | _ -> false

    let isHoliday (date : DateTime) =
        holidays |> Array.contains date

    dateStart
    |> Seq.unfold generator
    |> Seq.filter (not << isWeekend)
    |> Seq.filter (not << isHoliday)
