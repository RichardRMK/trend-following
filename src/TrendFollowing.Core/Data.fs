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

[<Literal>]
let private sqlGetQuotes = @"..\..\sql\GetQuotes.sql"

type private GetQuotesCommandProvider = SqlCommandProvider<sqlGetQuotes, connectionName, ConfigFile = configFile>

let private toQuote (record : GetQuotesCommandProvider.Record) : Quote =

    { Date     = record.Date
      Ticker   = record.Ticker
      Hi       = record.Hi
      Lo       = record.Lo
      Close    = record.Close
      Dividend = record.Dividend
      SplitNew = record.SplitNew |> Option.map uint32
      SplitOld = record.SplitOld |> Option.map uint32 }

let getQuotes date =
    use command = new GetQuotesCommandProvider()
    let records = command.Execute(date)
    records
    |> Seq.map toQuote
    |> Seq.toArray

//-------------------------------------------------------------------------------------------------

[<Literal>]
let private sqlGetHolidays = @"..\..\sql\GetHolidays.sql"

type private GetHolidaysCommandProvider = SqlCommandProvider<sqlGetHolidays, connectionName, ConfigFile = configFile>

let getHolidays dateStart dateFinal =
    use command = new GetHolidaysCommandProvider()
    let records = command.Execute(dateStart, dateFinal)
    records
    |> Seq.toArray

//-------------------------------------------------------------------------------------------------

let getDates dateStart dateFinal =

    let holidays = getHolidays dateStart dateFinal

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
