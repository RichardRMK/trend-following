select
    quote.[Date]   as [Date],
    issue.[Ticker] as [Ticker],
    quote.[High]   as [Hi],
    quote.[Low]    as [Lo],
    quote.[Close]  as [Close],
    divid.[Amount] as [Dividend],
    split.[New]    as [SplitNew],
    split.[Old]    as [SplitOld]
from
    [Quote] quote
    left  join [Divid] divid on quote.[IssueId] = divid.[IssueId] and quote.[Date] = divid.[Date]
    left  join [Split] split on quote.[IssueId] = split.[IssueId] and quote.[Date] = split.[Date]
    inner join [Issue] issue on quote.[IssueId] = issue.[IssueId]
where
    quote.[Date] between @dateStart and @dateFinal
    and issue.[Ticker] in (@ticker)
order by
    issue.[Ticker] asc
