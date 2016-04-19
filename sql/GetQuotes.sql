select
    [Date]   as [Date],
    [Ticker] as [Ticker],
    [High]   as [Hi],
    [Low]    as [Lo],
    [Close]  as [Close]
from
    [Quote] quote inner join [Issue] issue on quote.IssueId = issue.IssueId
where
    quote.[Date] = @date
    and issue.Ticker in ('BRK.A')
order by
    issue.Ticker asc
