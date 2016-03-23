select
    [Date]  as [Date],
    [Open]  as [Open],
    [High]  as [Hi],
    [Low]   as [Lo],
    [Close] as [Close]
from
    [Quote] quote inner join [Issue] issue on quote.IssueId = issue.IssueId
where
    issue.ticker = @ticker
order by
    quote.[Date] asc
