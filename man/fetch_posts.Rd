% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_posts.R
\name{fetch_posts}
\alias{fetch_posts}
\title{Fetch all structured posts data from news articles, blog posts and online discussions}
\usage{
fetch_posts(query, sort = "relevancy", ts = (Sys.time() - (3 * 24 * 60 *
  60)), order = "desc", accuracy_confidence = NULL, highlight = FALSE,
  pre_alloc_max = 30, quiet = !interactive(),
  token = Sys.getenv("WEBHOSE_TOKEN"), ...)
}
\arguments{
\item{query}{A string query containing the filters that define which posts will be returned.}

\item{sort}{By default the results are sorted by relevancy. Acceptable values are
"\code{relevancy}", "\code{social.facebook.likes}", "\code{social.facebook.shares}",
"\code{social.facebook.comments}", "\code{social.gplus.shares}", "\code{social.pinterest.shares}",
"\code{social.linkedin.shares}", "\code{social.stumbledupon.shares}", "\code{social.vk.shares}",
"\code{replies_count}", "\code{participants_count}", "\code{spam_score}", "\code{performance_score}",
"\code{published}", "\code{thread.published}", "\code{domain_rank}", "\code{ord_in_thread}",
"\code{rating}".}

\item{ts}{A timestamp to start the search from. If a \code{POSIXct} is passed in, it will
be converted to the necessary value in milliseconds. Default is previous 3 days.}

\item{order}{\code{asc} (ascending) or \code{desc} (descending, default) sort order for results}

\item{accuracy_confidence}{\code{NULL} or \code{high}. If \code{high}, return only posts with high
extraction accuracy, but removes about 30% of the total matching posts (with
lower confidence).}

\item{highlight}{\code{FALSE} or \code{TRUE}. Return the fragments in the post that matched the
textual boolean query. The matched keywords will be surrounded by \code{<em/>} tags.
Default: \code{FALSE}}

\item{pre_alloc_max}{If you know the approximate number of API calls that will need
to be made to retrieve a full set of records for a query, use that number
here (defaults to \code{30}, which is enough space to hold 3,000 total records if
each API call returns 100). This helps pre-allocate space and avoid copying
interim buffers as the result set expands.}

\item{quiet}{By default, calls in interactive sessions will return fetch status updates.
Use \code{TRUE} to suppress these messages.}

\item{token}{Your private access token. You get a unique access token when you sign up.
Store it in an environment variable \code{WEBHOSE_TOKEN} (usually in \code{~/.Renviron})
or provide it directly.}

\item{...}{other parameters passed on (eventually) to \code{httr::GET()}}
}
\description{
Fetch all structured posts data from news articles, blog posts and online discussions
}
\examples{
\dontrun{
res <- fetch_posts("(China AND United) language:english site_type:news site:bloomberg.com",
                            ts = 1213456)
}
}
\references{
\href{https://docs.webhose.io/docs/get-parameters}{webhose API}
}
