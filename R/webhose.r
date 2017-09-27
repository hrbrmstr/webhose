#' Retrieve structured posts data from news articles, blog posts and online discussions
#'
#' @md
#' @param query A string query containing the filters that define which posts will be returned.
#' @param sort By default the results are sorted by relevancy. Acceptable values are
#'        "`relevancy`", "`social.facebook.likes`", "`social.facebook.shares`",
#'        "`social.facebook.comments`", "`social.gplus.shares`", "`social.pinterest.shares`",
#'        "`social.linkedin.shares`", "`social.stumbledupon.shares`", "`social.vk.shares`",
#'        "`replies_count`", "`participants_count`", "`spam_score`", "`performance_score`",
#'        "`published`", "`thread.published`", "`domain_rank`", "`ord_in_thread`",
#'        "`rating`".
#' @param ts A timestamp to start the search from. If a `POSIXct` is passed in, it will
#'        be converted to the necessary value in milliseconds. Default is previous 3 days.
#' @param order `asc` (ascending) or `desc` (descending, default) sort order for results
#' @param size Total number of posts returned per request, ranges between `1:100`
#'        (default is `100`).
#' @param accuracy_confidence `NULL` or `high`. If `high`, return only posts with high
#'        extraction accuracy, but removes about 30% of the total matching posts (with
#'        lower confidence).
#' @param highlight `FALSE` or `TRUE`. Return the fragments in the post that matched the
#'        textual boolean query. The matched keywords will be surrounded by `<em/>` tags.
#'        Default: `FALSE`
#' @param from Paging parameter (starting record number). Default is `0`.
#' @param quiet By default, calls in interactive sessions will return updates during fetching.
#'        Use `TRUE` to suppress these messages.
#' @param token Your private access token. You get a unique access token when you sign up.
#'        Store it in an environment variable `WEBHOSE_TOKEN` (usually in `~/.Renviron`)
#'        or provide it directly.
#' @param ... other parameters passed on to `httr::GET()`
#' @return a `list` with these fields:
#' * `totalResults`: The total number of posts matching your query (numeric)
#' * `moreResultsAvailable`: How many more results are available (numeric)
#' * `next`: A URL to get the next batch of posts matching your query. (character)
#' * `requestsLeft`: How many more requests are available in your current subscription plan. (numeric)
#' @references [webhose API](https://docs.webhose.io/docs/get-parameters)
#' @export
#' @examples \dontrun{
#' res <- filter_web_content("(China AND United) language:english site_type:news site:bloomberg.com",
#'                             ts = 1213456)
#' }
filter_web_content <- function(query, sort = "relevancy",
                               ts = (Sys.time() - (3 * 24 * 60 * 60)),
                               order = "desc", size = 100,
                               accuracy_confidence = NULL, highlight = FALSE,
                               from = 0, quiet = !interactive(),
                               token = Sys.getenv("WEBHOSE_TOKEN"), ...) {

  if (inherits(ts, "POSIXct")) ts <- as.numeric(ts)

  sort <- match.arg(tolower(sort[1]), sort_params)
  order <- match.arg(tolower(order[1]), c("asc", "desc"))

  params <- list(
    token = token,
    format = "json",
    q = query,
    sort = sort,
    order = order,
    size = size,
    ts = ts,
    from = from,
    highlight = highlight
  )

  if (!is.null(accuracy_confidence)) {
    accuracy_confidence <- match.arg(accuracy_confidence, "high")
    params$accuracy_confidence = accuracy_confidence
  }

  httr::GET(
    url = "https://webhose.io/filterWebContent",
    query = params,
    ...
  ) -> res

  httr::stop_for_status(res)

  res <- httr::content(res, as="text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(res, flatten=TRUE)

  if (!quiet) message(sprintf("You have %s API calls remaining on your plan",
                              comma(res$requestsLeft)))

  res

}
