#' Fetch all structured reviews data from hundreds of review sites
#'
#' @md
#' @param query A string query containing the filters that define which reviews will be returned.
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
#' @param accuracy_confidence `NULL` or `high`. If `high`, return only posts with high
#'        extraction accuracy, but removes about 30% of the total matching posts (with
#'        lower confidence).
#' @param highlight `FALSE` or `TRUE`. Return the fragments in the post that matched the
#'        textual boolean query. The matched keywords will be surrounded by `<em/>` tags.
#'        Default: `FALSE`
#' @param pre_alloc_max If you know the approximate number of API calls that will need
#'        to be made to retrieve a full set of records for a query, use that number
#'        here (defaults to `30`, which is enough space to hold 3,000 total records if
#'        each API call returns 100). This helps pre-allocate space and avoid copying
#'        interim buffers as the result set expands.
#' @param quiet By default, calls in interactive sessions will return fetch status updates.
#'        Use `TRUE` to suppress these messages.
#' @param token Your private access token. You get a unique access token when you sign up.
#'        Store it in an environment variable `WEBHOSE_TOKEN` (usually in `~/.Renviron`)
#'        or provide it directly.
#' @param ... other parameters passed on (eventually) to `httr::GET()`
#' @references [webhose API](https://docs.webhose.io/docs/get-parameters)
#' @export
#' @examples \dontrun{
#' res <- fetch_reviews("site_category:travel rating:<3 (bug OR roach OR cockroach)")
#' }
fetch_reviews <- function(query,
                          sort = "relevancy",
                          ts = (Sys.time() - (3 * 24 * 60 * 60)),
                          order = "desc",
                          accuracy_confidence = NULL,
                          highlight = FALSE,
                          pre_alloc_max = 30,
                          quiet = !interactive(),
                          token = Sys.getenv("WEBHOSE_TOKEN"),
                          ...) {

  results <- vector(mode = "list", length = pre_alloc_max)
  res <- NULL

  i <- 1
  from <- 0
  repeat {
    res <- filter_reviews(query=query,
                          sort=sort,
                          ts=ts,
                          order=order,
                          size=100,
                          accuracy_confidence=accuracy_confidence,
                          highlight=highlight,
                          from=from,
                          token=token,
                          quiet=TRUE,
                          ...)

    results[[i]] <- res

    if (res[["moreResultsAvailable"]] > 0) {

      if (!quiet) message("Fetching next 100 records...")
      i <- i + 1
      from <- from + 100

    } else {

      break

    }
  }

  if (!quiet) message(
    sprintf(
      "You have %s API calls remaining on your plan",
      comma(res$requestsLeft)
    )
  )

  purrr::discard(results, is.null) %>%
    purrr::map_df(~{ .x$reviews }) %>%
    tibble::as_tibble() %>%
    mcga()

}