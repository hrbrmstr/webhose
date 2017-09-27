#' Make Column Names Great Again
#'
#' Remove punctuation and spaces and turn them to underscores plus convert to lower case.
#'
#' @md
#' @param tbl a `data.frame`-like object
#' @return whatver class `x` was but with truly great, really great column names.
#'     They're amazing. Trust me. They'll be incredible column names once we're done.
#' @export
mcga <- function(tbl) {

  x <- colnames(tbl)
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- make.unique(x, sep = "_")

  colnames(tbl) <- x

  tbl

}