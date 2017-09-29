#' Tools to Work with the 'webhose.io' 'API'
#'
#' The 'webhose.io' <https://webhose.io/about> 'API' provides access to structured
#' web data feeds across vertical content domains. Their crawlers download the web,
#' structure the data and index save it into domain-specific repositories that can be
#' accessed on demand. Methods are provided to query and retrieve content from this 'API'.
#'
#' @name webhose
#' @docType package
#' @author Bob Rudis (bob@@rud.is)
#' @author Philipp Ottolinger (philipp@@ottolinger.de)
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df discard %>%
#' @importFrom tibble as_tibble
NULL
