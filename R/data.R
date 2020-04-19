
#' School Map
#'
#' A dataset providing mappings between school name, school conference, school_id,
#'   year, division, and conference_id
#'
#' @name school_map
#' @docType data
#'
#' @format A tibble with 7291 rows and 6 columns:
#' \describe{
#'   \item{school}{name of the school}
#'   \item{conference}{name of the conference}
#'   \item{school_id}{NCAA id (integer) for the school}
#'   \item{year}{season year (integer)}
#'   \item{division}{Division 1, 2, or 3 (integer)}
#'   \item{conference_id}{NCAA id (integer) for the conference}
#' }
#' @source baseballr::school_id_lu('')
NULL
