
#' Get School Map
#'
#' Obtain table mapping school names to conferences,
#'   school_id, year, division, conference_id
#'
#' @return returns a 6-column data frame with columns for school, conference, school_id,
#'   year, division, and conference_id
#'
#' @importFrom utils data
#' @importFrom baseballr school_id_lu
#'
#' @examples
#' getSchoolMap()
#'
#' @export
getSchoolMap <- function() {
  utils::data("school_map", envir = environment())
  return (school_map)
}

#' Scrape Game Play-by-Play
#'
#' Given a game_info_url, returns the play-by-play for that game
#'
#' @param game_info_url the stats.ncaa.org page with the game play-by-play,
#'   accessible via baseballr::get_ncaa_schedule_info function
#'
#' @return returns a 10-column data frame with columns for date, location, attendance,
#'   inning, inning_top_bot, score, batting, fielding, description, game_info_url
#'
#' @importFrom baseballr get_ncaa_baseball_pbp
#'
#' @examples
#' getGamePbp("https://stats.ncaa.org/game/index/4502166?org_id=674")
#'
#' @export
getGamePbp <- function(game_info_url) {
  game_pbp <- baseballr::get_ncaa_baseball_pbp(game_info_url)
  game_pbp[["game_info_url"]] <- game_info_url
  return (game_pbp)
}

#' Get School Season URLs
#'
#' Obtain game_info_url values for all games in a school's season
#'
#' @param school_name the name of the school
#'   (must have an exact match in school column of getSchoolMap())
#' @param season_year the year of the season to acquire data for
#'   (must have an exact match in the year column of getSchoolMap())
#'
#' @return returns a character vector of URLs for each game
#'
#' @importFrom baseballr get_ncaa_schedule_info
#'
#' @examples
#' getSchoolSeasonURLs("Stanford", 2018)
#'
#' @export
#'
getSchoolSeasonURLs <- function(school_name, season_year) {
  school_map <- getSchoolMap()
  school_matches <- which(school_map$school == school_name)
  year_matches <- which(school_map$year == season_year)
  school_map_row <- school_map[intersect(school_matches, year_matches), ]
  if (nrow(school_map_row) < 1) {
    stop ("no records match that school_name / season_year combination")
  } else if (nrow(school_map_row) > 1) {
    school_map_row <- school_map_row[1, ]
  }
  season_info <- baseballr::get_ncaa_schedule_info(school_map_row$school_id, school_map_row$year)
  season_urls <- season_info$game_info_url
  return (season_urls)
}

#' Scrape School Season
#'
#' Obtain play-by-play data for all games in a school's season
#'
#' @param school_name the name of the school
#'   (must have an exact match in school column of getSchoolMap())
#' @param season_year the year of the season to acquire data for
#'   (must have an exact match in the year column of getSchoolMap())
#'
#' @return returns a 10-column data frame with columns for date, location, attendance,
#'   inning, inning_top_bot, score, batting, fielding, description, and game_info_url
#'
#' @importFrom baseballr get_ncaa_schedule_info
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{scrapeSchoolSeason("Stanford", 2018)}
#'
#' @export
#'
scrapeSchoolSeason <- function(school_name, season_year) {
  season_urls <- getSchoolSeasonURLs(school_name, season_year)
  season_pbp <- purrr::map_dfr(season_urls, getGamePbp)
  return (season_pbp)
}

#' Scrape Conference Season
#'
#' Obtain play-by-play data for all games in a season played by all teams in a given conference
#'
#' @param conference_name the name of the conference
#'   (must have an exact match in conference column of getSchoolMap())
#' @param season_year the year of the season to acquire data for
#'   (must have an exact match in the year column of getSchoolMap())
#'
#' @return returns a 10-column data frame with columns for date, location, attendance,
#'   inning, inning_top_bot, score, batting, fielding, description, and game_info_url
#'
#' @importFrom baseballr get_ncaa_schedule_info
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{scrapeConferenceSeason("Pac-12", 2018)}
#'
#' @export
#'
scrapeConferenceSeason <- function(conference_name, season_year) {
  school_map <- getSchoolMap()
  conf_matches <- which(school_map$conference == conference_name)
  year_matches <- which(school_map$year == season_year)
  school_map_rows <- school_map[intersect(conf_matches, year_matches), ]
  if (nrow(school_map_rows) < 1) {
    stop ("no records match that conference_name / season_year combination")
  }
  all_urls <- unlist(purrr::map(school_map_rows$school, getSchoolSeasonURLs, season_year = season_year))
  conf_urls <- all_urls[!duplicated(all_urls)]
  conf_pbp <- purrr::map_dfr(conf_urls, getGamePbp)
  return (conf_pbp)
}

