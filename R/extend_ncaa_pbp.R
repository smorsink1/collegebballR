
#' Add Team Info
#'
#' Adds team information to scraped play-by-play dataframe
#'
#' @param pbp_df the raw play-by-play dataframe, ie the output of getGamePbp
#'   (or scrapeSchoolSeason, scrapeConferenceSeason)
#'
#' @return returns a 17-column data frame, adding columns for year, away (visiting team),
#'   home (home team), away_conference (ie Pac-12), away_division (ie 1 for Division 1),
#'   home_conference, and home_division
#'
#' @importFrom dplyr left_join rename select
#' @importFrom magrittr %>%
#'
#' @examples
#' game_pbp <- getGamePbp("https://stats.ncaa.org/game/index/4502166?org_id=674")
#' addTeamInfo(game_pbp)
#' \dontrun{
#'   season_pbp <- scrapeSchoolSeason("Stanford", 2018)
#'   addteamInfo(season_pbp)
#' }
#'
addTeamInfo <- function(pbp_df) {
  pbp_df[["year"]] <- substr(pbp_df$date, nchar(pbp_df$date) - 3, nchar(pbp_df$date)) %>%
    as.integer()
  pbp_df[["away"]] <- pbp_df$batting[pbp_df$inning_top_bot == "top"][1]
  pbp_df[["home"]] <- pbp_df$batting[pbp_df$inning_top_bot == "bot"][1]
  away_school_info <- getSchoolMap() %>%
    dplyr::select(school, conference, division, year) %>%
    dplyr::rename("away" = "school", "away_conference" = "conference",
                  "away_division" = "division")
  home_school_info <- getSchoolMap() %>%
    dplyr::select(school, conference, division, year) %>%
    dplyr::rename("home" = "school", "home_conference" = "conference",
                  "home_division" = "division")
  pbp_df_upd <- pbp_df %>%
    dplyr::left_join(away_school_info, by = c("year", "away")) %>%
    dplyr::left_join(home_school_info, by = c("year", "home"))
  return (pbp_df_upd)
}

#' Add Pitch Info
#'
#' Adds pitch-level information to the scraped play-by-play dataframe
# addPitchInfo <- function(pbp_df) {
#
# }
