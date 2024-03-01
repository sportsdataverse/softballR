#' Get all NCAA softball team names and ids
#'
#' @author Tyson King
#' @description 2021-2023 for D1, only 2023 for D2 and D3
#' @param season 2021, 2022 2023, or 2024
#' @param division "D1", "D2", or "D3
#'
#' @return data frame of date, team names and their scores
#' @importFrom dplyr case_when filter select distinct
#' @importFrom rvest read_html html_table
#' @export
#'
#' @examples
#' try(ncaa_softball_teams(2024))
ncaa_softball_teams <- function(season, division = "D1"){

  team_ids <- "https://stats.ncaa.org/game_upload/team_codes" %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::filter(!(X1 %in% c("NCAA Codes", "ID"))) %>%
    `names<-`(c("team_id", "team_name"))

  possible_seasons <- c(2021, 2022, 2023, 2024)
  possible_divisions <- c("D1", "D2", "D3")

  if(!(season %in% possible_seasons &
       division %in% possible_divisions)) stop("Invalid Season")

  if(division %in% c("D2","D3") & season != 2023) stop("Invalid Season")

  team_info <- load_ncaa_softball_team_info()

  division_id <- dplyr::case_when(division == "D1" ~ "D-I",
                                  division == "D2" ~ "D-II",
                                  division == "D3" ~ "D-III")

  season_id <- season

  teams <- team_info %>%
    dplyr::filter(division == division_id &
                    season == season_id)

  scoreboard <- load_ncaa_softball_scoreboard(season = season,
                                              division = division)

  softball_ids <- rbind(scoreboard[c(1,2)] %>% `names<-`(c("team_name","softball_id")),
                        scoreboard[c(5,6)] %>% `names<-`(c("team_name","softball_id"))) %>%
    dplyr::distinct()

  teams_final <- merge(teams, team_ids %>% dplyr::select(-team_name), by = "team_id") %>%
    merge(softball_ids, by = "team_name") %>%
    dplyr::select(season, team_name, team_id, softball_id, division, conference,
                  head_coach, wins, losses, ties, win_perc)

}
