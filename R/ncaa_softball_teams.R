#' Get all NCAA softball team names and ids
#'
#' @author Tyson King
#' @description Only has division 1 teams for now
#' @param season 2021, 2022 or 2023
#'
#' @return data frame of date, team names and their scores
#' @importFrom dplyr case_when filter select mutate
#' @importFrom glue glue
#' @importFrom stringr str_remove_all
#' @importFrom magrittr extract extract2
#' @importFrom rvest read_html html_table
#' @export
#'
#' @examples
#' try(ncaa_softball_teams(2023))
ncaa_softball_teams <- function(season){

  possible_seasons <- c(2021, 2022, 2023)

  if(!(season %in% possible_seasons)) stop("Invalid Season")

  url_ext <- dplyr::case_when(season == 2021 ~ 21123,
                              season == 2022 ~ 27203,
                              season == 2023 ~ 30768)

  team_ids <- "https://stats.ncaa.org/game_upload/team_codes" %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::filter(!(X1 %in% c("NCAA Codes", "ID"))) %>%
    `names<-`(c("team_id", "team_name"))

  raw_softball <- glue::glue("https://stats.ncaa.org/selection_rankings/nitty_gritties/{url_ext}") %>%
    readLines()

  locs <- grep("<a target=\"TEAM_PAGE\"", raw_softball)

  teams_raw <- raw_softball[locs]
  teams <- data.frame()

  for(i in 1:length(teams_raw)){

    split <- strsplit(teams_raw[i], "href=\"/teams/|\"") %>%
      magrittr::extract2(1)

    softball_id <- split %>%
      magrittr::extract(14)

    team_name <- split %>%
      magrittr::extract(15) %>%
      stringr::str_remove_all(">|</a></td>|&#39|&amp;")

    teams <- rbind(teams, data.frame(team_name, softball_id))

  }

  teams <- merge(teams, team_ids, by = "team_name") %>%
    dplyr::mutate(season = season)

  team_info <- load_ncaa_softball_team_info()

  teams_final <- merge(teams, team_info, by = c("team_id","season")) %>%
    dplyr::select(team_name, team_id, softball_id, division, conference, head_coach, wins, losses, ties, win_perc)

  return(teams_final)

}
