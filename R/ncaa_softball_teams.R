#' Get all NCAA softball team names and ids
#'
#' @author Tyson King
#' @description Only has division 1 teams for now
#' @param season 2021, 2022 or 2023
#'
#' @return data frame of date, team names and their scores
#' @importFrom dplyr case_when
#' @importFrom glue glue
#' @importFrom stringr str_remove_all
#' @importFrom magrittr extract extract2
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

  raw <- glue::glue("https://stats.ncaa.org/selection_rankings/nitty_gritties/{url_ext}") %>%
    readLines()

  locs <- grep("<a target=\"TEAM_PAGE\"", raw)

  teams_raw <- raw[locs]
  teams <- data.frame()

  for(i in 1:length(teams_raw)){

    split <- strsplit(teams_raw[i], "href=\"/teams/|\"") %>%
      magrittr::extract2(1)

    team_id <- split %>%
      magrittr::extract(14)

    team_name <- split %>%
      magrittr::extract(15) %>%
      stringr::str_remove_all(">|</a></td>|&#39|&amp;")

    teams <- rbind(teams, data.frame(team_name, team_id))

  }

  return(teams)

}
