#' Get all NCAA Softball rosters for a given seaosn and division
#'
#' @param season  2021-2023
#' @param division "D1", "D2", or "D3
#' @importFrom dplyr filter arrange select mutate
#' @importFrom glue glue
#' @importFrom rvest read_html html_table
#' @importFrom magrittr extract2
#' @importFrom janitor row_to_names clean_names
#'
#' @return dataframe of every player on every team in a given season/division
#' @export
#'
#' @examples try(ncaa_softball_rosters(2023, "D1"))
ncaa_softball_rosters <- function(season, division){

  if(!(season %in% c(2021, 2022, 2023))) stop("Invalid season")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid division")

  division_id <- dplyr::case_when(division == "D1" ~ "D-I",
                           division == "D2" ~ "D-II",
                           division == "D3" ~ "D-III")

  roster_ext <- dplyr::case_when(season == 2023 ~ "16361",
                                 season == 2022 ~ "15940",
                                 season == 2021 ~ "15620")

  teams <- load_ncaa_softball_team_info() %>%
    dplyr::filter(season == .env$season & division == division_id) %>%
    dplyr::arrange(team_name)

  get_roster <- function(team_id, roster_ext){

    url <- glue::glue("https://stats.ncaa.org/team/{team_id}/roster/{roster_ext}")

    roster <- url %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      magrittr::extract2(1) %>%
      janitor::row_to_names(1) %>%
      janitor::clean_names() %>%
      dplyr::select(jersey, player, pos, yr) %>%
      dplyr::mutate(team_id, division, season)

    return(roster)

  }

  rosters <- data.frame()

  for(i in 1:nrow(teams)){

    rosters <- rbind(rosters, get_roster(teams$team_id[i], roster_ext))

    print(teams$team_name[i])

  }

  return(rosters)

}
