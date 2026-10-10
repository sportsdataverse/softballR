#' Get all NCAA Softball rosters for a given seaosn and division
#'
#' @param season  2012 onward (every season with a stats.ncaa.org roster id, see
#'   `softballR:::.ncaa_softball_roster_ids`)
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

  season <- as.numeric(season)
  valid <- as.numeric(names(.ncaa_softball_roster_ids))
  if (length(season) != 1 || is.na(season) || !season %in% valid) {
    stop(sprintf("Invalid season: %s. Valid seasons are %d-%d.", paste(season, collapse = ", "), min(valid), max(valid)), call. = FALSE)
  }

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid division")

  division_id <- dplyr::case_when(division == "D1" ~ "D-I",
                           division == "D2" ~ "D-II",
                           division == "D3" ~ "D-III")

  roster_ext <- .ncaa_softball_roster_ids[[as.character(season)]]

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

# stats.ncaa.org softball season ids (`sport_year_ctl_id`), the `{id}` in
# https://stats.ncaa.org/team/{org_id}/roster/{id}. 2012-2023 are the original
# table; 2024-2026 were read on 2026-10-09 from the `sport_year_ctl_id=` links
# on the Texas (org_id 703) team pages and confirmed to resolve to the 2023-24,
# 2024-25 and 2025-26 Softball rosters. A new season is one new row; the
# season check derives its range from this table
# (rules/college_softball.yaml#college_softball-2012-softballr-roster-season-ids).
.ncaa_softball_roster_ids <- c(
  "2012" = "10900",
  "2013" = "11300",
  "2014" = "11640",
  "2015" = "12120",
  "2016" = "12381",
  "2017" = "12580",
  "2018" = "13430",
  "2019" = "14800",
  "2020" = "15220",
  "2021" = "15620",
  "2022" = "15940",
  "2023" = "16361",
  "2024" = "16540",
  "2025" = "16800",
  "2026" = "17020"
)
