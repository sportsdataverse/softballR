#' Load NCAA Roster
#'
#' @description Supports 2016-2023 for all divisions
#' @param season YYYY format, pass season or vector of seasons 2021-2023
#' @param division "D1", "D2", or "D3"
#'
#' @return dataframe of all rosters from given season(s) and division
#' @export
#'
#' @examples try(load_ncaa_softball_rosters(2022:2023))
load_ncaa_softball_rosters <- function(season, division = "D1"){

  if(!is.numeric(season)) return("Invalid Input")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if(min(season) < 2021 | max(season) > 2023) stop("Invalid Season")

  url <- "https://github.com/sportsdataverse/softballR-data/raw/main/data/ncaa_rosters_2021_2023.RDS"

  con <- url(url)

  on.exit(close(con))

  rosters <- try(readRDS(con), silent = TRUE) %>%
    dplyr::filter(season %in% .env$season & division == .env$division)

  return(rosters)

}
