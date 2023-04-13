#' Load NCAA Scoreboard
#'
#' @param season season (YYYY)
#' @description Has data for 2019 - 2023
#'
#' @return dataframe of all games from NCAA website from selected season
#' @importFrom glue glue
#' @export
#'
#' @examples try(load_ncaa_scoreboard(2023))
load_ncaa_scoreboard <- function(season){

  if(!is.numeric(season)) return("Invalid Input")

  url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season}.RDS?raw=true")

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

}
