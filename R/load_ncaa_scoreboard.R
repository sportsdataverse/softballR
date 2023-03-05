#' Load NCAA Scoreboard
#'
#' @param season season (YYYY)
#' @description ONLY HAS 2023 DATA FOR NOW
#'
#' @return dataframe of all games from NCAA website from selected season
#' @export
#'
#' @examples try(load_ncaa_scoreboard(2023))
load_ncaa_scoreboard <- function(season){

  if(!is.numeric(season)) return("Invalid Input")

  url <- "https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_2023.RDS?raw=true"

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

}
