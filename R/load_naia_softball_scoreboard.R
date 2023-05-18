#' Load NAIA Scoreboard
#'
#' @description Only supports 2023 season for now
#' @param season YYYY
#'
#' @return dataframe of all games from NAIA website from selected season
#' @importFrom glue glue
#' @export
#'
#' @examples try(load_naia_softball_scoreboard(2023))
load_naia_softball_scoreboard <- function(season){

  if(!is.numeric(season)) return("Invalid Input")

  if(season != 2023) stop("Invalid Season")

  url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/naia_scoreboard_{season}.RDS?raw=true")

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

  return(scoreboard)

}
