#' Load NCAA Scoreboard
#'
#' @description Supports 2019-2023 for D1 and only 2023 for D2 and D3
#' @param season YYYY
#' @param division "D1", "D2", or "D3"
#'
#' @return dataframe of all games from NCAA website from selected season
#' @importFrom glue glue
#' @export
#'
#' @examples try(load_ncaa_softball_scoreboard(2023))
load_ncaa_softball_scoreboard <- function(season, division = "D1"){

  if(!is.numeric(season)) return("Invalid Input")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if((division != "D1" & season != 2023)) stop("Invalid Date")

  if(division == "D1"){

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season}.RDS?raw=true")

  } else{

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{division}_{season}.RDS?raw=true")

  }

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

}