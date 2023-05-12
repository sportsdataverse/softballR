#' Load NCAA Scoreboard
#'
#' @description Supports 2016-2023 for all divisions
#' @param season YYYY format, pass season or vector of seasons
#' @param division "D1", "D2", or "D3"
#'
#' @return dataframe of all games from NCAA website from selected season
#' @importFrom glue glue
#' @export
#'
#' @examples try(load_ncaa_softball_scoreboard(2022:2023))
load_ncaa_softball_scoreboard <- function(season, division = "D1"){

  if(!is.numeric(season)) return("Invalid Input")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if(min(season) < 2016 | max(season) > 2023) stop("Invalid Season")

  if(length(season) == 1){

    if(division == "D1"){

      url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season}.RDS?raw=true")

    } else{

      url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{division}_{season}.RDS?raw=true")

    }

    con <- url(url)

    on.exit(close(con))

    scoreboard <- try(readRDS(con), silent = TRUE)

  } else{

    scoreboard <- data.frame()

    for(i in 1:length(season)){

      if(division == "D1"){

        url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season[i]}.RDS?raw=true")

      } else{

        url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{division}_{season[i]}.RDS?raw=true")

      }

      con <- url(url)

      on.exit(close(con))

      scoreboard <- rbind(scoreboard, try(readRDS(con), silent = TRUE))
    }

  }

  return(scoreboard)

}
