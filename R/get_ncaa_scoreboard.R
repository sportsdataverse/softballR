#' Get NCAA Softball scoreboard for a given day
#'
#' @author Tyson King
#' @param date "YYYY-MM-DD"
#'
#' @return data frame of team names, their scores, and the game ids
#' @importFrom lubridate mday month year
#' @importFrom stringr str_remove_all
#' @export
#'
#' @examples
#' date = "2022-03-05"
#' try(get_ncaa_scoreboard(date))
get_ncaa_scoreboard <- function(date){
  teamname_ref <- "                <span class=\"gamePod-game-team-name\">"

  date = as.Date(date)

  day = lubridate::mday(date)
  day = ifelse(day <= 9,paste0("0",day),day)

  month = lubridate::month(date)
  month = ifelse(month <= 9,paste0("0",month))

  year = lubridate::year(date)

  url <- paste0("https://www.ncaa.com/scoreboard/softball/d1/",year,"/",month,"/",day,"/all-conf")

  raw_html <- url %>% readLines()

  locations <- grep(teamname_ref,raw_html)
  locations <- locations %>% split(ceiling(seq_along(locations) / 2)) %>% as.data.frame()

  games <- as.data.frame(matrix(nrow = 0,ncol = 5))
  names(games) <- c("home_name","home_score","away_name","away_score","game_id")

  for(i in 1:ncol(locations)){
    team1 <- stringr::str_remove_all(raw_html[locations[1,i]],c("                <span class=\"gamePod-game-team-name\">|</span>"))
    team2 <- stringr::str_remove_all(raw_html[locations[2,i]],c("                <span class=\"gamePod-game-team-name\">|</span>"))

    team1_score <- stringr::str_remove_all(raw_html[locations[1,i] + 1],c("                <span class=\"gamePod-game-team-score\">|</span>"))
    team2_score <- stringr::str_remove_all(raw_html[locations[2,i] + 1],c("                <span class=\"gamePod-game-team-score\">|</span>"))

    game_id <- stringr::str_remove_all(raw_html[locations[1,i] - 20],"        href=\"/game/|\">")
    if(str_detect(game_id,"SEED")) game_id <- stringr::str_remove_all(raw_html[locations[1,i] - 21],"        href=\"/game/|\">")


    temp <- data.frame(home_name = team2,
                       home_score = team2_score,
                       away_name = team1,
                       away_score = team1_score,
                       game_id = game_id)

    games <- rbind(games,temp)
  }

  return(games)

}
