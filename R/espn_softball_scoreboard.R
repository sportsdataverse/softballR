#' Get ESPN Softball Scoreboard
#'
#' @author Tyson King
#' @param date YYYY-MM-DD
#'
#' @return data frame with 21 columns of relevant game information
#' @importFrom stringr str_remove_all str_split str_replace str_remove
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' try(espn_softball_scoreboard("2022-03-05"))
espn_softball_scoreboard <- function(date){
  options(warn = -1)

  d <- try(as.Date(date))

  if("try-error" %in% class(d) || is.na(d)){
    stop("Invalid Date")
  }

  date_fmtd <- paste0(substr(date,1,4),substr(date,6,7),substr(date,9,10))

  url <- paste0("https://www.espn.com/college-softball/scoreboard/_/date/",date_fmtd)

  raw_html <- url %>% readLines()

  location <- grep("\"CSOFT\"",raw_html)

  string <- raw_html[location] %>%
    stringr::str_split("\"name\":\"NCAA Softball\",\"abbrev\":\"CSOFT\"")

  string <- string[[1]][2]

  bygame <- string %>% stringr::str_split("competitors")
  bygame <- bygame[[1]]

  if(length(bygame) == 1){
    stop("No games on this date")
  }

  scoreboard <- data.frame(home_team_abbreviation = NA, home_team_display_name = NA, home_team_mascot = NA,
                           home_team_logo = NA, home_team_color = NA, home_team_runs = NA, home_team_hits = NA, home_team_errors = NA,
                           away_team_abbreviation = NA, away_team_display_name = NA, away_team_mascot = NA,
                           away_team_logo = NA, away_team_color = NA, away_team_runs = NA, away_team_hits = NA, away_team_errors = NA,
                           description = NA, game_date = NA, game_time = NA, game_id = NA)

  for (i in 2:length(bygame)) {
    current <- bygame[i]

    split <- stringr::str_split(current,":")[[1]]

    abbreviation_locator_1 <- grep("abbrev\"",split)[1] + 1
    team1_abbreviation <- stringr::str_remove_all(split[abbreviation_locator_1],",|displayName|\"")

    display_name_locator_1 <- grep("displayName",split)[1] + 1
    team1_display_name <- stringr::str_remove_all(split[display_name_locator_1],",|shortDisplayName|\"")

    mascot_locator_1 <- grep("shortDisplayName",split)[1] + 1
    team1_mascot <- stringr::str_remove_all(split[mascot_locator_1],",|logo|\"")

    logo_locator_1 <- grep("logo",split)[1] + 1
    team1_logo <- stringr::str_replace(stringr::str_remove_all(paste(split[logo_locator_1],split[logo_locator_1 + 1]),",|teamColor|\"")," ",":")

    color_locator_1 <- grep("teamColor",split)[1] + 1
    team1_color <- stringr::str_remove_all(split[color_locator_1],",|uid|\"")

    ishome_locator_1 <- grep("isHome",split)[1] + 1
    team1_ishome <- stringr::str_remove_all(split[ishome_locator_1],",|score|\"")

    runs_locator_1 <- grep("runs",split)[1] + 1
    team1_runs <- stringr::str_remove_all(split[runs_locator_1],",|statistics|\"")

    abbreviation_locator_2 <- grep("abbrev\"",split)[2] + 1
    team2_abbreviation <- stringr::str_remove_all(split[abbreviation_locator_2],",|displayName|\"")

    display_name_locator_2 <- grep("displayName",split)[2] + 1
    team2_display_name <- stringr::str_remove_all(split[display_name_locator_2],",|shortDisplayName|\"")

    mascot_locator_2 <- grep("shortDisplayName",split)[2] + 1
    team2_mascot <- stringr::str_remove_all(split[mascot_locator_2],",|logo|\"")

    logo_locator_2 <- grep("logo",split)[3] + 1
    team2_logo <- stringr::str_replace(stringr::str_remove_all(paste(split[logo_locator_2],split[logo_locator_2 + 1]),",|teamColor|\"")," ",":")

    color_locator_2 <- grep("teamColor",split)[2] + 1
    team2_color <- stringr::str_remove_all(split[color_locator_2],",|uid|\"")

    ishome_locator_2 <- grep("isHome",split)[2] + 1
    team2_ishome <- stringr::str_remove_all(split[ishome_locator_2],",|score|\"")

    runs_locator_2 <- grep(",\"statistics\"",split)[2]
    team2_runs <- stringr::str_remove_all(split[runs_locator_2],",|statistics|\"")

    hits_locator_1 <- grep("awy",split)[1] + 1
    away_hits <- stringr::str_split(stringr::str_remove_all(split[hits_locator_1],"hme|\""),",")[[1]][2]
    away_errors <- stringr::str_split(stringr::str_remove_all(split[hits_locator_1],"]|hme|\""),",")[[1]][3]

    hits_locator_2 <- grep("hme",split)[1] + 1
    home_hits <- stringr::str_split(stringr::str_remove_all(split[hits_locator_2],"lbls|\""),",")[[1]][2]
    home_errors <- stringr::str_split(stringr::str_remove_all(split[hits_locator_2],"]|lbls|\""),",")[[1]][3]

    description_locator <- grep("description",split)[1] + 1
    description <- stringr::str_remove_all(split[description_locator], ",|detail|\"")

    game_date_locator <- grep("date",split)[1] + 1
    game_date_raw <- stringr::str_remove_all(split[game_date_locator], ",|\"")
    game_date <- substr(game_date_raw,1,nchar(game_date_raw) - 3)

    game_time_locator <- grep("time\\\"",split)[1] + 1
    game_time <- stringr::str_replace(stringr::str_remove_all(paste(split[game_time_locator],split[game_time_locator + 1]),",|hideScoreDate|\"")," ",":")

    game_id <- substr(str_split(current,"https://www.espn.com/college-softball/game/_/gameId/")[[1]][2],1,9)

    if(description == "Scheduled"){
      team1_runs <- NA
      home_hits <- NA
      team1_errors <- NA

      team2_runs <- NA
      away_hits <- NA
      team2_errors <- NA
    }


    if(team1_ishome == "true"){
      home_team = 1
    }else if(team2_ishome == "true"){
      home_team = 2
    }else{
      home_team = 1
    }
    if(home_team == 1){
      temp_df <- data.frame(home_team_abbreviation = team1_abbreviation, home_team_display_name = team1_display_name,
                            home_team_mascot = team1_mascot,home_team_logo = team1_logo, home_team_color = team1_color,
                            home_team_runs = team1_runs, home_team_hits = home_hits, home_team_errors = home_errors,
                            away_team_abbreviation = team2_abbreviation, away_team_display_name = team2_display_name,
                            away_team_mascot = team2_mascot, away_team_logo = team2_logo, away_team_color = team2_color,
                            away_team_runs = team2_runs, away_team_hits = away_hits, away_team_errors = away_errors,
                            description = description, game_date = game_date, game_time = game_time, game_id = game_id)
    }
    else{
      temp_df <- data.frame(home_team_abbreviation = team2_abbreviation, home_team_display_name = team2_display_name,
                            home_team_mascot = team2_mascot,home_team_logo = team2_logo, home_team_color = team2_color,
                            home_team_runs = team2_runs, home_team_hits = home_hits, home_team_errors = home_errors,
                            away_team_abbreviation = team1_abbreviation, away_team_display_name = team1_display_name,
                            away_team_mascot = team1_mascot, away_team_logo = team1_logo, away_team_color = team1_color,
                            away_team_runs = team1_runs, away_team_hits = away_hits, away_team_errors = away_errors,
                            description = description, game_date = game_date, game_time = game_time, game_id = game_id)
    }

    scoreboard <- rbind(scoreboard,temp_df) %>% tidyr::drop_na(home_team_abbreviation)

    scoreboard$home_team_logo <- stringr::str_remove(scoreboard$home_team_logo,"uid")
    scoreboard$away_team_logo <- stringr::str_remove(scoreboard$away_team_logo,"uid")

  }

  scoreboard <- scoreboard %>%
    dplyr::filter(description == "Final")

  return(scoreboard)
}
