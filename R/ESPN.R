# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(devtools)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(rvest)

get_json <- function(gameID){
  #Get full URL using gameID
  url <- paste0("https://cdn.espn.com/college-softball/playbyplay?render=false&userab=1&xhr=1&gameId=",gameID)

  #Fix accents, etc.
  res <- httr::RETRY("GET",url)
  resp <- res %>%
    httr::content(as = "text",encoding = "UTF-8")

  #Get data from JSON
  jackpot <- tryCatch(
    expr = {jsonlite::fromJSON(resp, flatten = TRUE)},
    error = function(err){"Game ID Does Not Exist"}
  )
  return(jackpot)
}

get_espn_teambox <- function(gameID){
  jackpot <- jsonlite::get_json(gameID)

  team_box <- jackpot$gamepackageJSON$boxscore$teams

  if(length(team_box) == 0){
    return("No Team Box Score")
  }

  team1 <- team_box$team.displayName[1]
  team2 <- team_box$team.displayName[2]

  team1logo <- team_box$team.logo[1]
  team2logo <- team_box$team.logo[2]

  team1_stats <- team_box[1,]$statistics[[1]]$stats
  team2_stats <- team_box[2,]$statistics[[1]]$stats

  team1_batting <- team1_stats[[1]] %>% dplyr::select(abbreviation,value)
  team2_batting <- team2_stats[[1]] %>% dplyr::select(abbreviation,value)

  team1_pitching <- team1_stats[[2]] %>% dplyr::select(abbreviation,value)
  team2_pitching <- team2_stats[[2]] %>% dplyr::select(abbreviation,value)

  team1_fielding <- team1_stats[[3]] %>% dplyr::select(abbreviation,value)
  team2_fielding <- team2_stats[[3]] %>% dplyr::select(abbreviation,value)

  batting_stats <- as.data.frame(matrix(nrow=0,ncol=25))
  names(batting_stats) <- c("team",team1_batting$abbreviation,"logo")

  batting_stats[1,] <- c(team1,team1_batting$value,team1logo)
  batting_stats[2,] <- c(team2,team2_batting$value,team2logo)

  pitching_stats <- as.data.frame(matrix(nrow=0,ncol=29))
  names(pitching_stats) <- c("team",team1_pitching$abbreviation,"logo")

  pitching_stats[1,] <- c(team1,team1_pitching$value,team1logo)
  pitching_stats[2,] <- c(team2,team2_pitching$value,team2logo)

  fielding_stats <- as.data.frame(matrix(nrow=0,ncol=17))
  names(fielding_stats) <- c("team",team1_fielding$abbreviation,"logo")

  fielding_stats[1,] <- c(team1,team1_fielding$value,team1logo)
  fielding_stats[2,] <- c(team2,team2_fielding$value,team2logo)

  team_box <- list("Batting" = batting_stats,"Pitching" = pitching_stats,"Fielding" = fielding_stats)

  return(team_box)
}

get_espn_playerbox <- function(gameID){
  jackpot <- get_json(gameID)

  player_box <- jackpot$gamepackageJSON$boxscore$players

  if(length(player_box) == 0){
    return("No Team Box Score")
  }

  team1 <- player_box$team.displayName[1]
  team2 <- player_box$team.displayName[2]

  team1logo <- player_box$team.logo[1]
  team2logo <- player_box$team.logo[2]

  hitting_stats <- player_box$statistics[[1]]$names[[1]]

  team1_hitters <- player_box$statistics[[1]]$athletes[[1]] %>% dplyr::select(athlete.displayName,athlete.id,position.name)
  team1_hitters_stats <- player_box$statistics[[1]]$athletes[[1]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% dplyr::mutate(athlete.id = team1_hitters$athlete.id)
  names(team1_hitters_stats) <- c(hitting_stats,"athlete.id")

  team1_hitters <- merge(team1_hitters,team1_hitters_stats) %>% dplyr::mutate(team = team1)

  team2_hitters <- player_box$statistics[[2]]$athletes[[1]] %>% dplyr::select(athlete.displayName,athlete.id,position.name)
  team2_hitters_stats <- player_box$statistics[[2]]$athletes[[1]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% dplyr::mutate(athlete.id = team2_hitters$athlete.id)
  names(team2_hitters_stats) <- c(hitting_stats,"athlete.id")

  team2_hitters <- merge(team2_hitters,team2_hitters_stats) %>% dplyr::mutate(team = team2)

  hitting_stats <- rbind(team1_hitters,team2_hitters)

  pitching_stats <- player_box$statistics[[1]]$names[[2]]

  team1_pitchers <- player_box$statistics[[1]]$athletes[[2]] %>% dplyr::select(athlete.displayName,athlete.id,position.name)
  team1_pitchers_stats <- player_box$statistics[[1]]$athletes[[2]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% dplyr::mutate(athlete.id = team1_pitchers$athlete.id)
  names(team1_pitchers_stats) <- c(pitching_stats,"athlete.id")

  team1_pitchers <- merge(team1_pitchers,team1_pitchers_stats) %>% dplyr::mutate(team = team1)

  team2_pitchers <- player_box$statistics[[2]]$athletes[[2]] %>% dplyr::select(athlete.displayName,athlete.id,position.name)
  team2_pitchers_stats <- player_box$statistics[[2]]$athletes[[2]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% dplyr::mutate(athlete.id = team2_pitchers$athlete.id)
  names(team2_pitchers_stats) <- c(pitching_stats,"athlete.id")

  team2_pitchers <- merge(team2_pitchers,team2_pitchers_stats) %>% dplyr::mutate(team = team2)

  pitching_stats <- rbind(team1_pitchers,team2_pitchers)

  player_box <- list("Hitting" = hitting_stats,"Pitching" = pitching_stats)

  return(player_box)
}

get_espn_pbp <- function(gameID){

  jackpot <- get_json(gameID)

  #Finds play by play data
  pbp <- jackpot$gamepackageJSON$plays %>%
    dplyr::dplyr::mutate(game_id = gameID)

  if(length(pbp) == 0){
    return("No Play by Play Data")
  }
  fix_pbp <- function(gameID){
    participants <- pbp$participants %>%
      bind_rows(.id = "rownum")

    participants2 <- participants %>%
      pivot_wider(names_from = type,values_from = athlete.id)

    temp <- pbp %>%
      dplyr::filter(participants != "NULL") %>%
      dplyr::select(-participants) %>%
      janitor::clean_names() %>%
      dplyr::mutate(rownum = as.character(row_number()))

    temp$on_third_athlete_id = ifelse("on_third_athlete_id" %in% colnames(temp),temp$on_third_athlete_id,NA)

    final <- left_join(temp,participants2,by = "rownum") %>%
      dplyr::select(-c(rownum,on_first_athlete_id,on_second_athlete_id,on_third_athlete_id)) %>%
      dplyr::filter(type_text != "start batter/pitcher" & type_text != "end batter/pitcher")

    return(final)
  }

  get_roster <- function(gameID){

    #Finds roster data
    batternames1 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[1]][["athletes"]][[1]][["athlete.shortName"]]
    batterids1 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[1]][["athletes"]][[1]][["athlete.id"]]
    pitchernames1 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[1]][["athletes"]][[2]][["athlete.shortName"]]
    pitcherids1 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[1]][["athletes"]][[2]][["athlete.id"]]
    batternames2 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[2]][["athletes"]][[1]][["athlete.shortName"]]
    batterids2 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[2]][["athletes"]][[1]][["athlete.id"]]
    pitchernames2 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[2]][["athletes"]][[2]][["athlete.shortName"]]
    pitcherids2 <- jackpot[["gamepackageJSON"]][["boxscore"]][["players"]][["statistics"]][[2]][["athletes"]][[2]][["athlete.id"]]
    team1 <- jackpot[["gamepackageJSON"]][["boxscore"]][["teams"]][["team.displayName"]][[1]]
    team2 <- jackpot[["gamepackageJSON"]][["boxscore"]][["teams"]][["team.displayName"]][[2]]


    names1 <- c(batternames1,pitchernames1)
    ids1 <- c(batterids1,pitcherids1)

    roster1 <- data.frame("Name" = names1,"ID" = ids1,"Team" = team1)

    names2 <- c(batternames2,pitchernames2)
    ids2 <- c(batterids2,pitcherids2)

    roster2 <- data.frame("Name" = names2,"ID" = ids2,"Team" = team2)

    roster <- rbind(roster1,roster2)

    return(roster)
  }

  #Example game: Texas vs. Oklahoma, Game 1 WCWS 2022
  playbyplay <- fix_pbp(gameID)
  playbyplay$pitcher_name <- NA
  playbyplay$batter_name <- NA
  playbyplay$onFirst_name <- NA
  playbyplay$onSecond_name <- NA
  playbyplay$onThird_name <- NA
  playbyplay$hitting_team <- NA
  playbyplay$pitching_team <- NA


  roster <- get_roster(gameID)


  for (i in 1:nrow(playbyplay)) {
    if(!is.na(playbyplay$pitcher[i])){
      pitcherid <- playbyplay$pitcher[i]
      pitchername <- roster %>% dplyr::filter(ID == pitcherid) %>% dplyr::select(Name) %>% as.character()

      pitchingteamname <- roster %>% dplyr::filter(ID == pitcherid) %>% dplyr::select(Team) %>% as.character()

      playbyplay$pitcher_name[i] <- pitchername
      playbyplay$pitching_team[i] <- pitchingteamname
    }

    if(!is.na(playbyplay$batter[i])){
      batterid <- playbyplay$batter[i]
      battername <- roster %>% dplyr::filter(ID == batterid) %>% dplyr::select(Name) %>% as.character()

      hittingteamname <- roster %>% dplyr::filter(ID == batterid) %>% dplyr::select(Team) %>% as.character()

      playbyplay$batter_name[i] <- battername
      playbyplay$hitting_team[i] <- hittingteamname
    }

    if(!is.na(playbyplay$onFirst[i])){
      onFirstid <- playbyplay$onFirst[i]
      onFirstname <- roster %>% dplyr::filter(ID == onFirstid) %>% dplyr::select(Name) %>% as.character()

      playbyplay$onFirst_name[i] <- onFirstname
    }

    if(!is.na(playbyplay$onSecond[i])){
      onSecondid <- playbyplay$onSecond[i]
      onSecondname <- roster %>% dplyr::filter(ID == onSecondid) %>% dplyr::select(Name) %>% as.character()

      playbyplay$onSecond_name[i] <- onSecondname
    }

    if(!is.na(playbyplay$onThird[i])){
      onThirdid <- playbyplay$onThird[i]
      onThirdname <- roster %>% dplyr::filter(ID == onThirdid) %>% dplyr::select(Name) %>% as.character()

      playbyplay$onThird_name[i] <- onThirdname
    }
  }

  playbyplay$onThird = ifelse("onThird" %in% colnames(playbyplay),playbyplay$onThird,NA)

  new_pbp <- playbyplay %>%
    dplyr::select(-c(pitcher,batter,onFirst,onSecond,onThird)) %>%
    dplyr::filter(type_text != "Play Result") %>%
    dplyr::mutate(sequence_number = as.numeric(sequence_number) - 1,
           home_team = first(pitching_team),
           away_team = first(hitting_team),
           end_of_ab = ifelse(summary_type == "P",FALSE,TRUE),
           scoring_play = ifelse(end_of_ab == TRUE,scoring_play,FALSE),
           score_value = ifelse(end_of_ab == TRUE,score_value,0),
           hit_coordinate_x = ifelse(type_text == "foul ball",NA,hit_coordinate_x),
           hit_coordinate_y = ifelse(type_text == "foul ball",NA,hit_coordinate_y)) %>%
    dplyr::select(sequence_number,home_score,away_score,period_number,
           period_type,outs,pitch_count_balls,pitch_count_strikes,result_count_balls,result_count_strikes,
           pitcher_name,batter_name,bats_abbreviation,bat_order,onFirst_name,onSecond_name,onThird_name,
           text,type_id,type_text,pitch_coordinate_x,pitch_coordinate_y,hit_coordinate_x,hit_coordinate_y,
           home_team,away_team,game_id)

  return(new_pbp)
}
