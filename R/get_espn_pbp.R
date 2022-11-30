get_espn_pbp <- function(gameID){

  jackpot <- get_json(gameID)

  #Finds play by play data
  pbp <- jackpot$gamepackageJSON$plays %>%
    mutate(game_id = gameID)

  if(length(pbp) == 0){
    return("No Play by Play Data")
  }
  fix_pbp <- function(gameID){
    participants <- pbp$participants %>%
      bind_rows(.id = "rownum")

    participants2 <- participants %>%
      pivot_wider(names_from = type,values_from = athlete.id)

    temp <- pbp %>%
      filter(participants != "NULL") %>%
      select(-participants) %>%
      janitor::clean_names() %>%
      mutate(rownum = as.character(row_number()))

    temp$on_third_athlete_id = ifelse("on_third_athlete_id" %in% colnames(temp),temp$on_third_athlete_id,NA)

    final <- left_join(temp,participants2,by = "rownum") %>%
      select(-c(rownum,on_first_athlete_id,on_second_athlete_id,on_third_athlete_id)) %>%
      filter(type_text != "start batter/pitcher" & type_text != "end batter/pitcher")

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
      pitchername <- roster %>% filter(ID == pitcherid) %>% select(Name) %>% as.character()

      pitchingteamname <- roster %>% filter(ID == pitcherid) %>% select(Team) %>% as.character()

      playbyplay$pitcher_name[i] <- pitchername
      playbyplay$pitching_team[i] <- pitchingteamname
    }

    if(!is.na(playbyplay$batter[i])){
      batterid <- playbyplay$batter[i]
      battername <- roster %>% filter(ID == batterid) %>% select(Name) %>% as.character()

      hittingteamname <- roster %>% filter(ID == batterid) %>% select(Team) %>% as.character()

      playbyplay$batter_name[i] <- battername
      playbyplay$hitting_team[i] <- hittingteamname
    }

    if(!is.na(playbyplay$onFirst[i])){
      onFirstid <- playbyplay$onFirst[i]
      onFirstname <- roster %>% filter(ID == onFirstid) %>% select(Name) %>% as.character()

      playbyplay$onFirst_name[i] <- onFirstname
    }

    if(!is.na(playbyplay$onSecond[i])){
      onSecondid <- playbyplay$onSecond[i]
      onSecondname <- roster %>% filter(ID == onSecondid) %>% select(Name) %>% as.character()

      playbyplay$onSecond_name[i] <- onSecondname
    }

    if(!is.na(playbyplay$onThird[i])){
      onThirdid <- playbyplay$onThird[i]
      onThirdname <- roster %>% filter(ID == onThirdid) %>% select(Name) %>% as.character()

      playbyplay$onThird_name[i] <- onThirdname
    }
  }

  playbyplay$onThird = ifelse("onThird" %in% colnames(playbyplay),playbyplay$onThird,NA)

  new_pbp <- playbyplay %>%
    select(-c(pitcher,batter,onFirst,onSecond,onThird)) %>%
    filter(type_text != "Play Result") %>%
    mutate(sequence_number = as.numeric(sequence_number) - 1,
           home_team = first(pitching_team),
           away_team = first(hitting_team),
           end_of_ab = ifelse(summary_type == "P",FALSE,TRUE),
           scoring_play = ifelse(end_of_ab == TRUE,scoring_play,FALSE),
           score_value = ifelse(end_of_ab == TRUE,score_value,0),
           hit_coordinate_x = ifelse(type_text == "foul ball",NA,hit_coordinate_x),
           hit_coordinate_y = ifelse(type_text == "foul ball",NA,hit_coordinate_y)) %>%
    select(sequence_number,home_score,away_score,period_number,
           period_type,outs,pitch_count_balls,pitch_count_strikes,result_count_balls,result_count_strikes,
           pitcher_name,batter_name,bats_abbreviation,bat_order,onFirst_name,onSecond_name,onThird_name,
           text,type_id,type_text,pitch_coordinate_x,pitch_coordinate_y,hit_coordinate_x,hit_coordinate_y,
           home_team,away_team,game_id)

  return(new_pbp)
}
