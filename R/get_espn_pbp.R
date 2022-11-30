#' Get ESPN Play-by-Play data
#'
#' @author Tyson King
#' @param gameID
#'
#' @return A data frame of every pitch of the given game
#' @importFrom dplyr bind_rows mutate filter select row_number left_join first
#' @importFrom  tidyr pivot_wider
#' @importFrom  janitor clean_names
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' ID = 401444869
#' try(get_espn_pbp(ID))

get_espn_pbp <- function(gameID){

  #jackpot: full json file for game
  jackpot <- get_json(gameID)

  #Finds play by play data
  pbp <- jackpot$gamepackageJSON$plays %>%
    dplyr::mutate(game_id = gameID)

  #Some games only have scoreboard data and not play-by-play
  if(length(pbp) == 0){
    return("No Play by Play Data")
  }

  #Internal function for adjusting the pbp data frame
  fix_pbp <- function(pbp){
    #the participants row is an embedded list, so this is fixing that
    participants <- pbp$participants %>%
      dplyr::bind_rows(.id = "rownum")

    #expands skinny data frame so it has a separate column for each participant
    participants2 <- participants %>%
      tidyr::pivot_wider(names_from = type,values_from = athlete.id)

    #cleaning up column names, etc.
    temp <- pbp %>%
      dplyr::filter(participants != "NULL") %>%
      dplyr::select(-participants) %>%
      janitor::clean_names() %>%
      dplyr::mutate(rownum = as.character(dplyr::row_number()))

    #some games never had a player on third, so that column was excluded... this fixes that i think
    temp$on_third_athlete_id = ifelse("on_third_athlete_id" %in% colnames(temp),temp$on_third_athlete_id,NA)

    #rejoins the two data frames and filters substitution rows
    final <- dplyr::left_join(temp,participants2,by = "rownum") %>%
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

  pbp_upd <- fix_pbp(pbp) %>%
    dplyr::mutate(pitcher_name = NA,
                  batter_name = NA,
                  onFirst_name = NA,
                  onSecond_name = NA,
                  onThird_name = NA,
                  hitting_team = NA,
                  pitching_team = NA)


  roster <- get_roster(gameID)

  tic()
  for (i in 1:nrow(pbp_upd)) {
    if(!is.na(pbp_upd$pitcher[i])){
      pitcherid <- pbp_upd$pitcher[i]
      pitchername <- roster %>% dplyr::filter(ID == pitcherid) %>% dplyr::select(Name) %>% as.character()

      pitchingteamname <- roster %>% dplyr::filter(ID == pitcherid) %>% dplyr::select(Team) %>% as.character()

      pbp_upd$pitcher_name[i] <- pitchername
      pbp_upd$pitching_team[i] <- pitchingteamname
    }

    if(!is.na(pbp_upd$batter[i])){
      batterid <- pbp_upd$batter[i]
      battername <- roster %>% dplyr::filter(ID == batterid) %>% dplyr::select(Name) %>% as.character()

      hittingteamname <- roster %>% dplyr::filter(ID == batterid) %>% dplyr::select(Team) %>% as.character()

      pbp_upd$batter_name[i] <- battername
      pbp_upd$hitting_team[i] <- hittingteamname
    }

    if(!is.na(pbp_upd$onFirst[i])){
      onFirstid <- pbp_upd$onFirst[i]
      onFirstname <- roster %>% dplyr::filter(ID == onFirstid) %>% dplyr::select(Name) %>% as.character()

      pbp_upd$onFirst_name[i] <- onFirstname
    }

    if(!is.na(pbp_upd$onSecond[i])){
      onSecondid <- pbp_upd$onSecond[i]
      onSecondname <- roster %>% dplyr::filter(ID == onSecondid) %>% dplyr::select(Name) %>% as.character()

      pbp_upd$onSecond_name[i] <- onSecondname
    }

    if(!is.na(pbp_upd$onThird[i])){
      onThirdid <- pbp_upd$onThird[i]
      onThirdname <- roster %>% dplyr::filter(ID == onThirdid) %>% dplyr::select(Name) %>% as.character()

      pbp_upd$onThird_name[i] <- onThirdname
    }
  }
  toc()

  pbp_upd$onThird = ifelse("onThird" %in% colnames(pbp_upd),pbp_upd$onThird,NA)

  new_pbp <- pbp_upd %>%
    dplyr::select(-c(pitcher,batter,onFirst,onSecond,onThird)) %>%
    dplyr::filter(type_text != "Play Result") %>%
    dplyr::mutate(sequence_number = as.numeric(sequence_number) - 1,
                  home_team = dplyr::first(pitching_team),
                  away_team = dplyr::first(hitting_team),
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
