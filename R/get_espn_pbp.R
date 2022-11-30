#' Get ESPN Play-by-Play data
#'
#' @author Tyson King
#' @param gameID
#'
#' @return A data frame of every pitch of the given game
#' @importFrom dplyr bind_rows mutate filter select row_number left_join
#' @importFrom  tidyr pivot_wider
#' @importFrom  janitor clean_names
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' ID = 401444869
#' try(get_espn_pbp(ID))

get_espn_pbp <- function(gameID){

  jackpot <- get_json(gameID)

  #Finds play by play data
  pbp <- jackpot$gamepackageJSON$plays %>%
    dplyr::mutate(game_id = gameID)

  if(length(pbp) == 0){
    return("No Play by Play Data")
  }
  fix_pbp <- function(gameID){
    participants <- pbp$participants %>%
      dplyr::bind_rows(.id = "rownum")

    participants2 <- participants %>%
      tidyr::pivot_wider(names_from = type,values_from = athlete.id)

    temp <- pbp %>%
      dplyr::filter(participants != "NULL") %>%
      dplyr::select(-participants) %>%
      janitor::clean_names() %>%
      dplyr::mutate(rownum = as.character(dplyr::row_number()))

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
