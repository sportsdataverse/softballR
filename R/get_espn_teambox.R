get_espn_teambox <- function(gameID){
  jackpot <- get_json(gameID)

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

  team1_batting <- team1_stats[[1]] %>% select(abbreviation,value)
  team2_batting <- team2_stats[[1]] %>% select(abbreviation,value)

  team1_pitching <- team1_stats[[2]] %>% select(abbreviation,value)
  team2_pitching <- team2_stats[[2]] %>% select(abbreviation,value)

  team1_fielding <- team1_stats[[3]] %>% select(abbreviation,value)
  team2_fielding <- team2_stats[[3]] %>% select(abbreviation,value)

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
