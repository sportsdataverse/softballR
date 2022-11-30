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

  team1_hitters <- player_box$statistics[[1]]$athletes[[1]] %>% select(athlete.displayName,athlete.id,position.name)
  team1_hitters_stats <- player_box$statistics[[1]]$athletes[[1]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% mutate(athlete.id = team1_hitters$athlete.id)
  names(team1_hitters_stats) <- c(hitting_stats,"athlete.id")

  team1_hitters <- merge(team1_hitters,team1_hitters_stats) %>% mutate(team = team1)

  team2_hitters <- player_box$statistics[[2]]$athletes[[1]] %>% select(athlete.displayName,athlete.id,position.name)
  team2_hitters_stats <- player_box$statistics[[2]]$athletes[[1]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% mutate(athlete.id = team2_hitters$athlete.id)
  names(team2_hitters_stats) <- c(hitting_stats,"athlete.id")

  team2_hitters <- merge(team2_hitters,team2_hitters_stats) %>% mutate(team = team2)

  hitting_stats <- rbind(team1_hitters,team2_hitters)

  pitching_stats <- player_box$statistics[[1]]$names[[2]]

  team1_pitchers <- player_box$statistics[[1]]$athletes[[2]] %>% select(athlete.displayName,athlete.id,position.name)
  team1_pitchers_stats <- player_box$statistics[[1]]$athletes[[2]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% mutate(athlete.id = team1_pitchers$athlete.id)
  names(team1_pitchers_stats) <- c(pitching_stats,"athlete.id")

  team1_pitchers <- merge(team1_pitchers,team1_pitchers_stats) %>% mutate(team = team1)

  team2_pitchers <- player_box$statistics[[2]]$athletes[[2]] %>% select(athlete.displayName,athlete.id,position.name)
  team2_pitchers_stats <- player_box$statistics[[2]]$athletes[[2]]$stats %>% as.data.frame() %>% t() %>% as.data.frame(row.names = NULL) %>% mutate(athlete.id = team2_pitchers$athlete.id)
  names(team2_pitchers_stats) <- c(pitching_stats,"athlete.id")

  team2_pitchers <- merge(team2_pitchers,team2_pitchers_stats) %>% mutate(team = team2)

  pitching_stats <- rbind(team1_pitchers,team2_pitchers)

  player_box <- list("Hitting" = hitting_stats,"Pitching" = pitching_stats)

  return(player_box)
}
