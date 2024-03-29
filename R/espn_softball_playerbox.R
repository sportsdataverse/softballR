#' Get ESPN Player Box Score for given game
#'
#' @author Tyson King
#' @param gameID ESPN Game ID
#'
#' @return A named list of dataframes, hitting and pitching
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' ID = 401444869
#' try(espn_softball_playerbox(ID))

espn_softball_playerbox <- function(gameID){
  jackpot <- espn_json(gameID)

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
