#' Get Massey Scoreboard
#'
#' @author Tyson King
#' @param league D1, D2, or D3
#'
#' @return The final score of all games through the current date
#' @importFrom stringr str_remove_all word
#' @importFrom dplyr na_if case_when
#' @export
#'
#' @examples
#' try(get_massey_scoreboard("D1"))
get_massey_scoreboard <- function(league){

  league_id <- dplyr::case_when(league == "D1" ~ "11590",
                                league == "D2" ~ "11606",
                                league == "D3" ~ "11620",
                                TRUE ~ "error")

  if(league_id == "error") return("invalid league")

  full <- paste0("https://masseyratings.com/scores.php?s=518060&sub=",league_id) %>%
    readLines()

  start <- grep("pre",full)[1]
  end <- grep("pre",full)[2] - 4

  games <- full[start:end] %>%
    stringr::str_remove_all("<a href=\"/|<hr><pre>|@")

  game_date <- as.Date(trimws(substr(games,1,10)))

  games_upd <- games %>%
    stringr::str_remove_all(paste(as.character(game_date),"518060","/",">",sep="|")) %>%
    trimws()

  winning_team_id <- games_upd %>%
    stringr::word(1, sep = "\"") %>%
    as.numeric

  winning_team_name <- games_upd %>%
    stringr::word(2, sep = "\"|<")

  winning_team_runs <- games_upd %>%
    stringr::str_remove_all("a") %>%
    stringr::word(3, sep = "\"|<") %>%
    trimws() %>%
    stringr::word(1) %>%
    as.numeric

  losing_team_id <- games_upd %>%
    stringr::str_remove_all("a") %>%
    stringr::word(3, sep = "\"|<") %>%
    trimws() %>%
    substr(3,100) %>%
    trimws() %>%
    as.numeric

  losing_team_name <- games_upd %>%
    stringr::word(4, sep = "\"|<")

  losing_team_runs <- games_upd %>%
    stringr::str_remove_all("a") %>%
    stringr::word(5, sep = "\"|<") %>%
    trimws() %>%
    stringr::word(1) %>%
    as.numeric

  description <- games_upd %>%
    stringr::str_remove_all("a") %>%
    stringr::word(5, sep = "\"|<") %>%
    trimws() %>%
    stringr::word(2) %>%
    dplyr::na_if("")

  games_df <- data.frame(game_date,
                         winning_team_id,
                         winning_team_name,
                         winning_team_runs,
                         losing_team_id,
                         losing_team_name,
                         losing_team_runs,
                         description) %>%
    dplyr::filter(description != "Sch" | is.na(description))

  return(games_df)

}
