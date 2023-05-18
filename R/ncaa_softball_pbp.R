#' Get NCAA play-by-play data for an entire season
#'
#' @param game_id NCAA Game ID
#' @description only has D1 data for now
#'
#' @return dataframe of every play from team's season
#' @importFrom glue glue
#' @importFrom stringr str_detect str_split
#' @importFrom rvest read_html html_table
#' @importFrom dplyr filter pull rename select mutate arrange
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' try(ncaa_softball_pbp(2377327))
ncaa_softball_pbp <- function(game_id){

  options(warn = -1)

  box_raw <- glue::glue("https://stats.ncaa.org/contests/{game_id}/box_score") %>%
    readLines()

  pbp_id <- sub('.*/(\\d+).*', '\\1', box_raw[grep("Play by Play", box_raw)[1]])

  possible_events <- c("struck out swinging",
                       "struck out looking",
                       "grounded out",
                       "flied out",
                       "infield fly",
                       "hit into double play",
                       "lined out",
                       "out",
                       "reached on a fielder's choice",
                       "reached on an error",
                       "reached on a throwing error",
                       "reached on a fielding error",
                       "lined into double play",
                       "grounded into double play",
                       "hit by pitch",
                       "walked",
                       "intentionally walked",
                       "fouled out",
                       "fouled into double play",
                       "popped up",
                       "singled",
                       "doubled",
                       "tripled",
                       "homered")

  raw <- glue::glue("https://stats.ncaa.org/game/play_by_play/{pbp_id}") %>%
    rvest::read_html() %>%
    rvest::html_table()

  upd <- raw[(5:length(raw))][c(F,T)]

  df <- do.call(rbind, upd)

  names(df) <- df[1,]

  team_name <- df[1,1] %>% as.character()
  team_col <- 1

  opponent_name <- df[1,3] %>% as.character()
  opponent_col <- 3

  filtered <- df %>%
    dplyr::rename(team = team_col, opponent = opponent_col) %>%
    tidyr::separate(Score, c("away_team_runs", "home_team_runs"), sep = "-") %>%
    dplyr::select(team, away_team_runs, opponent, home_team_runs) %>%
    dplyr::mutate(new_inning = team == team_name,
                  inning = cumsum(new_inning)) %>%
    dplyr::filter(stringr::str_detect(team, paste(possible_events, collapse = "|")) |
                    stringr::str_detect(opponent, paste(possible_events, collapse = "|")) |
                    stringr::str_detect(team, "\\(|R:") |
                    stringr::str_detect(opponent, "\\(|R:")) %>%
    dplyr::select(-new_inning)

  away_team <- ifelse(filtered$team[1] != "", team_col, opponent_col)
  top_bottom <- as.character(ifelse(!filtered[away_team] == "", "bottom", "top"))

  filtered <- filtered  %>%
    dplyr::mutate(top_bottom = top_bottom,
                  events = ifelse(team == "", opponent, team),
                  team = ifelse(team == "", opponent_name, team_name),
                  opponent = ifelse(opponent == "", opponent_name, team_name)) %>%
    dplyr::filter(!stringr::str_detect(events, "H: "))

  # Play ID Format: game_id, inning, top/bottom (top = 0, bottom = 1), play #

  pbp <- filtered %>%
    dplyr::mutate(top_bottom = top_bottom,
                  game_id = game_id) %>%
    dplyr::group_by(inning, top_bottom) %>%
    dplyr::mutate(play_id = paste0(game_id, "_", inning, "_", as.numeric(top_bottom == "bottom"), "_", dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::filter(away_team_runs != "Score")

  return(pbp)

}

