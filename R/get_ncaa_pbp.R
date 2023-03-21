#' Get NCAA play-by-play data for an entire season
#'
#' @param team_id get this from ncaa_teams function
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
#' try(get_ncaa_pbp(549186))
get_ncaa_pbp <- function(team_id){

  team_site <- glue::glue("https://stats.ncaa.org/teams/{team_id}") %>%
    readLines()

  ids <- rbind(get_ncaa_teams(2021),
               get_ncaa_teams(2022),
               get_ncaa_teams(2023))

  team_id_curr <- team_id

  team_name <- ids %>%
    dplyr::filter(team_id == team_id_curr) %>%
    dplyr::pull(team_name)

  games <- grep("http://web2.ncaa.org/ncaa_style/img/All_Logos", team_site)[2:56] + 3

  games <- games[which(stringr::str_detect(team_site[games],"BOX_SCORE_WINDOW"))]

  url_exts <- c()

  for(i in 1:length(games)){

    current_ext <- as.numeric(stringr::str_split(team_site[games[i]],"          <a target=\"BOX_SCORE_WINDOW\" class=\"skipMask\" href=\"/contests/|/box_score")[[1]][2])

    current_html <- glue::glue("https://stats.ncaa.org/contests/{current_ext}/box_score") %>% readLines()

    url <- current_html[grep("Play by Play",current_html)[1]]

    url_upd <- as.numeric(stringr::str_split(url, "\t\t\t<a href=\"/game/play_by_play/|\">Play by Play</a>")[[1]][2])

    url_exts <- append(url_exts, url_upd)

  }

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

  get_pbp <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/game/play_by_play/{id}") %>%
      rvest::read_html() %>%
      rvest::html_table()

    upd <- raw[(5:length(raw))][c(F,T)]

    df <- do.call(rbind, upd)
    names(df) <- df[1,]

    team_col <- which(df[1,] == team_name)
    opponent_col <- ifelse(team_col == 3, 1, 3)

    opponent_name <- as.character(df[1, opponent_col])

    filtered <- df %>%
      dplyr::rename(team = team_col, opponent = opponent_col) %>%
      dplyr::select(team, opponent) %>%
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
                    team = ifelse(team == "", opponent_name, team_name)) %>%
      dplyr::filter(!stringr::str_detect(events, "H: ")) %>%
      dplyr::select(-opponent)

    # Removing this part for now

    #runs <- filtered %>%
    #  dplyr::group_by(inning, top_bottom) %>%
    #  dplyr::mutate(inning_stats = dplyr::last(events),
    #                inning_runs = ifelse(!str_detect(inning_stats, "R:"), 0,
    #                                     as.numeric(str_remove(regmatches(inning_stats, regexpr("R:\\s+(\\d+)", inning_stats)),"R: ")))) %>%
    #  dplyr::filter(events == dplyr::last(events)) %>%
    #  dplyr::ungroup() %>%
    #  dplyr::group_by(team) %>%
    #  dplyr::mutate(runs = cumsum(inning_runs)) %>%
    #  dplyr::select(team, inning, top_bottom, runs)

    pbp <- filtered %>%
      dplyr::mutate(top_bottom = top_bottom,
                    game_id = id) %>%
      #merge(runs, by = c("team", "inning", "top_bottom")) %>%
      dplyr::arrange(inning, team) %>%
      dplyr::mutate(play = stringr::str_extract(events, paste(possible_events, collapse = "|"))) %>%
      tidyr::separate(events, c("player", "result"), paste(possible_events, collapse = "|"), remove = FALSE) %>%
      dplyr::rename(batting_team = team) %>%
      dplyr::select(inning, top_bottom, batting_team, events, player, play, game_id)

    return(pbp)

  }

  games <- data.frame()

  for(i in 1:length(url_exts)){

    games <- rbind(games, get_pbp(url_exts[i]))

  }

  return(games)

}

