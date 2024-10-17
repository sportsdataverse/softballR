#' Get all box scores for a given game id
#'
#' @param game_id NCAA Game ID
#'
#' @return named list with three elements, "Hitting", "Pitching" and "Fielding".
#' Each contains a dataframe of the box scores for that game
#' @importFrom glue glue
#' @importFrom dplyr filter pull mutate
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove_all
#' @importFrom lubridate year
#' @importFrom anytime anydate
#' @export
#'
#' @examples
#' try(ncaa_softball_playerbox(2377042))
ncaa_softball_playerbox <- function(game_id){

  options(warn = -1)

  get_hitting_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/individual_stats") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[2]][2,1])
    second_team <- as.character(raw[[2]][3,1])
    date <- as.character(raw[[2]][4,1])

    upd <- rbind(raw[[4]],raw[[5]]) %>%
      janitor::clean_names() %>%
      dplyr::rename(player = name) %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[4]]$Name, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"

    cols = c("player", "pos", "g", "rbi", "ab", "r", "h", "x2b", "x3b", "tb", "hr", "ibb", "bb", "hbp", "sf", "sh", "k", "dp", "sb", "cs", "picked", "team", "opponent", "game_id", "game_date", "season")

    upd <- upd %>%
      dplyr::rename(pos = p) %>%
      dplyr::mutate(g = 1, game_date = date, season = lubridate::year(anytime::anydate(date)), game_id = game_id) %>%
      dplyr::select(cols) %>%
      dplyr::mutate(across(.cols = 3:20, .fns = \(col) as.numeric(str_remove(col, "/"))))

    return(upd)

  }

  get_pitching_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/individual_stats") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[2]][2,1])
    second_team <- as.character(raw[[2]][3,1])
    date <- as.character(raw[[2]][4,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      janitor::clean_names() %>%
      dplyr::rename(player = name) %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[6]]$Name, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")

    cols = c("game_id", "team", "opponent", "player", "ip", "ha", "er", "bb", "hb", "so", "bf", "hr_a", "season")

    upd <- upd %>%
      dplyr::mutate(game_id = game_id, season = lubridate::year(anytime::anydate(date))) %>%
      dplyr::select(cols) %>%
      dplyr::mutate(across(5:12, as.numeric)) %>%
      dplyr::filter(ip > 0)

    return(upd)

  }

  get_fielding_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/individual_stats") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[2]][2,1])
    second_team <- as.character(raw[[2]][3,1])
    date <- as.character(raw[[2]][4,1])

    upd <- rbind(raw[[8]],raw[[9]]) %>%
      janitor::clean_names() %>%
      dplyr::rename(player = name) %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[8]]$Name, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"

    upd <- upd %>%
      dplyr::select(-number) %>%
      dplyr::rename(pos = p) %>%
      dplyr::mutate(across(3:12, as.numeric)) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  hitting <- try(get_hitting_box(game_id))
  pitching <- try(get_pitching_box(game_id))
  #fielding <- try(get_fielding_box(game_id))

  return(list("Hitting" = hitting,
              "Pitching" = pitching))

}

