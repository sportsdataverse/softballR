#' Get all box scores for a given game id
#'
#' @param team_id get game ids from get_ncaa_scoreboard function
#'
#' @return named list with three elements, "Hitting", "Pitching" and "Fielding".
#' Each contains a dataframe of the box scores for that game
#' @importFrom glue glue
#' @importFrom dplyr filter pull mutate
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove_all
#' @export
#'
#' @examples
#' try(get_ncaa_player_box(2377042))
get_ncaa_player_box <- function(game_id){

  options(warn = -1)

  get_hitting_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      janitor::clean_names() %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"

    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric)) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  get_pitching_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      readLines()

    pitching_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
      stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")

    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{pitching_id}?year_stat_category_id=15021") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      janitor::clean_names() %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")

    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric)) %>%
      dplyr::filter(ip > 0) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  get_fielding_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      readLines()

    fielding_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
      stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")

    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{fielding_id}?year_stat_category_id=15022") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      janitor::clean_names() %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

    upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")

    upd <- upd %>%
      dplyr::mutate(across(3:12, as.numeric)) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  hitting <- try(get_hitting_box(game_id))
  pitching <- try(get_pitching_box(game_id))
  fielding <- try(get_fielding_box(game_id))

  return(list("Hitting" = hitting,
              "Pitching" = pitching,
              "Fielding" = fielding))

}
