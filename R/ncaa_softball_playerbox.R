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
#' @export
#'
#' @examples
#' try(ncaa_softball_playerbox(2377042))
ncaa_softball_playerbox <- function(game_id){

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
      dplyr::mutate(across(.cols = 3:76, .fns = \(col) as.numeric(str_remove(col, "/")))) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  get_pitching_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      readLines()

    pitching_url <- raw[grep("Pitching", raw)] %>% 
      trimws() %>% 
      stringr::str_remove_all("\\<a href=\"|\"\\>Pitching\\</a\\>  &nbsp;\\|") %>% 
      paste0("https://stats.ncaa.org/", .)

    raw <- pitching_url %>%
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
      dplyr::mutate(across(3:35, as.numeric)) %>%
      dplyr::filter(ip > 0) %>%
      dplyr::mutate(game_id = game_id)

    return(upd)

  }

  get_fielding_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      readLines()

    fielding_url <- raw[grep("Fielding", raw)] %>% 
      trimws() %>% 
      stringr::str_remove_all("\\<a href=\"|\"\\>Fielding\\</a\\>") %>% 
      paste0("https://stats.ncaa.org/", .)

    raw <- fielding_url %>% 
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
      dplyr::mutate(across(3:11, as.numeric)) %>%
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
