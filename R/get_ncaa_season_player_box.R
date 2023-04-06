#' Get all box scores for a team's entire season (pitching, hitting, and fielding)
#'
#' @param team_id get team ids from get_ncaa_teams function
#' @description only has data for 2021, 2022, and 2023 and only has D1 for now
#'
#' @return named list with three elements, "Hitting", "Pitching" and "Fielding".
#' Each contains a dataframe of every box score for that team's season
#' @importFrom glue glue
#' @importFrom dplyr filter pull mutate
#' @export
#'
#' @examples
#' try(get_ncaa_season_player_box(549186))
get_ncaa_season_player_box <- function(team_id){

  options(warn = -1)

  team_site <- try(glue::glue("https://stats.ncaa.org/teams/{team_id}") %>%
    readLines())

  if("try-error" == class(team_site)) stop("Invalid team id")

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

  get_hitting_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{id}") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      dplyr::filter(!(Player %in% c(ids$team_name,"Player","Totals")))

    upd$team <- ifelse(upd$Player %in% raw[[6]]$X1, first_team, second_team)
    upd[upd == ""] <- "0"

    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric))

    return(upd)

  }

  get_pitching_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{id}?year_stat_category_id=15021") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      dplyr::filter(!(Player %in% c(ids$team_name,"Player","Totals")))

    upd$team <- ifelse(upd$Player %in% raw[[6]]$X1, first_team, second_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")

    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric)) %>%
      dplyr::filter(IP > 0)

    return(upd)

  }

  get_fielding_box <- function(id){

    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{id}?year_stat_category_id=15022") %>%
      rvest::read_html() %>%
      rvest::html_table()

    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])

    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      dplyr::filter(!(Player %in% c(ids$team_name,"Player","Totals")))

    upd$team <- ifelse(upd$Player %in% raw[[6]]$X1, first_team, second_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")

    upd <- upd %>%
      dplyr::mutate(across(3:12, as.numeric))

    return(upd)

  }

  hitting <- data.frame()
  pitching <- data.frame()
  fielding <- data.frame()

  for(i in 1:length(url_exts)){

    date_raw <- glue::glue("https://stats.ncaa.org/game/box_score/{url_exts[i]}") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      magrittr::extract2(3)

    date <- as.character(date_raw[1,2])

    hitting <- rbind(hitting, get_hitting_box(url_exts[i]) %>% dplyr::mutate(game_date = date))
    pitching <- rbind(pitching, get_pitching_box(url_exts[i]) %>% dplyr::mutate(game_date = date))
    fielding <- rbind(fielding, get_fielding_box(url_exts[i]) %>% dplyr::mutate(game_date = date))

  }

  return(list("Hitting" = hitting,
              "Pitching" = pitching,
              "Fielding" = fielding))

}
