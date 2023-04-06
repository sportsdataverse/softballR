#' Get all NCAA softball scores for a given day
#'
#' @author Tyson King
#' @description for now only supports 2022 and 2023 seasons, will update further later
#' @param date "YYYY-MM-DD"
#'
#' @return data frame of date, team names and their scores
#' @importFrom lubridate month year day
#' @importFrom stringr str_remove_all str_detect
#' @importFrom dplyr case_when
#' @importFrom magrittr extract extract2
#' @importFrom rvest read_html html_text
#' @export
#'
#' @examples
#' date = "2022-03-05"
#' try(get_ncaa_scoreboard(date))
get_ncaa_scoreboard <- function(date){

  if(as.Date(date) >= Sys.Date()){
    stop("Invalid Date")
  }

  if(class(date) != "Date"){

    year <- try(strsplit(date, "-")[[1]][1])
    month <- try(strsplit(date, "-")[[1]][2])
    day <- try(strsplit(date, "-")[[1]][3])

  } else{

    month <- lubridate::month(date)
    day <- lubridate::day(date)
    year <- lubridate::year(date)

  }

  division_id <- dplyr::case_when(year == 2023 ~ 18101,
                                  year == 2022 ~ 17840,
                                  year == 2021 ~ 15620,
                                  year == 2020 ~ 15220,
                                  year == 2019 ~ 16820)


  raw <- paste0("https://stats.ncaa.org/season_divisions/",division_id,"/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=",month,"%2F",day,"%2F",year) %>%
    readLines()

  locs <- grep("<tr id=\"", raw)

  assemble_df <- function(loc, next_loc){

    game_vec <- raw[loc:(next_loc-1)]

    game_id <- game_vec[grep("<tr id=\"", game_vec)[1]] %>%
      trimws() %>%
      stringr::str_remove_all("<tr id=\"contest_|\">")

    game_date <- game_vec[grep("<td rowspan=\"2\" valign=\"middle\">", game_vec)[1] + 1] %>%
      trimws()

    away_team <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[1]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)

    away_team_id <- game_vec[grep("<a target=\"TEAMS_WIN\" class=\"skipMask\" href=\"/teams/",game_vec)[1]] %>%
      strsplit("href=\"/teams/|\">") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)

    away_team_logo <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[1]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(3) %>%
      stringr::str_remove_all("\" />")

    away_team_runs <- game_vec[grep("<div id=\"score_", game_vec)[1] + 1] %>%
      trimws()

    home_team <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[2]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)

    home_team_id <- game_vec[grep("<a target=\"TEAMS_WIN\" class=\"skipMask\" href=\"/teams/",game_vec)[2]] %>%
      strsplit("href=\"/teams/|\">") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)

    home_team_logo <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[2]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(3) %>%
      stringr::str_remove_all("\" />")

    home_team_runs <- game_vec[grep("<div id=\"score_", game_vec)[2] + 1] %>%
      trimws()

    status <- game_vec[grep("<div class=\"livestream", game_vec) + 1] %>%
      trimws()

    game_df <- data.frame(away_team, away_team_id, away_team_logo, away_team_runs,
                          home_team, home_team_id, home_team_logo, home_team_runs,
                          game_date, game_id, status) %>%
      filter(status == "Final")

    return(game_df)

  }

  games_df <- data.frame()

  for(i in 1:(length(locs) - 2)){

    if(i %% 2 == 0) next

    loc <- locs[i]

    if(i == length(locs) - 3){
      next_loc <- length(raw)
    } else{
      next_loc <- locs[i + 2]
    }

    games_df <- rbind(games_df, assemble_df(loc, loc + 100))

  }

  games_df <- games_df %>%
    dplyr::filter(away_team_runs != "") %>%
    dplyr::mutate(home_team_runs = as.numeric(home_team_runs),
                  away_team_runs = as.numeric(away_team_runs),
                  game_date = stringr::str_remove_all(game_date, " \\(1\\)| \\(2\\)"))

  return(games_df)

}
