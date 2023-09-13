#' Get all NCAA softball scores for a given day
#'
#' @author Tyson King
#' @description Supports 2017-2023 for D1 and only 2023 for D2 and D3
#' @param date "YYYY-MM-DD"
#' @param division "D1", "D2", or "D3"
#'
#' @return data frame of date, team names and their scores
#' @importFrom lubridate month year day
#' @importFrom stringr str_remove_all str_detect
#' @importFrom dplyr case_when distinct
#' @importFrom magrittr extract extract2
#' @importFrom rvest read_html html_text
#' @importFrom glue glue
#' @export
#'
#' @examples
#' date = "2022-03-05"
#' try(ncaa_softball_scoreboard(date))
ncaa_softball_scoreboard <- function(date, division = "D1"){

  if(as.Date(date) >= Sys.Date()){
    stop("Invalid Date")
  }

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if(!(is(date, "Date"))){

    year <- try(strsplit(date, "-")[[1]][1])
    month <- try(strsplit(date, "-")[[1]][2])
    day <- try(strsplit(date, "-")[[1]][3])

  } else{

    month <- lubridate::month(date)
    day <- lubridate::day(date)
    year <- lubridate::year(date)

  }

  division_id <- dplyr::case_when(division == "D1" & year == 2023 ~ 18101,
                                  division == "D1" & year == 2022 ~ 17840,
                                  division == "D1" & year == 2021 ~ 17540,
                                  division == "D1" & year == 2020 ~ 17103,
                                  division == "D1" & year == 2019 ~ 16820,
                                  division == "D1" & year == 2018 ~ 15196,
                                  division == "D1" & year == 2017 ~ 13300,
                                  division == "D1" & year == 2016 ~ 12981,
                                  division == "D1" & year == 2015 ~ 12400,
                                  division == "D1" & year == 2014 ~ 11920,
                                  division == "D1" & year == 2013 ~ 11180,
                                  division == "D1" & year == 2012 ~ 10741,
                                  division == "D2" & year == 2023 ~ 18102,
                                  division == "D2" & year == 2022 ~ 17841,
                                  division == "D2" & year == 2021 ~ 17541,
                                  division == "D2" & year == 2020 ~ 17104,
                                  division == "D2" & year == 2019 ~ 16821,
                                  division == "D2" & year == 2018 ~ 15197,
                                  division == "D2" & year == 2017 ~ 13301,
                                  division == "D2" & year == 2016 ~ 12980,
                                  division == "D3" & year == 2023 ~ 18103,
                                  division == "D3" & year == 2022 ~ 17842,
                                  division == "D3" & year == 2021 ~ 17542,
                                  division == "D3" & year == 2020 ~ 17105,
                                  division == "D3" & year == 2019 ~ 16822,
                                  division == "D3" & year == 2018 ~ 15198,
                                  division == "D3" & year == 2017 ~ 13302,
                                  division == "D3" & year == 2016 ~ 12982)


  raw <- glue::glue("https://stats.ncaa.org/season_divisions/{division_id}/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date={month}%2F{day}%2F{year}&conference_id=0&tournament_id=&commit=Submit") %>%
    readLines()

  locs <- grep("<tr id=\"", raw)

  if(length(locs) == 0) return(NULL)

  assemble_df <- function(loc, next_loc){

    game_vec <- raw[loc:(next_loc-1)]

    if(any(grepl("Canceled", game_vec))) return(NULL)

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

  for(i in 1:(length(locs) - 1)){

    if(i %% 2 == 0) next

    loc <- locs[i]

    if(i == length(locs) - 3){
      next_loc <- length(raw)
    } else{
      next_loc <- locs[i + 2]
    }

    games_df <- rbind(games_df, assemble_df(loc, loc + 70))

  }

  if(length(games_df)== 0) return(NULL)

  games_df <- games_df %>%
    dplyr::filter(away_team_runs != "") %>%
    dplyr::mutate(home_team_runs = as.numeric(home_team_runs),
                  away_team_runs = as.numeric(away_team_runs),
                  game_date = stringr::str_remove_all(game_date, " \\(1\\)| \\(2\\)| \\<br/\\>\\*If necessary")) %>%
    dplyr::distinct()

  print(paste0(date, ": completed"))

  return(games_df)

}
