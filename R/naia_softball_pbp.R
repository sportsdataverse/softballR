#' Get NCAA play-by-play data for a given game id and game date
#'
#' @param game_id NAIA Game ID
#' @param game_date YYYY-MM-DD format
#' @description only has D1 data for now
#'
#' @return dataframe of every play from a given game
#' @importFrom glue glue
#' @importFrom lubridate year month day
#' @importFrom magrittr extract2 extract
#' @importFrom dplyr pull mutate select
#' @importFrom rvest read_html html_table
#' @importFrom stringr word str_remove_all str_count
#' @export
#'
#' @examples
#' try(naia_softball_pbp("r7z2", "2023-02-05"))
naia_softball_pbp <- function(game_id, game_date){

  options(warn = -1)

  date <- try(as.Date(game_date))

  if("try-error" %in% class(date)) stop("Invalid date")

  date_fmt <- paste0(lubridate::year(date),
                     formatC(lubridate::month(date), width = 2, format = "d", flag = "0"),
                     formatC(lubridate::day(date), width = 2, format = "d", flag = "0"))

  url <- glue::glue("https://naiastats.prestosports.com/sports/sball/2022-23/boxscores/{date_fmt}_{game_id}.xml?view=plays&inning=1")

  raw <- url %>%
    rvest::read_html() %>%
    rvest::html_table()

  if(length(raw) < 8){print("No PBP"); return(NULL)}

  teams <- raw %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1) %>%
    unlist() %>%
    as.character()

  format_inning <- function(pbp_vec){

    info <- pbp_vec[1]

    hitting_team <- strsplit(info, "\n")[[1]][1]
    top_bottom <- stringr::word(trimws(strsplit(info, "\n")[[1]][3]),1)
    inning <- stringr::word(trimws(strsplit(info, "\n")[[1]][3]),2, sep = "         ") %>%
      stringr::str_remove_all("st|nd|rd|th")

    plays <- pbp_vec[2:(length(pbp_vec)-1)]

    curr_pbp <- data.frame()
    outs <- 0
    new_outs <- 0

    for(i in 1:length(plays)){

      play <- strsplit(plays[i],"\n")[[1]][1]

      new_outs <- outs + stringr::str_count(play, "out|popped up")
      runs_scored <- stringr::str_count(play, "scored")

      pitching_team <- setdiff(teams, hitting_team)

      if(hitting_team == teams[1]){
        away_team_runs <<- away_team_runs + runs_scored
      } else{
        home_team_runs <<- home_team_runs + runs_scored
      }

      curr_pbp <- rbind(curr_pbp, data.frame(hitting_team, pitching_team, top_bottom, inning, play, outs, new_outs, away_team_runs, home_team_runs))

      outs <- new_outs

    }

    return(curr_pbp)

  }

  pbp <- data.frame()
  away_team_runs <- 0
  home_team_runs <- 0

  for(i in 9:length(raw)){

    if(length(raw[[i]]) != 0){

      pbp <- rbind(pbp, format_inning(raw[[i]]$X1))

    }

  }

  pbp <- pbp %>%
    dplyr::mutate(game_date, game_id) %>%
    dplyr::select(game_date, game_id, hitting_team, pitching_team, top_bottom,
                  inning, play, outs, new_outs, away_team_runs, home_team_runs)

  return(pbp)
}
