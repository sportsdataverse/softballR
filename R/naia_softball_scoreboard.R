#' Get all NAIA softball scores for a given day
#'
#' @author Tyson King
#' @description
#' @param date "YYYY-MM-DD"
#'
#' @return data frame of date, team names and their scores
#' @importFrom lubridate month day year
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @import magrittr
#' @export
#'
#' @examples
#' date = "2022-03-05"
#' try(naia_softball_scoreboard(date))
naia_softball_scoreboard <- function(date){

  options(warn = -1)

  if(as.Date(date) >= Sys.Date()){
    stop("Invalid Date")
  }

  if(!(is(date, "Date"))){

    year <- try(strsplit(date, "-")[[1]][1])
    month <- try(strsplit(date, "-")[[1]][2])
    day <- try(strsplit(date, "-")[[1]][3])

  } else{

    month <- lubridate::month(date)
    day <- lubridate::day(date)
    year <- lubridate::year(date)

  }

  url <- glue::glue("https://naiastats.prestosports.com/sports/sball/scoreboard?d={year}-{month}-{day}")

  raw_html <- url %>%
    readLines()

  game_locs <- grep("<div class=\"event-info clearfix\">", raw_html)

  if(length(game_locs) == 0){print("No games on this date"); return(NULL)}

  date_fmt <- paste0(year,
                     formatC(month, width = 2, format = "d", flag = "0"),
                     formatC(day, width = 2, format = "d", flag = "0"))

  game_id_locs <- grep(paste0("boxscores/",date_fmt), raw_html)

  winner_locs <- grep("<div class=\"team winner clearfix\">", raw_html)

  loser_locs <- grep("<div class=\"team loser clearfix\">", raw_html)

  assemble_df <- function(loc, next_loc){

    if(!stringr::str_detect(raw_html[loc - 10], "Final")) return(NULL)

    game_id <- try(strsplit(raw_html[game_id_locs[dplyr::between(game_id_locs, loc, next_loc - 1)]], paste0(date_fmt,"_|\\.xml"))[[1]][2], silent = T)

    winning_team <- try(strsplit(raw_html[winner_locs[dplyr::between(winner_locs, loc, next_loc - 1)] + 11], "title=\"|\"")[[1]][4], silent = T)

    winning_team_logo <- try(strsplit(raw_html[winner_locs[dplyr::between(winner_locs, loc, next_loc - 1)] + 8], "<img src=\"|\" alt=")[[1]][2], silent = T)

    winning_team_runs <- try(strsplit(raw_html[winner_locs[dplyr::between(winner_locs, loc, next_loc - 1)] + 2], "\"result\">|<")[[1]][3], silent = T)

    losing_team <- try(strsplit(raw_html[loser_locs[dplyr::between(loser_locs, loc, next_loc - 1)] + 11], "title=\"|\"")[[1]][4], silent = T)

    losing_team_logo <- try(strsplit(raw_html[loser_locs[dplyr::between(loser_locs, loc, next_loc - 1)] + 8], "<img src=\"|\" alt=")[[1]][2], silent = T)

    losing_team_runs <- try(strsplit(raw_html[loser_locs[dplyr::between(loser_locs, loc, next_loc - 1)] + 2], "\"result\">|<")[[1]][3], silent = T)

    if("try-error" %in% class(game_id)) game_id <- NA

    # In result of a tie

    if("try-error" %in% class(winning_team)){

      winning_team <- try(strsplit(raw_html[loc + 17], "title=\"|\"")[[1]][4], silent = T)

      winning_team_logo <- try(strsplit(raw_html[loc + 14], "<img src=\"|\" alt=")[[1]][2], silent = T)

      winning_team_runs <- try(strsplit(raw_html[loc + 8], "\"result\">|<")[[1]][3], silent = T)

      losing_team <- try(strsplit(raw_html[loc + 36], "title=\"|\"")[[1]][4], silent = T)

      losing_team_logo <- try(strsplit(raw_html[loc + 33], "<img src=\"|\" alt=")[[1]][2], silent = T)

      losing_team_runs <- try(strsplit(raw_html[loc + 27], "\"result\">|<")[[1]][3], silent = T)

    }

    final_df <- data.frame(game_date = date, game_id,
                           winning_team, winning_team_logo, winning_team_runs,
                           losing_team, losing_team_logo, losing_team_runs)

    return(final_df)

  }

  games <- data.frame()

  for(i in 1:length(game_locs)){

    games <- rbind(games, assemble_df(game_locs[i],
                                      min(game_locs[i + 1], length(raw_html), na.rm = T)))

  }

  print(date)

  games_final <- games %>%
    dplyr::mutate(winning_team_runs = as.numeric(winning_team_runs),
                  losing_team_runs = as.numeric(losing_team_runs))

  return(games)

}
