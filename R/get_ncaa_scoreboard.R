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


  test <- paste0("https://stats.ncaa.org/season_divisions/",division_id,"/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=",month,"%2F",day,"%2F",year) %>%
    rvest::read_html() %>%
    rvest::html_text() %>%
    strsplit("Box Score") %>%
    magrittr::extract2(1) %>%
    strsplit("\\n")

  for(i in 1:length(test)){

    for(j in 1:length(test[[i]])){

      test[[i]][j] <- test[[i]][j] %>% trimws()

    }

    test[[i]] <- test[[i]][nzchar(test[[i]])]

  }

  start_loc <- grep("Attendance",test[[1]])

  test[[1]] <- test[[1]][(start_loc+1):length(test[[1]])]

  assemble_df <- function(game_vector){

    if("Canceled" %in% game_vector){

      num_canceled <- length(grep("Canceled",game_vector))

      date_locs <- grep(paste(month,day,year,sep = "/"),game_vector)

      game_vector <- game_vector[-c(1:(date_locs[num_canceled+1]-1))]
    }

    date <- game_vector[1] %>% stringr::str_remove_all(" \\(1\\)| \\(2\\)")

    game_vector <- game_vector[!stringr::str_detect(game_vector, " \\(1\\)| \\(2\\)")]

    team1 <- game_vector[grep("\\(",game_vector)[1]] %>% strsplit(" \\(") %>% magrittr::extract2(1) %>% magrittr::extract(1)
    team2 <- game_vector[grep("\\(",game_vector)[2]] %>% strsplit(" \\(") %>% magrittr::extract2(1) %>% magrittr::extract(1)

    team1_runs <- game_vector[grep("Final",game_vector) - 1] %>% as.numeric
    team2_runs <- game_vector[length(game_vector)] %>% as.numeric

    upd_game_vector <- game_vector[!(game_vector) %in%
                                     c(date, team1, team2, team1_runs, team2_runs, "Final")]

    upd_game_vector <- upd_game_vector[-c(length(upd_game_vector), length(upd_game_vector) - 1)]

    game_df <- data.frame(date, team1, team2, team1_runs, team2_runs)

    return(game_df)

  }

  games_df <- data.frame()

  for(i in 1:(length(test)-1)){

    games_df <- rbind(games_df, assemble_df(test[[i]]))

  }

  return(games_df)

}
