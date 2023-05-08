#' Get ESPN Softball Season Scoreboard
#'
#' @author Tyson King
#' @param season YYYY (2015 - 2022)
#' @description Disclaimer: Can take a while to run
#'
#' @return data frame with data from every game in the given season
#' @importFrom dplyr select filter
#' @export
#'
#' @examples
#' try(get_espn_softball_season_scoreboard(2021))
get_espn_softball_season_scoreboard <- function(season){
  options(warn = -1)

  s <- try(as.numeric(season))

  if("try-error" %in% class(s) || is.na(s) || s < 2015 || s > 2023){
    stop("Invalid Date")
  }

  seasons <- data.frame(season = 2015:2023,
                        start_date = c("2015-02-05","2016-02-11","2017-02-09","2018-02-08","2019-02-07","2020-02-06","2021-02-11","2022-02-10","2023-02-09"),
                        end_date = c("2015-06-03","2016-06-08","2017-06-07","2018-06-06","2019-06-04","2020-03-12","2021-06-10","2022-06-09","2023-06-09")) #Go back and fix after season

  start_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(start_date) %>% as.character() %>% as.Date()
  end_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(end_date) %>% as.character() %>% as.Date()

  scoreboard <- data.frame()

  for(i in seq(start_date,min(end_date,Sys.Date()),1)){
    date = as.Date(i,origin = "1970-01-01")

    temp <- try(espn_softball_scoreboard(date),silent = TRUE)

    if("try-error" %in% class(temp)){
      next
    }

    scoreboard <- rbind(scoreboard,temp)
  }

  scoreboard <- scoreboard %>%
    filter(description == "Final")

  return(scoreboard)
}

