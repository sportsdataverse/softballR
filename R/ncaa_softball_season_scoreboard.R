#' Get all NCAA softball scores for a given season
#'
#' @description Provides the score and some other info for all D1, D2, and D3 games from 2016-2023 (for now)
#' @param season YYYY 2016-2023
#' @param division "D1", "D2", or "D3"
#'
#' @return data frame of date, team names and their scores
#' @importFrom dplyr filter select distinct
#' @export
#'
#' @examples
#' season = "2024"
#' try(ncaa_softball_season_scoreboard(season))
ncaa_softball_season_scoreboard <- function(season, division = "D1"){
  options(warn = -1)
  
  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")
  
  s <- try(as.numeric(season))
  
  if("try-error" %in% class(s) || is.na(s) || s < 2017 || s > 2023){
    stop("Invalid Season")
  }
  
  seasons <- data.frame(season = 2015:2023,
                        start_date = c("2015-02-05","2016-02-11","2017-02-09","2018-02-08","2019-02-07","2020-02-06","2021-02-11","2022-02-10","2023-02-09", "2024-02-08"),
                        end_date = c("2015-06-03","2016-06-08","2017-06-07","2018-06-06","2019-06-04","2020-03-12","2021-06-10","2022-06-09","2023-06-09", "2024-06-09")) #Go back and fix after season
  
  start_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(start_date) %>% as.character() %>% as.Date()
  end_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(end_date) %>% as.character() %>% as.Date()
  
  scoreboard <- data.frame()
  
  dates <- seq(start_date,min(end_date,Sys.Date()-1),1)
  
  scoreboard <- do.call(rbind, lapply(X = dates, FUN = ncaa_softball_scoreboard, division = division)) %>%
    dplyr::distinct()
  
  return(scoreboard)
}
