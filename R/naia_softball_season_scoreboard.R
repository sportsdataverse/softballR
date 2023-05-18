#' Get all NAIA softball scores for a given season
#'
#' @description
#' @param season YYYY Only 2023 for now
#'
#' @return data frame of date, team names and their scores
#' @importFrom dplyr filter select distinct
#' @export
#'
#' @examples
#' season = "2023"
#' try(naia_softball_season_scoreboard(season))
naia_softball_season_scoreboard <- function(season){

   options(warn = -1)

  s <- try(as.numeric(season))

  if("try-error" %in% class(s) || is.na(s) || s < 2016 || s > 2023){
    stop("Invalid Season")
  }

  seasons <- data.frame(season = 2023,
                        start_date = c("2023-02-05"),
                        end_date = c("2023-06-09")) #Go back and fix after season

  start_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(start_date) %>% as.character() %>% as.Date()
  end_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(end_date) %>% as.character() %>% as.Date()

  scoreboard <- data.frame()

  dates <- seq(start_date,min(end_date,Sys.Date()-1),1)

  scoreboard <- do.call(rbind, lapply(X = dates, FUN = naia_softball_scoreboard))  %>%
    dplyr::distinct()

  return(scoreboard)
}
