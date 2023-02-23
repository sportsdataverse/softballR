#' Load ESPN Scoreboard
#'
#' @param seasons vector of seasons (YYYY:YYYY)
#'
#' @return dataframe of all games on ESPN website from selected seasons
#' @importFrom dplyr mutate filter
#' @importFrom lubridate year
#' @export
#'
#' @examples try(load_espn_scoreboard(2020:2023))
load_espn_scoreboard <- function(seasons){

  if(!is.numeric(seasons)) return("Invalid Input")

  curr_url <- "https://github.com/tmking2002/softballR-data/blob/main/data/espn_scoreboard_2023.RDS?raw=true"
  past_url <- "https://github.com/tmking2002/softballR-data/blob/main/data/espn_scoreboard_2015_2022.RDS?raw=true"

  curr_con <- url(curr_url)
  past_con <- url(past_url)

  on.exit(close(curr_con))
  on.exit(close(past_con))

  curr_scoreboard <- try(readRDS(curr_con), silent = TRUE)
  past_scoreboard <- try(readRDS(past_con), silent = TRUE)

  full_scoreboard <- append(past_scoreboard, list("2023" = curr_scoreboard))

  scoreboard <- do.call(rbind, full_scoreboard) %>%
    dplyr::mutate(season = lubridate::year(game_date)) %>%
    `rownames<-`(NULL) %>%
    dplyr::filter(season %in% seasons)
}
