#' Load all-time info for all NCAA softball teams (all divisions)
#'
#' @return df with 9 columns of info on each season of each team
#' @export
#'
#' @examples
#' try(load_ncaa_softball_team_info())
load_ncaa_softball_team_info <- function(){

  url <- "https://github.com/sportsdataverse/softballR-data/raw/main/data/ncaa_team_info.RDS"

  con <- url(url)

  on.exit(close(con))

  info <- try(readRDS(con), silent = TRUE)

  info$season <- ifelse(info$season == 1900, 2000, info$season)

  return(info)

}
