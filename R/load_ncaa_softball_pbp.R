#' Load NCAA PBP
#'
#' @description Play-by-play for every game of an NCAA season. The
#'   softballR-data release starts in 2021 (D1, D2 and D3); any season up to the
#'   current calendar year is accepted and a season that has not been published
#'   yet returns a `try-error` from the download.
#' @param season YYYY
#' @param division D1, D2, or D3
#'
#' @return dataframe of all play by play data for an NCAA season
#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @export
#'
#' @examples try(load_ncaa_softball_pbp(2024))
load_ncaa_softball_pbp <- function(season, division = "D1"){

  if(!is.numeric(season)) stop("Invalid Input")

  .check_softball_season(season, floor = 2021)

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid division")

  division_id <- stringr::str_replace(division, "D", "d")

  url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division_id}_ncaa_pbp_{season}.RDS?raw=true")

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

  return(scoreboard)

}
