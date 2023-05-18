#' Load NAIA PBP
#'
#' @description Only supports 2023 season for now
#' @param season YYYY
#'
#' @return dataframe of all play by play data for an NAIA season
#' @importFrom glue glue
#' @export
#'
#' @examples try(load_naia_softball_pbp(2023))
load_naia_softball_pbp <- function(season){

  if(!is.numeric(season)) return("Invalid Input")

  if(season != 2023) stop("Invalid Season")

  url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/naia_pbp_{season}.RDS?raw=true")

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

  return(scoreboard)

}
