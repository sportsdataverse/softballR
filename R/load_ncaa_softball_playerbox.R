#' Load NCAA Player Box Scores
#'
#' @description Only supports 2023 data for D1 for now
#' @param season YYYY
#' @param category Hitting or Pitching
#'
#' @return dataframe of all box scores from NCAA website from selected season
#' @export
#'
#' @examples try(load_ncaa_softball_playerbox(2023))
load_ncaa_softball_playerbox <- function(season = 2023, category){

  if(!is.numeric(season)) return("Invalid Season")

  if(!(category %in% c("Hitting", "Pitching"))) return("Invalid Category")

  if(season != 2023) return("Only includes 2023 data... for now")

  if(category == "Hitting"){

    url <- "https://github.com/tmking2002/softballR-data/blob/main/data/d1_hitting_box_scores_2023.RDS?raw=true"

  } else if(category == "Pitching"){

    url <- "https://github.com/tmking2002/softballR-data/blob/main/data/d1_pitching_box_scores_2023.RDS?raw=true"

  }

  con <- url(url)

  on.exit(close(con))

  box <- try(readRDS(con), silent = TRUE)

  return(box)

}
