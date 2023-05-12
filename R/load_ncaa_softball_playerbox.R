#' Load NCAA Player Box Scores
#'
#' @description Only supports 2023 for now (only D1 for fielding)
#' @param season YYYY (2023)
#' @param category Hitting, Pitching, or Fielding
#' @importFrom glue glue
#'
#' @return dataframe of all box scores from NCAA website from selected season
#' @export
#'
#' @examples try(load_ncaa_softball_playerbox(2023))
load_ncaa_softball_playerbox <- function(season = 2023, category, division = "D1"){

  if(!is.numeric(season)) return("Invalid Season")

  if(!(category %in% c("Hitting", "Pitching", "Fielding"))) return("Invalid Category")

  if(season != 2023) return("Only includes 2023 data... for now")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if(category == "Hitting"){

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division}_hitting_box_scores_2023.RDS?raw=true")

  } else if(category == "Pitching"){

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division}_pitching_box_scores_2023.RDS?raw=true")

  } else if(category == "Fielding"){

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/d1_fielding_box_scores_2023.RDS?raw=true")

  }

  con <- url(url)

  on.exit(close(con))

  box <- try(readRDS(con), silent = TRUE)

  return(box)

}
