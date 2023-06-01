#' Load NCAA Player Box Scores
#'
#' @description 2021-2023 for D1 hitting and pitching, only 2023 for everything else
#' @param season YYYY can also use vector of years
#' @param category Hitting, Pitching, or Fielding
#' @importFrom glue glue
#' @importFrom stringr str_replace
#'
#' @return dataframe of all box scores from NCAA website from selected season
#' @export
#'
#' @examples try(load_ncaa_softball_playerbox(2021:2023, category = "Hitting"))
load_ncaa_softball_playerbox <- function(season = 2023, category, division = "D1"){

  if(!is.numeric(season)) return("Invalid Season")

  if(!(category %in% c("Hitting", "Pitching", "Fielding"))) return("Invalid Category")

  if((season[1] != "2023" | length(season) > 1) & (division != "D1" | category == "Fielding")) return("Only includes 2023 data... for now")

  if(min(season < 2021 | max(season > 2023))) return("Invalid Season")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  division <- stringr::str_replace(division, "D", "d")

  url <- c()

  if(category == "Hitting"){

    for(i in season){
      url <- c(url, glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division}_hitting_box_scores_{i}.RDS?raw=true"))
    }

  } else if(category == "Pitching"){

    for(i in season){
      url <- c(url, glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division}_pitching_box_scores_{i}.RDS?raw=true"))
    }

  } else if(category == "Fielding"){

    url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/d1_fielding_box_scores_2023.RDS?raw=true")

  }

  box <- data.frame()

  for(i in url){

    con <- url(i)

    on.exit(close(con))

    box <- bind_rows(box, readRDS(con))

  }

  return(box)

}
