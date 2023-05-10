#' Get NCAA play-by-play data for an entire season
#'
#' @param team_id get this from ncaa_teams function
#' @description only has D1 data for now
#'
#' @return dataframe of every play from team's season
#' @importFrom glue glue
#' @importFrom stringr str_detect str_split
#' @importFrom rvest read_html html_table
#' @importFrom dplyr filter pull rename select mutate arrange
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' try(ncaa_softball_season_pbp(549186))
ncaa_softball_season_pbp <- function(team_id){

  options(warn = -1)

  team_site <- try(glue::glue("https://stats.ncaa.org/teams/{team_id}") %>%
                     readLines())

  if("try-error" == class(team_site)) stop("Invalid team id")

  ids <- rbind(ncaa_softball_teams(2021),
               ncaa_softball_teams(2022),
               ncaa_softball_teams(2023))

  team_id_curr <- team_id

  team_name <- ids %>%
    dplyr::filter(softball_id == team_id_curr) %>%
    dplyr::pull(team_name)

  games <- grep("http://web2.ncaa.org/ncaa_style/img/All_Logos", team_site)[2:56] + 3

  games <- games[which(stringr::str_detect(team_site[games],"BOX_SCORE_WINDOW"))]

  pbp <- data.frame()

  for(i in 1:length(games)){

    current_id <- as.numeric(stringr::str_split(team_site[games[i]],"          <a target=\"BOX_SCORE_WINDOW\" class=\"skipMask\" href=\"/contests/|/box_score")[[1]][2])

    pbp <- rbind(pbp, ncaa_softball_pbp(current_id))
    print(i)

  }

  return(pbp)

}

