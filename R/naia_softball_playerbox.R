#' Get all box scores for a given game id and date
#'
#' @param game_id NAIA Game ID
#' @param game_date YYYY-MM-DD format
#'
#' @return named list with two elements, "Hitting" and "Pitching"
#' Each contains a dataframe of the box scores for that game
#'
#' @importFrom glue glue
#' @importFrom lubridate year month day
#' @importFrom rvest read_html html_table
#' @importFrom magrittr extract2
#' @importFrom dplyr pull mutate filter select
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' try(naia_softball_playerbox("r7z2", "2023-02-05"))
naia_softball_playerbox <- function(game_id, game_date){

  options(warn = -1)

  date <- try(as.Date(game_date))

  if("try-error" %in% class(date)) stop("Invalid date")

  date_fmt <- paste0(lubridate::year(date),
                     formatC(lubridate::month(date), width = 2, format = "d", flag = "0"),
                     formatC(lubridate::day(date), width = 2, format = "d", flag = "0"))

  url <- url(glue::glue("https://naiastats.prestosports.com/sports/sball/2022-23/boxscores/{date_fmt}_{game_id}.xml"), "rb")

  raw <- url %>%
    rvest::read_html() %>%
    rvest::html_table()

  teams <- raw %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1) %>%
    unlist() %>%
    as.character()

  hitting_1 <- raw %>%
    magrittr::extract2(2) %>%
    dplyr::mutate(team = teams[1])

  hitting_2 <- raw %>%
    magrittr::extract2(3) %>%
    dplyr::mutate(team = teams[2])

  total_hitting <- rbind(hitting_1, hitting_2) %>%
    dplyr::filter(Hitters != "Totals") %>%
    dplyr::mutate(player = stringr::str_remove_all(Hitters, "\n|\t")) %>%
    tidyr::separate(player, c("player", "position"), sep = "          ") %>%
    dplyr::select(team, player, position, AB, R, H, RBI, BB, SO)

  pitching_1 <- raw %>%
    magrittr::extract2(4) %>%
    dplyr::mutate(team = teams[1])

  pitching_2 <- raw %>%
    magrittr::extract2(5) %>%
    dplyr::mutate(team = teams[2])

  total_pitching <- rbind(pitching_1, pitching_2) %>%
    dplyr::filter(Pitchers != "Totals") %>%
    dplyr::mutate(player = stringr::str_remove_all(Pitchers, "\n|\t|\\(|,|\\)")) %>%
    tidyr::separate(player, c("player", "decision"), sep = "  ") %>%
    dplyr::mutate(decision = stringr::word(decision, 1)) %>%
    dplyr::select(team, player, decision, IP, H, R, ER, BB, SO, HR)

  return(list("Hitting" = total_hitting,
              "Pitching" = total_pitching))

}
