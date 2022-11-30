#' Gets full ESPN JSON file (Helper Function)
#'
#' @author Tyson King
#' @param gameID
#'
#' @return The entire json file for a given gameID on ESPN's website
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' ID = 401444869
#' try(get_json(ID))

get_json <- function(gameID){
  #Get full URL using gameID
  url <- paste0("https://cdn.espn.com/college-softball/playbyplay?render=false&userab=1&xhr=1&gameId=",gameID)

  #Fix accents, etc.
  res <- httr::RETRY("GET",url)
  resp <- res %>%
    httr::content(as = "text",encoding = "UTF-8")

  #Get data from JSON
  jackpot <- tryCatch(
    expr = {jsonlite::fromJSON(resp, flatten = TRUE)},
    error = function(err){"Game ID Does Not Exist"}
  )
  return(jackpot)
}
