#' Access NCAA Individual Stats Rankings
#'
#' @author Tyson King
#'
#' @return named list of data frames for the top 50 of each individual stat on the NCAA website
#' @importFrom tidyr separate
#' @importFrom stringr str_remove_all str_split
#' @importFrom dplyr arrange mutate across
#' @export
#'
#' @examples
#' try(get_individual_stats())
get_individual_stats <- function(){

  raw_html <- "https://www.ncaa.com/stats/softball/d1/current/individual/271" %>% readLines()

  start_location <- grep("INDIVIDUAL STATISTICS",raw_html)

  stat_meta <- stringr::str_remove_all(stringr::str_split(raw_html[start_location + 1],"</option><option value=")[[1]][-1],"</option></select>") %>%
    as.data.frame() %>%
    tidyr::separate(".",c("url","name"),">")

  stats <- list()

  for(i in 1:nrow(stat_meta)){
    url <- paste0("https://www.ncaa.com/",substr(stat_meta[i,]$url,2,str_length(stat_meta[i,]$url) - 1))

    stats_raw <- url %>% rvest::read_html() %>% rvest::html_table()
    stats_raw <- stats_raw[[1]]


    if(stats_raw[1,1] == 1){high_good = TRUE}else{FALSE}

    if(high_good == TRUE){
      stats_upd <- stats_raw %>% dplyr::mutate(Rank = rank(-dplyr::across(last_col())[[1]],ties.method = "min"))
    }
    else{
      stats_upd <- stats_raw %>% dplyr::mutate(Rank = rank(dplyr::across(last_col())[[1]],ties.method = "min"))
    }

    stats[[stat_meta[i,]$name]] <- stats_upd %>% dplyr::arrange(Rank)
  }

  return(stats)

}
