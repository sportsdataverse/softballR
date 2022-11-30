#' Get NCAA Softball Team Rankings
#'
#' @author Tyson King
#' @param source
#' @description Source options: RPI, D1Softball, USA Today, or Massey
#'
#' @return data frame of rankings, format changes based on source
#' @importFrom stringr str_length str_remove str_split
#' @importFrom tidyr separate
#' @importFrom dplyr mutate select arrange
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom httr RETRY content
#' @importFrom rvest html_table read_html
#' @export
#'
#' @examples
#' sources <- c("RPI","D1Softball","USA Today","Massey")
#' for(i in sources){
#'   try(get_rankings(i))
#' }
get_rankings <- function(source){

  `%!in%` = Negate(`%in%`)

  #Could go back and add ESPN and Softball America
  sources <- c("RPI","D1Softball","USA Today","Massey")

  if(source %!in% sources){
    stop("Invalid Source")
  }

  if(source == "RPI"){
    rankings <- "https://www.ncaa.com/rankings/softball/d1/ncaa-womens-softball-rpi" %>% rvest::read_html() %>% rvest::html_table()
    rankings <- rankings[[1]]
  }

  if(source == "D1Softball"){
    raw_html <- "https://d1softball.com/category/top-25" %>% readLines

    rankings <- as.data.frame(matrix(nrow = 0,ncol = 2))
    names(rankings) <- c("rank","team")

    first_location <- grep("<li><a href=\"/team/",raw_html)[1]

    for(i in 0:24){
      temp <- raw_html[first_location + i]
      length <- stringr::str_length(temp)

      team <- stringr::str_remove(stringr::str_split(temp,">")[[1]][7],"</a")
      rank <- i

      rankings <- rbind(rankings,data.frame(rank = rank,team = team))
    }
  }

  if(source == "USA Today"){
    rankings <- "https://nfca.org/component/com_nfca/Itemid,230/list,1/pdiv,div1/top25,1/year,2022/" %>% rvest::read_html() %>% rvest::html_table()
    rankings <- rankings[[1]] %>%
      tidyr::separate(Team,c("Team","First Place Votes"),"\\(") %>%
      dplyr::mutate(`First Place Votes` = stringr::str_remove(`First Place Votes`,"\\)"),
                    `First Place Votes` = ifelse(is.na(`First Place Votes`),0,`First Place Votes`))
  }

  if(source == "Massey"){
    json <- "https://masseyratings.com/json/rate.php?argv=slxlZrMjujc7FOv1L0Uz63vHFtgvVYTN9tPFcHcgcxIs72nWxxwgp35IAExoj-Cv39AN9n6oCP6-MoaTOAPIJchbHTuLa7KpHCbHhWc4sGw.&task=json"

    #Fix accents, etc.
    res <- httr::RETRY("GET",json)
    resp <- res %>%
      httr::content(as = "text",encoding = "UTF-8")

    #Get data from JSON
    jackpot <- tryCatch(
      expr = {jsonlite::fromJSON(resp, flatten = TRUE)},
      error = function(err){"Error"}
    )

    colnames <- c("team","team_id","conference","conference_id","record","delta")

    rankings <- jackpot$DI %>%
      unlist() %>%
      as.data.frame() %>%
      split(1:23) %>%
      as.data.frame() %>%
      janitor::clean_names() %>%
      dplyr::select(c(1,3,4,6,7,12)) %>%
      `names<-`(colnames) %>%
      tidyr::separate(record,c("wins","losses","ties"),"-") %>%
      dplyr::mutate_at(5:8,as.numeric) %>%
      dplyr::mutate(ties = ifelse(is.na(ties),0,ties),
             win_perc = wins / (wins + losses + ties),
             `rank (by win_perc)` = rank(-win_perc,ties.method = "min")) %>%
      dplyr::arrange(`rank (by win_perc)`) %>%
      dplyr::select(10,1:9)

  }

  return(rankings)
}
