% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/espn_softball_scoreboard.R
\name{espn_softball_scoreboard}
\alias{espn_softball_scoreboard}
\title{Get ESPN Softball Scoreboard}
\usage{
espn_softball_scoreboard(date)
}
\arguments{
\item{date}{YYYY-MM-DD}
}
\value{
data frame with 21 columns of relevant game information
}
\description{
Get ESPN Softball Scoreboard
}
\examples{
try(espn_softball_scoreboard("2022-03-05"))
}
\author{
Tyson King
}
