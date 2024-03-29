% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaa_softball_season_scoreboard.R
\name{ncaa_softball_season_scoreboard}
\alias{ncaa_softball_season_scoreboard}
\title{Get all NCAA softball scores for a given season}
\usage{
ncaa_softball_season_scoreboard(season, division = "D1")
}
\arguments{
\item{season}{YYYY 2016-2023}

\item{division}{"D1", "D2", or "D3"}
}
\value{
data frame of date, team names and their scores
}
\description{
Provides the score and some other info for all D1, D2, and D3 games from 2016-2023 (for now)
}
\examples{
season = "2023"
try(ncaa_softball_season_scoreboard(season))
}
