% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaa_softball_season_pbp.R
\name{ncaa_softball_season_pbp}
\alias{ncaa_softball_season_pbp}
\title{Get NCAA play-by-play data for an entire season}
\usage{
ncaa_softball_season_pbp(team_id)
}
\arguments{
\item{team_id}{get this from ncaa_teams function}
}
\value{
dataframe of every play from team's season
}
\description{
only has D1 data for now
}
\examples{
try(ncaa_softball_season_pbp(549186))
}
