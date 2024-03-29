% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaa_softball_season_playerbox.R
\name{ncaa_softball_season_playerbox}
\alias{ncaa_softball_season_playerbox}
\title{Get all box scores for a team's entire season (pitching, hitting, and fielding)}
\usage{
ncaa_softball_season_playerbox(team_id)
}
\arguments{
\item{team_id}{get team ids from get_ncaa_teams function}
}
\value{
named list with three elements, "Hitting", "Pitching" and "Fielding".
Each contains a dataframe of every box score for that team's season
}
\description{
only has data for 2021, 2022, and 2023 and only has D1 for now
}
\examples{
try(ncaa_softball_season_playerbox(549186))
}
