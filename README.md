# softballR <a href='https://github.com/tmking2002/softballR/'><img src="https://raw.githubusercontent.com/tmking2002/softballR/main/logo.png" align="right"  width="20%" min-width="100px"/></a>
Package that scrapes and cleans college softball data from NCAA, ESPN, and a few others.

## Installation

You can install the current version (0.5.0) of the package like this:
      
``` r
# install.packages("devtools")
devtools::install_github("tmking2002/softballR")
```

## Important Functions 

### Get ESPN Play-by-Play for a Given Game ID

``` r
id <- 401444869 # Game 2 of 2022 WCWS
pbp <- get_espn_pbp(id)
```

### Get basic stats (Runs, Hits, Errors, Etc.) for all games in a given season

``` r
espn_season_scoreboard <- load_espn_season_scoreboard(2022)
```

### Get scores for all games in a given season

``` r
ncaa_season_scoreboard <- load_ncaa_season_scoreboard(2023)
```

### Get all play by play data for a certain team's season

```r
# get team id for 2023 oklahoma team
oklahoma_id <- get_ncaa_teams(2023) %>% 
  dplyr::filter(team_name == "Oklahoma") %>% 
  dplyr::pull(team_id)
  
pbp <- get_ncaa_pbp(oklahoma_id)
```

### Get the Current RPI Rankings

``` r
rankings <- get_rankings("RPI")
```

## **Author**

[Tyson King](https://twitter.com/tking0426)  
<a href="https://twitter.com/tking0426" target="blank"><img src="https://img.shields.io/twitter/follow/tking0426?color=blue&label=%40tking0426&logo=twitter&style=for-the-badge" alt="@tking0426" /></a>
<a href="https://github.com/tmking2002" target="blank"><img src="https://img.shields.io/github/followers/tmking2002?color=eee&logo=Github&style=for-the-badge" alt="@tmking2002" /></a>
