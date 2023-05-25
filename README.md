# softballR <a href='https://github.com/tmking2002/softballR/'><img src="https://raw.githubusercontent.com/tmking2002/softballR/main/logo.png" align="right"  width="20%" min-width="100px"/></a>
<b>softballR</b> is an R package for acquiring NCAA softball data.

The package has functions for obtaining game-by-game scores, box scores, (some) play-by-play, and some different rankings websites. 

## Installation

You can install the current version (1.1.2) of the package like this:
      
``` r
# install.packages("devtools")
devtools::install_github("tmking2002/softballR")
```

## Important Functions 

### Get scores for all games in a given season

``` r
scoreboard <- softballR::load_ncaa_softball_scoreboard(season = 2023)
```

### Get box scores for all games in a given season (Only has 2023 data for now)

``` r
hitting <- softballR::load_ncaa_softball_playerbox(season = 2023, category = "Hitting")

pitching <- softballR::load_ncaa_softball_playerbox(season = 2023, category = "Pitching")
```

### Get all NCAA play by play data for a given team and season (2021-2023 for now)

``` r
# Use Oklahoma 2022 as example

team_id <- softballR::ncaa_softball_teams(2022) %>%
    dplyr::filter(team_name == "Oklahoma") %>%
    dplyr::pull(softball_id)
    
pbp <- softballR::ncaa_softball_season_pbp(team_id)
```


## **Author**

[Tyson King](https://twitter.com/tking0426)  
<a href="https://twitter.com/tking0426" target="blank"><img src="https://img.shields.io/twitter/follow/tking0426?color=blue&label=%40tking0426&logo=twitter&style=for-the-badge" alt="@tking0426" /></a>
<a href="https://github.com/tmking2002" target="blank"><img src="https://img.shields.io/github/followers/tmking2002?color=eee&logo=Github&style=for-the-badge" alt="@tmking2002" /></a>
