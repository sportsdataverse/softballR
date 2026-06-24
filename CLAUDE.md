# CLAUDE.md — softballR

R package for acquiring college softball data (NCAA, ESPN, NAIA) — game-by-game
scores, player/team box scores, some play-by-play, and rankings. Part of the
SportsDataverse R ecosystem. Pre-built season data lives in the sibling
`softballR-data` repo (see its CLAUDE.md). Upstream author: Tyson King
(`tmking2002`); version 1.4.0, MIT.

## Commands

No test suite, `_pkgdown.yml`, NEWS.md, or CI workflows exist in this repo yet.
Standard R-package dev commands (from repo root):

```r
devtools::document()   # regenerate NAMESPACE + man/ from roxygen2 (RoxygenNote 7.2.3)
devtools::load_all()   # load package for interactive testing
devtools::check()      # R CMD check
devtools::install_github("tmking2002/softballR")   # install (README install path)
```

Install for users: `devtools::install_github("tmking2002/softballR")`. Not on
CRAN.

## Architecture

`R/` splits into live scrapers and data-repo loaders. Two-tier by source prefix:

- **NCAA** — `ncaa_softball_*` scrape `stats.ncaa.org` HTML live (`rvest`):
  `ncaa_softball_scoreboard`, `_season_scoreboard`, `_pbp`, `_season_pbp`,
  `_playerbox`, `_season_playerbox`, `_teams`, `_rosters`, `_rankings`.
- **ESPN** — `espn_softball_*` hit ESPN JSON endpoints (`espn_json` helper):
  `espn_softball_scoreboard`, `_season_scoreboard`, `_pbp`, `_playerbox`,
  `_teambox`.
- **NAIA** — `naia_softball_*` scrape NAIA: `naia_softball_scoreboard`,
  `_season_scoreboard`, `_pbp`, `_playerbox`.
- **`load_*` loaders** — read pre-built `.RDS` from the data repo over raw
  GitHub blob URLs (NOT live scraping): `load_ncaa_softball_scoreboard`,
  `_pbp`, `_playerbox`, `_rosters`, `_team_info`; `load_espn_softball_scoreboard`;
  `load_naia_softball_pbp`, `_scoreboard`. Loaders are the fast path; scrapers
  are what the data repo's build scripts call to refresh those `.RDS` files.
- **Helpers** — `get_cur_season`, `espn_json`.

Coverage is uneven and encoded in each loader's season guards (e.g. NCAA
scoreboard 2012–2024 D1 / 2016+ other divisions; `load_ncaa_softball_pbp`
2021–2024; NAIA loaders 2023 only; rosters 2021–2023). Read the `if (season …)`
checks in `R/load_*.R` before assuming a year is available.

## Conventions

- roxygen2 markdown docstrings (`@param`/`@return`/`@export`/`@examples`);
  regenerate `NAMESPACE` + `man/` with `devtools::document()` — never hand-edit.
- tidyverse/`magrittr` `%>%` pipe style; `dplyr`/`tidyr`/`stringr`/`rvest`/
  `glue`/`janitor`/`lubridate`/`anytime`/`httr`/`jsonlite` (see DESCRIPTION
  `Imports`).
- Network reads go through `httr::RETRY` (scrapers) or `url()` + `readRDS()`
  (loaders); loaders wrap reads in `try(..., silent = TRUE)`.
- `load_*` data-repo URLs are split between two GitHub orgs — older paths point
  at `tmking2002/softballR-data`, newer ones at `sportsdataverse/softballR-data`.
  Both resolve to the same data; keep new loaders pointed at the SDV org.
- Never add AI co-author trailers to commits or PRs.

## Gotchas

- **No tests, no `_pkgdown.yml`, no CI** in this repo. Don't claim a pkgdown
  site or a test gate exists; verify before adding workflow references.
- **Loaders depend on `softballR-data` being current.** `load_*` reads whatever
  `.RDS` the data repo last committed; stale data there = stale loader output.
  Live scrapers (`ncaa_*`/`espn_*`/`naia_*`) bypass that and hit the source site.
- **Season availability is loader-specific and hardcoded.** PBP/playerbox cover
  far fewer years than scoreboards. Trust the in-function season guards over the
  README prose.
- `stats.ncaa.org` scrapers parse positional HTML (`grep` on `<tr id=`, fixed
  row offsets like `loc + 75`) and break when the NCAA site markup shifts.

## Reference

- `DESCRIPTION` — version, author, `Imports`.
- `NAMESPACE` — exported function list (roxygen-generated).
- `R/load_*.R` — exact season guards + data-repo URLs per loader.
- Sibling data repo: `softballR-data` (build scripts + committed `.RDS`).
