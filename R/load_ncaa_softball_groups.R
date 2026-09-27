# Loaders for the season-by-season division / conference reference tables
# published by sportsdataverse/sdv-reference-data to the sportsdataverse-data
# release tag ncaa_softball_groups. Schema: that repo's CONTRACT.md. The tag
# carries parquet + csv; the csv is read with the contract's column classes
# (ids stay character, "150" not 150; an empty field is a null).

# Column classes of the four {league}_groups tables, per CONTRACT.md.
.groups_col_classes <- list(
  groups = c(
    league = "character", group_id = "character", level = "character",
    first_season = "integer", last_season = "integer", notes = "character"
  ),
  group_seasons = c(
    league = "character", group_id = "character", season = "integer",
    level = "character", name = "character", short_name = "character",
    abbreviation = "character", parent_group_id = "character",
    n_teams = "integer"
  ),
  group_aliases = c(
    league = "character", group_id = "character", source = "character",
    source_id = "character", name_kind = "character", value = "character",
    valid_from = "integer", valid_to = "integer"
  ),
  team_group_seasons = c(
    league = "character", season = "integer", team_id = "character",
    team_id_source = "character", team_name = "character",
    subdivision_id = "character", conference_id = "character",
    division_id = "character", source = "character",
    sources_agree = "logical", notes = "character"
  )
)

# Read one release csv with the contract column classes. A file that fails to
# download warns and returns a zero-row frame carrying the contract schema.
.read_groups_csv <- function(url, cols) {
  tryCatch(
    suppressWarnings(utils::read.csv(url, colClasses = cols, na.strings = "",
                                     encoding = "UTF-8")),
    error = function(e) {
      warning("Failed to read <", url, ">: ", conditionMessage(e), call. = FALSE)
      as.data.frame(lapply(cols, vector, length = 0))
    }
  )
}

# Build the release URLs for one table (per-season files when `seasons` is a
# vector; `seasons = TRUE` reads the all-seasons file) and bind the reads.
.ncaa_softball_groups_loader <- function(table, seasons = NULL) {
  file_stem <- paste0("ncaa_softball_", table)
  if (!is.null(seasons) && !isTRUE(seasons)) {
    stopifnot(is.numeric(seasons),
              all(seasons >= 1982),
              all(seasons <= as.integer(format(Sys.Date(), "%Y"))),
              all(seasons == trunc(seasons)))
    file_stem <- paste0(file_stem, "_", seasons)
  }
  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    "ncaa_softball_groups/", file_stem, ".csv"
  )
  out <- lapply(urls, .read_groups_csv, cols = .groups_col_classes[[table]])
  dplyr::as_tibble(do.call(rbind, out))
}

#' Load NCAA softball groups (divisions and conferences)
#'
#' @description One row per NCAA softball group lineage (divisions I-III and
#'   their conferences), keyed by the SportsDataverse group id (e.g.
#'   `ncaa_softball:sec`). Published to the `ncaa_softball_groups` release tag
#'   on the [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
#'   by [sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
#'   Seasons are keyed by the spring year (2025 = the spring 2025 season).
#' @return A tibble with the following columns:
#'
#'    |col_name     |types     |description                                                                 |
#'    |:------------|:---------|:---------------------------------------------------------------------------|
#'    |league       |character |League key.                                                                 |
#'    |group_id     |character |SportsDataverse group id, `{league}:{slug}`; one id per lineage across renames. |
#'    |level        |character |Group level: `league`, `subdivision` or `conference`.                      |
#'    |first_season |integer   |First season with at least one member.                                      |
#'    |last_season  |integer   |Last season with at least one member.                                       |
#'    |notes        |character |Lineage decisions and source caveats.                                       |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_softball_groups())
#' }
load_ncaa_softball_groups <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .ncaa_softball_groups_loader("groups")
}

#' Load NCAA softball division and conference names and parents by season
#'
#' @description One row per NCAA softball group per season it existed, with
#'   the name, abbreviation and parent group **as of that season** (not
#'   today's). Published to the `ncaa_softball_groups` release tag on the
#'   sportsdataverse-data releases. Seasons are keyed by the spring year.
#' @return A tibble with the following columns:
#'
#'    |col_name        |types     |description                                                          |
#'    |:---------------|:---------|:--------------------------------------------------------------------|
#'    |league          |character |League key.                                                          |
#'    |group_id        |character |SportsDataverse group id, `{league}:{slug}`.                         |
#'    |season          |integer   |Season (spring year).                                                |
#'    |level           |character |Group level: `league`, `subdivision` or `conference`.               |
#'    |name            |character |Group name as of that season.                                        |
#'    |short_name      |character |Short name as of that season.                                        |
#'    |abbreviation    |character |Abbreviation as of that season.                                      |
#'    |parent_group_id |character |Parent group id as of that season (conference, subdivision, league). |
#'    |n_teams         |integer   |Member teams that season.                                            |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_softball_group_seasons())
#' }
load_ncaa_softball_group_seasons <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .ncaa_softball_groups_loader("group_seasons")
}

#' Load NCAA softball group aliases
#'
#' @description Every name and id a source uses for an NCAA softball division
#'   or conference, with the seasons it is valid for -- the crosswalk from
#'   stats.ncaa.org conference ids and names to SportsDataverse group ids.
#'   Published to the `ncaa_softball_groups` release tag on the
#'   sportsdataverse-data releases.
#' @return A tibble with the following columns:
#'
#'    |col_name   |types     |description                                                                     |
#'    |:----------|:---------|:-------------------------------------------------------------------------------|
#'    |league     |character |League key.                                                                     |
#'    |group_id   |character |SportsDataverse group id, `{league}:{slug}`.                                    |
#'    |source     |character |Source that uses the alias (e.g. `ncaa`, `sdv`).                                |
#'    |source_id  |character |The source's own id for the group (e.g. the NCAA conference id), when it has one. |
#'    |name_kind  |character |Alias kind: `name`, `short_name`, `abbreviation`, `slug` or `code`.            |
#'    |value      |character |The alias.                                                                      |
#'    |valid_from |integer   |First season the alias is valid (inclusive); `NA` = unbounded.                  |
#'    |valid_to   |integer   |Last season the alias is valid (inclusive); `NA` = unbounded.                   |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_softball_group_aliases())
#' }
load_ncaa_softball_group_aliases <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .ncaa_softball_groups_loader("group_aliases")
}

#' Load NCAA softball team division and conference memberships by season
#'
#' @description One row per NCAA softball team per season, with the division
#'   (`subdivision_id`) and conference it played in that season, e.g. Texas
#'   and Oklahoma moving from `ncaa_softball:big-12` (2024) to
#'   `ncaa_softball:sec` (2025). `team_id` is the stats.ncaa.org org id.
#'   Published to the `ncaa_softball_groups` release tag on the
#'   sportsdataverse-data releases, one file per season.
#' @param seasons A vector of 4-digit seasons (the spring year), or `TRUE` for
#'   every published season. (Min: 1982)
#' @return A tibble with the following columns:
#'
#'    |col_name       |types     |description                                                                  |
#'    |:--------------|:---------|:----------------------------------------------------------------------------|
#'    |league         |character |League key.                                                                  |
#'    |season         |integer   |Season (spring year).                                                        |
#'    |team_id        |character |stats.ncaa.org org id.                                                       |
#'    |team_id_source |character |Id system of `team_id` (`ncaa_org`).                                         |
#'    |team_name      |character |Team name as of that season.                                                 |
#'    |subdivision_id |character |SportsDataverse division group id (e.g. `ncaa_softball:d1`).                 |
#'    |conference_id  |character |SportsDataverse conference group id; `NA` where the level does not apply.    |
#'    |division_id    |character |SportsDataverse division-within-conference group id; `NA` when not used.     |
#'    |source         |character |Source the membership came from.                                             |
#'    |sources_agree  |logical   |Whether a second source agrees; `NA` when only one source covers the season. |
#'    |notes          |character |Notes.                                                                       |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_softball_team_group_seasons(seasons = 2025))
#' }
load_ncaa_softball_team_group_seasons <- function(seasons) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .ncaa_softball_groups_loader("team_group_seasons", seasons)
}
