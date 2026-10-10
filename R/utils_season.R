# Season-range check shared by the release loaders. The floor is the first
# season the softballR-data release ships for that asset; the ceiling is the
# current calendar year, so a new season never needs a code change and an
# unpublished season fails at the download, not here
# (rules/college_softball.yaml#college_softball-2016-softballr-player-box-release-floor).
.check_softball_season <- function(season, floor, ceiling = as.integer(format(Sys.Date(), "%Y"))) {
  if (!is.numeric(season) || length(season) == 0 || anyNA(season) ||
      any(season < floor | season > ceiling)) {
    stop(sprintf("Invalid season: %s. Valid seasons are %d-%d.",
                 paste(season, collapse = ", "), as.integer(floor), as.integer(ceiling)),
         call. = FALSE)
  }
  invisible(season)
}
