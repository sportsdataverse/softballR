# Offline tests read rows copied verbatim from the published
# ncaa_softball_groups release assets out of a local file; the last test is
# the gated live smoke call against the release itself
# (set SOFTBALLR_LOAD_TESTS=1 to run it).

test_that(".read_groups_csv keeps contract dtypes and reads empty fields as NA", {
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    "league,season,team_id,team_id_source,team_name,subdivision_id,conference_id,division_id,source,sources_agree,notes",
    "ncaa_softball,2025,703,ncaa_org,Texas,ncaa_softball:d1,ncaa_softball:sec,,ncaa,,"
  ), f)
  x <- .read_groups_csv(f, .groups_col_classes$team_group_seasons)

  expect_type(x$team_id, "character")
  expect_equal(x$team_id, "703")
  expect_type(x$season, "integer")
  expect_type(x$division_id, "character")
  expect_true(is.na(x$division_id))
  expect_type(x$sources_agree, "logical")
  expect_true(is.na(x$notes))
})

test_that("a failed group download warns and returns the contract columns", {
  skip_on_cran()
  cols <- .groups_col_classes$groups
  expect_warning(x <- .read_groups_csv(tempfile(fileext = ".csv"), cols), "Failed to read")
  expect_equal(nrow(x), 0)
  expect_equal(colnames(x), names(cols))
})

test_that("group loaders build the release urls", {
  skip_on_cran()
  seen <- character()
  local_mocked_bindings(.read_groups_csv = function(url, cols) {
    seen <<- c(seen, url)
    as.data.frame(lapply(cols, vector, length = 0))
  })
  load_ncaa_softball_team_group_seasons(seasons = 2024:2025)
  load_ncaa_softball_team_group_seasons(seasons = TRUE)
  load_ncaa_softball_group_aliases()

  expect_equal(basename(seen), c(
    "ncaa_softball_team_group_seasons_2024.csv",
    "ncaa_softball_team_group_seasons_2025.csv",
    "ncaa_softball_team_group_seasons.csv",
    "ncaa_softball_group_aliases.csv"
  ))
  expect_true(all(grepl("/releases/download/ncaa_softball_groups/", seen, fixed = TRUE)))
})

test_that("load_ncaa_softball_team_group_seasons rejects seasons before 1982", {
  skip_on_cran()
  expect_error(load_ncaa_softball_team_group_seasons(seasons = 1981))
  expect_error(load_ncaa_softball_team_group_seasons(seasons = 2024.5))
})

test_that("load_ncaa_softball_team_group_seasons live: Texas and Oklahoma to the SEC in 2025", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("SOFTBALLR_LOAD_TESTS"), "1"),
              "Set SOFTBALLR_LOAD_TESTS=1 to run live load_* tests")
  x <- load_ncaa_softball_team_group_seasons(seasons = 2024:2025)
  expect_setequal(unique(x$season), c(2024L, 2025L))

  moved <- x[x$team_id %in% c("522", "703"), ]
  moved <- moved[order(moved$team_id, moved$season), ]
  expect_equal(moved$conference_id, rep(c("ncaa_softball:big-12", "ncaa_softball:sec"), 2))
})
