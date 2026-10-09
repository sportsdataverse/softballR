# Season validation is network-free: a season that passes the range check
# reaches the division check, so an "Invalid division" error proves the season
# was accepted without downloading anything.

this_year <- as.integer(format(Sys.Date(), "%Y"))

test_that("load_ncaa_softball_pbp accepts 2025 and rejects seasons outside the release range", {
  skip_on_cran()
  expect_error(load_ncaa_softball_pbp(2025, division = "D4"), "Invalid division")
  expect_error(load_ncaa_softball_pbp(this_year, division = "D4"), "Invalid division")
  expect_error(load_ncaa_softball_pbp(2020), "Valid seasons are 2021-")
  expect_error(load_ncaa_softball_pbp(c(2021, 2020)), "Valid seasons are 2021-")
  expect_error(load_ncaa_softball_pbp(this_year + 1), sprintf("Valid seasons are 2021-%d", this_year))
})

test_that("load_ncaa_softball_playerbox accepts 2025 and floors per division and category", {
  skip_on_cran()
  expect_error(load_ncaa_softball_playerbox(2025, "Hitting", division = "D4"), "Invalid Division")
  expect_error(load_ncaa_softball_playerbox(2016:2025, "Hitting", division = "D4"), "Invalid Division")
  expect_error(load_ncaa_softball_playerbox(2011, "Hitting"), "Valid seasons are 2012-")
  expect_error(load_ncaa_softball_playerbox(2014, "Pitching"), "Valid seasons are 2015-")
  expect_error(load_ncaa_softball_playerbox(2015, "Hitting", division = "D2"), "Valid seasons are 2016-")
  expect_error(load_ncaa_softball_playerbox(this_year + 1, "Hitting"), "Valid seasons are 2012-")
  expect_error(load_ncaa_softball_playerbox("2024", "Hitting"), "Invalid season")
})

test_that("ncaa_softball_rosters resolves 2024-2026 ids and rejects seasons outside the table", {
  skip_on_cran()
  expect_equal(.ncaa_softball_roster_ids[["2025"]], "16800")
  expect_equal(.ncaa_softball_roster_ids[["2024"]], "16540")
  expect_equal(.ncaa_softball_roster_ids[["2026"]], "17020")
  expect_equal(.ncaa_softball_roster_ids[["2023"]], "16361")
  expect_error(ncaa_softball_rosters(2025, "D4"), "Invalid division")
  expect_error(ncaa_softball_rosters(2011, "D1"), "Valid seasons are 2012-2026")
  expect_error(ncaa_softball_rosters(2027, "D1"), "Valid seasons are 2012-2026")
})
