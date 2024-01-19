
source("scripts/tests/snapshot.R")

# take_snapshot_processed_data()

testthat::test_that("Metadata wb matching snapshots", {
  expect_match_snapshot("example_data_metadata.xlsx")
  expect_match_snapshot("Meststof proef WUR_metadata.xlsx")
})

# compare_with_snapshot("example_data_metadata.xlsx")
# compare_with_snapshot("Meststof proef WUR_metadata.xlsx")
