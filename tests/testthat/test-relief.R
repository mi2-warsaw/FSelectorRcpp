context("Filter: Relief")

if (require("FSelector")) {
  test_that("Basic comparsion with FSelector", {
    fs <- withr::with_seed(123, FSelector::relief(Species ~ ., iris))
    fs2 <- withr::with_seed(123, relief(Species ~ ., iris))
    expect_equal(fs, fs2)
  })
}
