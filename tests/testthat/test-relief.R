context("Filter: Relief")

if (require("FSelector")) {
  test_that("Basic comparsion with FSelector", {
    fs <- withr::with_seed(123, FSelector::relief(Species ~ ., iris))
    fs2 <- withr::with_seed(123, .relief(Species ~ ., iris))

    fs3 <- withr::with_seed(123, relief(x = iris[-5], y = iris$Species))

    expect_equal(fs[[1]], fs2[[2]])
    expect_equal(fs[[1]], fs3[[2]])
  })
}
