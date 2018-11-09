context("NA handling")

if(require("FSelector", quietly = TRUE)) {

  library(FSelector)

  test_that("NAs in information gain", {

    dtIris <- iris

    dtIris[1,1] <- NA
    dtIris[2,2] <- NA
    dtIris[3,5] <- NA

    fs <- FSelector::information.gain(Species ~ ., dtIris)
    fsrcpp <- suppressWarnings(FSelectorRcpp::information_gain(
      Species ~ .,
      dtIris
    ))

    expect_equal(fs$attr_importance, fsrcpp$importance)
  })

}
