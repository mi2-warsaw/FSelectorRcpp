test_that("Discretize transformer returns the same result on the same set", {

  iris$extra <- iris$Sepal.Length +
    runif(nrow(iris), min = 0, max = 0.1)
  disc <- discretize(Species ~ ., iris)
  x <- discretize_transform(disc, iris)
  expect_true(all(mapply(function(x, y) x == y, disc, x)))
})

test_that("FsDiscretizeTransformer returns the same result", {

  disc <- discretize(Species ~ ., iris)
  dtran <- extract_discretize_transformer(disc)

  expect_equal(
    discretize_transform(disc, iris),
    discretize_transform(dtran, iris)
  )
})

test_that("Print FsDiscretizeTransformer", {
  disc <- discretize(Species ~ ., iris)
  invisible(capture.output(print(extract_discretize_transformer(disc))))
})

test_that("Test drop columns", {

  x <- iris
  x["xx"] <- 1
  disc <- suppressWarnings(discretize(Species ~ ., x, all = TRUE))

  # there's no split points - remove column by default
  expect_equal(colnames(discretize_transform(disc, x)), colnames(iris))

  # keeps coolumn full of NAs
  res <- discretize_transform(disc, x, dropColumns = FALSE)
  expect_equal(res$xx, rep(NA, nrow(x)))

  disc <- discretize(Species ~ Sepal.Length, x, all = FALSE)

  # discretize only one column and leaves all others untouched
  xx <- discretize_transform(disc, x)
  expect_equivalent(xx$Sepal.Length, disc$Sepal.Length)
  expect_equivalent(xx[-1], x[-1])

  # leaves only columns present in disc
  expect_equivalent(
    discretize_transform(disc, x, dropColumns = TRUE),
    disc
  )
})
