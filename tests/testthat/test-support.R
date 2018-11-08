context("Support functions")

test_that("Test character table", {
  x <- c("bb", "aa", "cc", "aa")
  stTable <- table(x)

  fsTable <- FSelectorRcpp:::fs_table1d(x)
  fsTable <- fsTable[order(names(fsTable))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test numeric table", {
  x <- c(5.0, 12.14, 5.0, 17)
  stTable <- table(x)

  fsTable <- FSelectorRcpp:::fs_table1d(x)
  fsTable <- fsTable[order(as.numeric(names(fsTable)))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test integer table", {
  x <- c(31:1, 31:1)
  stTable <- table(x)

  fsTable <- FSelectorRcpp:::fs_table1d(x)
  fsTable <- fsTable[order(as.numeric(names(fsTable)))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test factor table", {
  x <- c("bb", "aa", "cc", "aa")
  x <- as.factor(x)
  stTable <- table(x)

  fsTable <- FSelectorRcpp:::fs_table1d(x)
  fsTable <- fsTable[order(names(fsTable))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test numeric 2d table", {
  expect_equal(
    FSelectorRcpp:::fs_table_numeric2d(c(10, 10, 20), c(10, 10, 11)),
    2:1)
})

test_that("Test count levels", {
  xNum <- c(12, 23, 0.3)
  xChar <- c("A", "A", "B")
  xFac <- as.factor(xChar)

  expect_equal(FSelectorRcpp:::fs_count_levels(xNum), 3)
  expect_equal(FSelectorRcpp:::fs_count_levels(xChar), 2)
  expect_equal(FSelectorRcpp:::fs_count_levels(xFac), 2)
  expect_error(FSelectorRcpp:::fs_count_levels(list()))
})

test_that("Test order levels", {
  xNum <- c(12, 23, 0.3)
  xChar <- c("C", "A", "A", "B")
  xFac <- factor(xChar, levels = c("A", "B", "C"), ordered = TRUE)

  expect_equal(xNum[FSelectorRcpp:::fs_order(xNum) + 1], sort(xNum))
  expect_equal(xFac[FSelectorRcpp:::fs_order(xFac) + 1], sort(xFac))
  expect_error(FSelectorRcpp:::fs_order(list()))
})

test_that("Test order levels", {
  xNum <- c(12, 23, 0.3)
  xChar <- c("C", "A", "A", "B")
  xFac <- factor(xChar, levels = c("A", "B", "C"), ordered = TRUE)

  expect_equal(FSelectorRcpp:::fs_entropy1d(xNum),
               entropy::entropy(table(xNum)))
  expect_equal(FSelectorRcpp:::fs_entropy1d(xChar),
               entropy::entropy(table(xChar)))
  expect_equal(FSelectorRcpp:::fs_entropy1d(xFac),
               entropy::entropy(table(xFac)))

  expect_error(FSelectorRcpp:::fs_entropy1d(list()))
})

test_that("equal_freq_bin - errors", {
  expect_error(FSelectorRcpp:::equal_freq_bin(c("a", "b"), 2))
  expect_error(FSelectorRcpp:::equal_freq_bin(c(1, 2), 0))
})


test_that("Formula test", {
  fm <- to_formula(colnames(iris)[-5], "Species")
  fm2 <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

  expect_equal(fm, fm2)
})

test_that("get_signif_digits", {

  expect_equal(FSelectorRcpp:::get_signif_digits(0), 1)
  expect_equal(FSelectorRcpp:::get_signif_digits(123), 3)
  expect_equal(FSelectorRcpp:::get_signif_digits(12.234), 5)
  expect_equal(FSelectorRcpp:::get_signif_digits(1e7), 8)
  expect_equal(FSelectorRcpp:::get_signif_digits(1e7 + 0.1), 9)

  # round to max 6 places after .
  expect_equal(FSelectorRcpp:::get_signif_digits(1e7 + 0.123456), 8 + 6)
  expect_equal(FSelectorRcpp:::get_signif_digits(1e7 + 0.1234567), 8 + 6)

  # very big number
  expect_equal(FSelectorRcpp:::get_signif_digits(2.1 ^ 40), 19)

})
