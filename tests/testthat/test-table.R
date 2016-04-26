
test_that("Test character table",
{
  x = c("bb", "aa", "cc", "aa")
  stTable = table(x)

  fsTable = fs_table1d(x)
  fsTable = fsTable[order(names(fsTable))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test numeric table",
{
  x = c(5.0, 12.14,5.0 ,17)
  stTable = table(x)

  fsTable = fs_table1d(x)
  fsTable = fsTable[order(as.numeric(names(fsTable)))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test integer table",
{
  x = c(31:1, 31:1)
  stTable = table(x)

  fsTable = fs_table1d(x)
  fsTable = fsTable[order(as.numeric(names(fsTable)))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})

test_that("Test factor table",
{
  x = c("bb", "aa", "cc", "aa")
  x = as.factor(x)
  stTable = table(x)

  fsTable = fs_table1d(x)
  fsTable = fsTable[order((names(fsTable)))]

  expect_equal(names(stTable), names(fsTable))
  expect_equal(as.integer(stTable), as.integer(fsTable))
})


