dt <- MASS::mvrnorm(
  300, Sigma = cbind(c(30, 20), c(30,20)), mu = c(150,150))

dt <- as.data.frame(dt)
dt[,1] <- as.factor(round(dt[,1], -1))


discFSRcpp <- discretize(V1 ~ ., dt)
drscWeka   <- RWeka::Discretize(V1 ~ ., dt, na.action = na.pass)

x <- na.omit(drscWeka$V2)
x <- gsub(x, pattern = "'\\(-?", replacement = "")
x <- gsub(x, pattern = "inf\\)'", replacement = "")
x <- gsub(x, pattern = "\\]'", replacement = "")
x <- unique(unlist(strsplit(x, split = "-")))
x <- as.numeric(x)
x <- x[!is.infinite(x)]

splitPoints <- attributes(discFSRcpp)$fsSplitPointsList$V2
splitPoints <- splitPoints[!is.infinite(splitPoints)]

sort(x)
sort(splitPoints)

testthat::expect_equal(fs$attr_importance, fsrcpp$importance)



