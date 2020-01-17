xx <- datasets::iris
xx$Sepal.Width <- as.character(as.integer(xx$Sepal.Width))
xx$Petal.Length <- as.factor(as.integer(xx$Petal.Length))

while (TRUE) { FSelectorRcpp::information_gain(Species ~ .,xx, threads = 8)}
