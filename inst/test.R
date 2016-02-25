species = iris$Species

iris2 = iris[1:4]

test_fnc(iris2, species)

library(RWeka)
library(microbenchmark)




microbenchmark(information_gain(Species ~ ., iris),
information.gain(Species ~ ., iris))


