library(dslabs)
library(dplyr)
data(olive)
head(olive)

with(olive, boxplot(palmitic~region))