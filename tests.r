library(dslabs)
library(dplyr)

df<-data.frame(xy = create_entry(0,1))
last_entry <- df$xy[length(df$xy)]

xy <- as.numeric(sub('.([0-9]).([0-9])',c('\\1','\\2'), last_entry))
xy