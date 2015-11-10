library(readr)
library(dplyr)
library(ggplot2)

tri <- read_csv('/Users/Steve/Github/epa_tri/data/TRI_2013_US.csv')
names(tri)
colnames(tri)[36] <- "FUGITIVE_AIR"
colnames(tri)[37] <- "STACK_AIR"

tri_trim <- select(tri, TRI_FACILITY_ID, FACILITY_NAME, CITY, ST, PRIMARY_NAICS, FUGITIVE_AIR, STACK_AIR)
tri_trim

# Split the dataset into industry sectors by NAICS
by_naics <- group_by(tri_trim, PRIMARY_NAICS)
summarise(by_naics, mean=mean(FUGITIVE_AIR),sd=sd(FUGITIVE_AIR))

emissions <- summarise(by_naics,
                   count = n(),  # Count the number of facilities per NAICS
                   stack = mean(STACK_AIR, na.rm = TRUE),  #  Compute the average stack emission
                   fugitive = mean(FUGITIVE_AIR, na.rm = TRUE))  #  Compute the average fugitive emission
emissions <- filter(emissions, count > 100, stack < 10000)

ggplot(emissions, aes(stack, fugitive)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()
