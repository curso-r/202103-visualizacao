library(tidyverse)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

saveRDS(transit_cost, "transit_cost.RDS")
