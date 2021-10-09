library(tidyverse)
library(benelib)

theme_set(theme_bene())


mario <- read_rds("01 Data/world-records.rds")

mario %>% 
  ggplot(aes())