library(tidyverse)

esg <- read_csv('datasets/ESG2017.csv')
esg

ggplot(esg, aes(x = `ESG Disc Score:Y`,
                y = `Revenue T12M`)) + 
  geom_point(aes(color = `GICS SubInd Name`)) +
  geom_smooth(method = 'lm') +
  facet_wrap(~`GICS Sector`) +
  labs(title = "ESG Discovery Score and Revenue",
       subtitle = "Partitioned by GICS Sector and Colored by GICS Sub-Industry Name",
       caption = "Source from Prof. DSLee")


