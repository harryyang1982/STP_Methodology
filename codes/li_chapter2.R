library(tidyverse)
pwt7 <- read.csv("datasets/pwt70_w_country_names.csv",
                 header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

pwt7

View(pwt7)

pwt7 %>% 
  head(1)

pwt7 %>% 
  tail(1)

pwt7 %>% 
  dim()

pwt7 %>% 
  names()

str(pwt7)

par(mfrow=c(2,2))

hist(pwt7$rgdpl)
boxplot(pwt7$rgdpl)
qqnorm(pwt7$rgdpl)

pwt7$POP[5]

pwt7$POP[pwt7$country == "Afghanistan" & pwt7$year == 1954]
pwt7 %>% 
  filter(country == "Afghanistan" & year == 1954) %>% 
  .$POP

pwt7[5, 4]

pwt7[pwt7$country == "Afghanistan" & pwt7$year == 1954, "POP"]


pwt7 <- pwt7[order(pwt7$country, pwt7$year), ]
pwt7
pwt7 <- pwt7[order(pwt7$country, -pwt7$year), ]
pwt7


pwt7new <- pwt7 %>% select(country, year, rgdpl)

pwt7_tmp1 <- pwt7 %>% select(isocode, year, rgdpl)
pwt7_tmp2 <- pwt7 %>% select(isocode, year, openk)

pwt7_m <- merge(pwt7_tmp1, pwt7_tmp2, by = c("isocode", "year"))
pwt7_m

pwt7_tmp1 %>% 
  left_join(pwt7_tmp2, by = c("isocode", "year"))

pwt7_ip <- pwt7 %>% 
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000) & country %in% c("India", "Pakistan")) %>% 
  select(country, year, rgdpl)
pwt7_ip

pwt7_ip %>% 
  pivot_wider(names_from = country, values_from = rgdpl)

pwt7_ip2 <- pwt7_ip %>% 
  pivot_wider(names_from = country, values_from = rgdpl)

pwt7_ip2 %>% 
  pivot_longer(cols=India:Pakistan, names_to = "country", values_to = "rgdpl") %>% 
  arrange(country, year)

pwt7_nc <- pwt7 %>% filter(isocode != "CH2")
dim(pwt7_nc)
pwt7 %>% filter(isocode == "CH2") %>% 
  .$year

pwt7_dup <- pwt7_ip %>% bind_rows(pwt7_ip %>% filter(country == "India"))
pwt7_dup

pwt7_dup[!duplicated(pwt7_dup %>% select("country", "year")), ]


pwt7 <- pwt7 %>% 
  mutate(income_group = case_when(rgdpl <= 1000 ~ "low_income",
                                  rgdpl > 1000 & rgdpl < 4000 ~ "low_middle",
                                  rgdpl > 4000 & rgdpl < 12000 ~ "up_middle",
                                  rgdpl > 12000 ~ "high_income"))

class(pwt7$income_group)

pwt7 <- pwt7 %>% 
  mutate(income_group = parse_factor(income_group, 
                                     levels = c("low_income", "low_middle", "up_middle", "high_income"), ordered = TRUE))

pwt7 %>% 
  count(income_group)
table(pwt7$income_group)

pwt7 <- pwt7 %>% 
  mutate(income_group2 = case_when(rgdpl <= 1000 ~ 1,
                                   rgdpl > 1000 & rgdpl < 4000 ~ 2,
                                   rgdpl > 4000 & rgdpl < 12000 ~ 3,
                                   rgdpl > 12000 ~ 4))

pwt7 <- pwt7 %>% 
  mutate(income_group2 = factor(income_group2,
                                      labels = c("low_income", "low_middle", "up_middle", "high_income")))

pwt7 <- pwt7 %>% 
  mutate(decade = case_when(year >= 1950 & year < 1960 ~ "1950s", 
                            year >= 1960 & year < 1970 ~ "1960s",
                            year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 ~ "2000s"))

pwt7 <- pwt7 %>% 
  arrange(country, year) %>% 
  mutate(rgdplead = lead(rgdpl, n = 1L, order_by = country),
         rgdplag = lag(rgdpl, n = 1L, order_by = country),
         growth = (rgdpl-rgdplag)/rgdplag)
  
pwt7_ag <- pwt7 %>% 
  group_by(decade, income_group) %>% 
  summarize(growth_di = mean(growth, na.rm=TRUE)) %>% 
  na.omit(income_group) %>% 
  arrange(income_group)

pwt7_ag

pwt7 <- pwt7 %>% 
  left_join(pwt7_ag, by = c("decade", "income_group"))

pwt7_cs <- pwt7 %>% 
  group_by(isocode, decade) %>% 
  summarize(growth_mean = mean(growth, na.rm = TRUE), openk_mean = mean(openk, na.rm = TRUE), pop_mean = mean(POP, na.rm = TRUE),
            growth_sd = sd(growth, na.rm = TRUE), openk_sd = sd(openk, na.rm = TRUE), pop_sd = sd(POP, na.rm = TRUE))

tail(pwt7_cs, 1)

pwt7 <- pwt7 %>% 
  rename(population = POP)

names(pwt7)

# recode names
pwt7 %>% 
  mutate(isocode = recode(isocode, "CH2"= "CHN"),
         country = recode(country, "China Version 1" = "China")) -> pwt72

pwt7 %>% 
  mutate(isocode = recode(isocode, "CH2"= "CHN"),
         country = recode(country, "China Version 1" = "China"))
pwt7$rgdpl[pwt7$rgdpl == -999] <- NA

library(Hmisc)
label(pwt7)

label(pwt7$isocode) <- "Penn World Table country code"
label(pwt7$rgdpl) <- "PPP Converted GDP Per Capita (Laspeyres) derived from growth rates of c, g, i, at 2006 constant prices"
label(pwt7$openk) <- "Openness at 2006 constant prices in percent"
label(pwt7$population) <- "Population (in thousands)"
label(pwt7$growth) <- "annual economic growth rate, based on RGDPL"

label(pwt7)