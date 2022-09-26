# making functions

pounds <- function(kilos){
  return(2.20462 * kilos)
}

pounds(80)

fahrenheit_to_celsius <- function(tempF) {
  return((tempF - 32) * 5 / 9)
}

fahrenheit_to_celsius(32)
fahrenheit_to_celsius(212)

#Example
# library(psy)
# library(stargazer)
# library(plyr)

pow <- function(x, y) {
  result <- x ^ y
  print(stringr::str_c(x, " raised to the power ", y, " is ", result))
}

pow(3, 7)

# Data Cleaning
surveydata <- data.frame(gender = c(1,2,2,2, 9999, 8888, NA, 1, 1, NA),  #In surveys, 9999 usually means missing answer, 8888 means that the respondent said "don't know"
                         education = c(1:4, 1, "NA", 3, 2, "NA", 2), 
                         climateChangeBelief = c("Yes", "Yes", "No", "Yes",  "yes", "no", "Nooo", "Don't Know", "Don'tCare", NA),
                         energyTransitionBelief = "",
                         recycle = NA)
surveydata

library(tidyverse)
surveydata %>% 
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female",
                            TRUE ~ NA_character_),
         education = case_when(education == 1 ~ "didn't finish high school",
                               education == 2 ~ "high school graduate",
                               education == 3 ~ "bachelor's graduate",
                               education == 4 ~ "obtained advanced degree",
                               TRUE ~ NA_character_))

surveydata %>% 
  mutate(gender = case_when(gender %in% c(1, 2) ~ gender,
                            TRUE ~ NA_real_),
         education = case_when(education == 1 ~ "didn't finish high school",
                               education == 2 ~ "high school graduate",
                               education == 3 ~ "bachelor's graduate",
                               education == 4 ~ "obtained advanced degree",
                               TRUE ~ NA_character_))

surveydata$education_text <- ifelse(surveydata$education == 1, "didn't finish high school",
                                    ifelse(surveydata$education == 2, "high school graduate",
                                           ifelse(surveydata$education == 3, "bachelor's graduate",
                                                  ifelse(surveydata$education == 4, "obtained advanced degree", NA))))
surveydata %>% 
  mutate(gender_new = as.integer(recode(gender, '1'='0', '2'='1')),
         education = as.integer(education))

surveydata

survey1 <- surveydata %>% 
  mutate(gender = case_when(gender %in% c(1, 2) ~ gender,
                            TRUE ~ NA_real_),
         education = case_when(education == 1 ~ "didn't finish high school",
                               education == 2 ~ "high school graduate",
                               education == 3 ~ "bachelor's graduate",
                               education == 4 ~ "obtained advanced degree",
                               TRUE ~ NA_character_)) %>% 
  mutate(climateChangeBelief = tolower(climateChangeBelief),
         climateChangeBelief = ifelse(str_detect(climateChangeBelief, "^yes"), "yes", 
                                      ifelse(str_detect(climateChangeBelief, "^no"), "no", NA))) %>% 
  unique()

survey1         

# Basic Plots
barplot(mtcars$mpg, main = 'mpg', xlab = 'value', horiz=TRUE)

library(tidyverse)
df <- read_csv("datasets/knightley.csv")
df

barplot(df$Activity, main = "My Baby's Activity Rate", xlab = 'Active', horiz=TRUE)

object <- table(df$Activity, df$Weight)

barplot(object, main = 'Weight over Activity Rate', xlab = 'Weight', col = c("skyblue1", "snow3", "orchid1"), legend=rownames(object))

barplot(object, main = 'Weight over Activity Rate', xlab = 'Weight', col = c("skyblue1", "snow3", "orchid1"), legend=rownames(object),
        horiz=TRUE)

barplot(object, main="My Baby's Weight and Activity Rate", xlab="Weight",
        col=c("tomato3", "darkslategray4", "gold2"), legend=rownames(object),
        args.legend=list(x="topright", inset=c(.85,0)))

# Histogram
df <- read_csv("datasets/immprotestdata.csv")
df

hist(df$Frequency)
hist(df$Frequency, xlab = "Frequency", main = "Frequency", xlim = c(1, 12), breaks=6)

stripchart(df$Frequency)

stripchart(df$UnionMember, method='stack')
stripchart(df$UnionMember, method='jitter')

stripchart(df$UnionMember, method='jitter', xlab = 'Union Member')
stripchart(df$UnionMember, method='stack', xlab = 'Union Member')

stripchart(df$UnionMember, method='stack', xlab = 'Union Member', vertical = TRUE)
stripchart(df$UnionMember, method='jitter', xlab = 'Union Member', vertical = TRUE)

stripchart(list(Union = df$UnionMember, NotUnion = df$UnionNotMember),
           col = c("orange", "red"))
