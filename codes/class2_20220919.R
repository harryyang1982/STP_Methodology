library(tidyverse)

df <- read_csv("datasets/immprotestdata.csv")

# NOIR: Nominal, Ordinal, Interval, Ratio

# Some statistical concepts
# Mean
# Median
# Mode
# Standard Deviation

mean(c(1, 1, 2, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 6))

median(c(1, 1, 2, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 6))


knightley <- read_csv("datasets/Knightley.csv")
knightley

summary(df)

# Actually practice with immprotestdata.csv

head(df)
tail(df)
colnames(df)
names(df)
colSums(is.na(df))
colMeans(is.na(df))

is.na(df)
table(is.na(df))
table(df$`Duration(Mean)`)
table(df$`Duration(Median)`)

mean(df$`Duration(Mean)`)
median(df$`Duration(Mean)`)
min(df$`Duration(Mean)`)
max(df$`Duration(Mean)`)
sd(df$`Duration(Mean)`)
str(df)
str(df$`Duration(Mean)`)
summary(df$`Duration(Mean)`)

df_IWFRonly <- subset(df, ImmigrantWorkersFreedomRide >0)
df_IWFRonly

df_IWFRonly2 <- df %>% filter(ImmigrantWorkersFreedomRide >0)
table(df_IWFRonly$ImmigrantWorkersFreedomRide)

df %>% 
  summarize(mean_metro = mean(Metcode),
            mean_state = mean(State),
            mean_nativity = mean(NativityForeign, na.rm=TRUE),
            median_nativity = median(NativityForeign, na.rm=TRUE),
            sd_nativity = sd(NativityForeign, na.rm=TRUE))

df %>% 
  group_by(State) %>% 
  summarize(max_income = max(Income_median)) %>% 
  arrange(-max_income)

df[which(df$Income_median == max(df$Income_median)), c("State", "Income_median")]

# number data
num_data <- c(3, 7, 2)
num_data

class(num_data)
str(num_data)
typeof(num_data)

# Factor
gender <- factor(c("female", "female", "male", "female", "male"))

levels(gender)
Region_fac <- as.factor(df$Region)
as_factor(df$Region)

str(Region_fac)

text <- c("test1", "test2", "test1", "test1")
text_factor <- as_factor(text)
str(text_factor)

text_factor2 <- factor(text_factor, levels = c("test2", "test1"))
text_factor2
str(text_factor2)

# importing data

Cat <- read_csv("datasets/Knightley.csv")
cat

summary(lm(Weight ~ Caloric_Intake + Sleep + Urination, data = cat))

table(is.na(cat))

cat$Foodfrequency  <- c(4,3,3,4,3,3,3,2,2,3,4,4,2,3,4,5,2,2,4)
cat

summary(lm(Weight ~ Caloric_Intake + Sleep + Urination + Foodfrequency, data = cat))
write.csv(cat, "datasets/cat.csv")

numbers <- 1:20
numbers


numbers <- c(1:7, 9:20)
numbers

rm(list=ls())

# df from scratch

fruits <- c("Apple", "Banana", "Orange", "Lemon")
price <- c(0.1, 0.55, 0.78, 0.4)
color <- c("red", "yellow", "orange", "yellow")
data_fruits <- data.frame(fruits, price, color)
data_fruits

# list
list_cat_fruits <- list(data_fruits, Cat)
list_cat_fruits

