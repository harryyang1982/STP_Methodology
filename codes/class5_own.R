library(tidyverse)
library(nlme)
library(ggtext)

esg <- read_csv("datasets/ESG2017.csv")
esg

immprotest <- read_csv("datasets/immprotestdata.csv")
immprotest
df<-data.frame(
  x=c(1,1,3,5,5),
  y=c(3,8,1,3,7),
  name=c("A", "B", "C", "D", "E")
)

df
names(df)[names(df) == 'x....c.1..1..3..5..5.'] <- 'x'
names(df)[names(df) == 'y....c.3..8..1..3..7.'] <- 'y'
names(df)[names(df) == 'label....c..A....B....C....D....E..'] <- 'name'

df

p <- ggplot(df, aes(x, y, label = name)) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 12))

p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity")



df
ggplot(Oxboys, aes(x = age, y = height, group = Subject)) +
  geom_point() +
  geom_line()

ggplot(Oxboys, aes(x = age, y = height, color = Subject)) +
  geom_point() +
  geom_line()

ggplot(Oxboys, aes(age, height)) +
  geom_point() +
  geom_line()

ggplot(Oxboys, aes(x = age, y = height, group = Subject, color = Subject)) +
  geom_point() +
  geom_line()

ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE) #`geom_smooth()` using formula 'y ~ x'
#A smooth line for each boy

ggplot(Oxboys, aes(age, height)) + 
  geom_line(aes(group = Subject)) + #group=Subject is added so that we are providing the correct group
  geom_smooth(method = "lm", size=2, se = FALSE) #here size just changes how thick the smooth line is
#> `geom_smooth()` using formula 'y ~ x'

ggplot(Oxboys, aes(age, height)) + 
  geom_point(aes(colour = factor(Occasion))) + 
  labs(
    x = "Age", 
    y = "Height", 
    colour = "Occasion",
    title = "Age and Heigh grat",
    subtitle = "Source: nmle package"
  )

p <- ggplot(immprotest, aes(x = Hispanic, y = ParticipantAvg))
p + geom_point(aes(color = factor(HouseElection))) +
  labs(color = "House Electorate") +
  geom_smooth(method = "lm", aes(group = HouseElection))


p + geom_point(aes(color = factor(HouseElection))) +
  labs(color = "House Electorate") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(immprotest, aes(x = State, y = ParticipantAvg)) +
  geom_col(aes(fill = State)) +
  facet_wrap(~HouseElection)

ggplot(immprotest, aes(x = City, y = Hispanic)) +
  geom_col(aes(fill = City)) +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(angle = 90))

# Slide 24

ggplot(Oxboys, aes(age, height)) + 
  geom_point(aes(colour = factor(Occasion))) + 
  labs(
    x = "Age with *italics* and **boldface**", 
    y = "Height", 
    colour = "Occasion",
    title = "Age and Heigh grat",
    subtitle = "Source: nmle package"
  ) + theme(axis.title.x = ggtext:: element_markdown())

names(esg)
esg["ESG Disc Score:Y"][is.na(esg["ESG Disc Score:Y"])] <- 0
is.na(esg["ESG Disc Score:Y"])
esg["ESG Disc Score:Y"]

table(is.na(esg$`CEO Duality:Y...19`))

esg$`CEO Duality.Y_char` <- as.character(esg$`CEO Duality:Y...19`)
esg$`CEO Duality.Y_char`

y<-c(30, 171, 184, 201, 212, 250, 265, 270, 272, 289, 305, 
     306, 322, 322, 336, 346, 351, 370, 390, 404, 409, 
     411, 436, 437, 439, 441, 444, 448, 451, 453, 470, 
     480, 482, 487, 494, 495, 499, 503, 514, 521, 522, 
     527, 548, 550, 559, 560, 570, 572, 574, 578, 585, 
     592, 592, 607, 616, 618, 621, 629, 637, 638, 640, 
     656, 668, 707, 709, 719, 737, 739, 752, 758, 766, 
     792, 792, 794, 802, 818, 830, 832, 843, 858, 860, 
     869, 918, 925, 953, 991, 1000, 1005, 1068, 1441)
quantile(y)
summary(y)

boxplot(y)$out

quantile(y)[[2]]-1.5*IQR(y)
quantile(y)[[4]]+1.5*IQR(y)
quantile(y)[[2]]-3*IQR(y)
quantile(y)[[4]]+3*IQR(y)

logged_y <- log(y)

hist(logged_y)
boxplot(logged_y)
squared_y <- y^2
boxplot(squared_y)

ggplot(immprotest, aes(x = Income_median, y = ParticipantAvg)) +
  geom_point(aes(color = State)) +
  geom_smooth(method = 'lm', se=FALSE)
