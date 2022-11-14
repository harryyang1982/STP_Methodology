library(tidyverse)
library(QCA)

#5 or 6 references are needed.
# use the dataset LR
df <- as_tibble(LR)
df

df %>% 
  arrange(DEV)

sort(LR$DEV)

Xplot(LR$DEV, at = pretty(LR$DEV, cex = .8))
Xplot(LR$DEV, at = pretty(LR$DEV, cex = .8), jitter=TRUE)

findTh(LR$DEV)

calibrate(LR$DEV, type="crisp", thresholds = 550)
#for the crisp data, we just need to specify the type and thresholds

#This will also give you the exact same result
recode(LR$DEV, rules = "lo:550 = 0; else = 1")

findTh(LR$DEV, n=2)

calibrate(LR$DEV, type="crisp", thresholds = "550, 850")
#now you have three groups, low developed, middle income, high income

#Here is the recode function in case you want to use this instead
recode(LR$DEV, rules = "lo:550 = 0; 551:850 = 1; else = 2")

recode(LR$DEV, cuts = "350, 550, 850", values = "0, 0.33, 0.66, 1")
#it needs to be between 0 and 1. What values we assign between 0 and 1 is up to the researcher

#I am creating a dataset 
set.seed(12345)
height <- rnorm(n = 100, mean = 170, sd = 10)
range(height)

Xplot(height, jitter = F, cex = 0.8)
Xplot(height, jitter = T, cex = 0.8)

#What do the plots show us?

#to use the function calibrate
#increasing calibration of height
height_inc <- calibrate(height, type="fuzzy", logistic=T, thresholds = "e=165, c=175, i=185") #if logistic=F, then the function returns linear s-shapred or bell shaped distributions 
height_inc
# exclusion
# cross point
# inclusion
?calibrate

df1 <- as_tibble(height)
df1

#decreasing calibration of height
height_dec <- calibrate(height, type="fuzzy", logistic=TRUE, thresholds = "i=165, c=175, e=185")
#here i comes before e 

plot(height, height_inc, main = "Set of tall people", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, height_dec, main = "Set of short people", xlab = "Raw data",
     ylab = "Calibrated data")

#Here depending on people's height, people have different inclusion scores
#again, noting that the threshold needs to account for cultural context 
#(e.g., the average height may need to be changed depending on gender/country)

#slide 61

triang <- calibrate(height, thresholds = "e1=155, c1=165, i1=175, i2=175, c2=185, e2=195")
trapez <- calibrate(height, thresholds = "e1=155, c1=164, i1=173, i2=177,c2=186, e2=195")
bellsh <- calibrate(height, thresholds = "e1=155, c1=165, i1=175, i2=17, c2=185, e2=195", below = 3, above = 3)
trabel <- calibrate(height, thresholds = "e1=155, c1=164, i1=173, i2=177, c2=186, e2=195", below = 3, above = 3)

par(mfrow=c(2, 2))

plot(height, triang, main = "Triangle looking plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, trapez, main = "Trapeze looking plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, bellsh, main = "Kinda bell shaped but not really plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, trabel, main = "Now this is a bell shpaed plot plot", xlab = "Raw data",
     ylab = "Calibrated data")

#you need to plot your calibration to make sure that it is bell curved and not other shapes because skewness can lead to distorted data

#slide 65

inc <- c(40110, 34400, 25200, 24920, 20060, 17090, 15320, 13680, 11720,
         11290, 10940, 9800, 7470, 4670, 4100, 4070, 3740, 3690, 3590,
         2980, 1000, 650, 450, 110) #this is supposed to be Ragin's data on per capita income 

incr <- recode(inc, cuts = "1000, 4000, 5000, 10000, 20000", # you can use theories to identify your thresholds 
               values = seq(0, 1, by = 0.2)) #this matches the table in slide 63

indirect_inc <- calibrate(inc, method = "indirect",
                          thresholds = "1000, 4000, 5000, 10000, 20000")
indirect_inc #too difficult to read, let's just get three decimals
round(indirect_inc, 3)
