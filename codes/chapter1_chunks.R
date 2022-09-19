install.packages("UsingR")

library(tidyverse)
library(UsingR)

2 + 2

(1 + 3 + 2 + 12 + 8 ) / 5

( (2 - 1) ^ 2 + (1 - 3)  ^2) ^ (1/2)

x <- 8
y <- x^2 - 2*x + 1
y

x <- pi
sin(x)
sqrt(x)

log(x)

log(x, 10)

log(x, 2)

x <- c(74, 122, 235, 111, 292)
x

mean(x)
sum(x)/length(x)

x + x
sqrt(x)

x - mean(x)

mean(x, trim = 0.5)
median(x)

### Generic Functions
x <- c(74, 122, 235, 111, 292)
y <- c(T, F, T, T)
summary(x)

summary(y)

ls()
x <- c(74, 122, 235, 111, 292)
str(x)

rm(x)
rm(list=ls())
remove(list=ls())

data(rivers)
rivers

require("HistData")
head(Cavendish)

str(Cavendish)


1 + 2 * (3 + 4)
4 **3 + 3 **2 + 1

2^3^4
8^4

2^(3^4)
(1 + 2 * 3^4) / (5/6 - 7)
(0.25 - 0.2) / sqrt(0.2 * (1 - 0.2) / 100)

data(rivers)
rivers

exec.pay

names(Orange)
mean(Orange$age)
