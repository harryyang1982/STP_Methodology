library(tidyverse)

# Sampling

set.seed(1234)

coinToss <- sample(0:1, size=5, replace=TRUE)
coinToss

df <- read_csv("datasets/immprotestdata.csv")
df

# Listwise deletion (removing rows with missing data)
table(is.na(df))
df1 <- na.omit(df)
df1

# replace missing values with the mean
df$ParticipantAvg_mean <- df$ParticipantAvg
df$ParticipantAvg_mean[is.na(df$ParticipantAvg)] <- mean(df$ParticipantAvg, na.rm=TRUE)
df$ParticipantAvg_mean

# Bayesian imputation #multiple imputation
library(Hmisc)
set.seed(100)
df$Income_median

imputed_ParticipantAvg <- transcan(~~ParticipantAvg+Frequency+Income_median+
                                     Hispanic+NativityForeign+NativityBothParentsFB+UnionMember, data=df, n.impute=5)
?transcan

completedata <- data.frame(impute.transcan(imputed_ParticipantAvg, imputation=1, data=df, list.out=TRUE, pr=FALSE, check=FALSE))
imputed_ParticipantAvg

completedata
table(is.na(completedata))

