######################################################################
#SAMPLING
#Slide 25
#how to produce random numbers in r
coinToss<-sample(0:1, size=5, replace=TRUE)
coinToss

coinToss<-sample(0:1, size=5, replace=TRUE)
coinToss #you will see that although we used the exact same code as above,
#the values are different
#that is because the numbers are generated randomly


######################################################################
#SAMPLING
#Slide 28
setwd("/Users/dasomlee/Desktop/Data/STP702") #macbook
getwd()
df<-read.csv("immprotestdata.csv", header=T, na.strings=c("","NA")) #If blank, make it NA

#Listwise deletion (removing rows with missing data)
df1<-na.omit(df)

#replace missing values with the mean
table(is.na(df$ParticipantAvg)) #first identify which variable has missing data
df$ParticipantAvg_mean<-df$ParticipantAvg
df$ParticipantAvg_mean[is.na(df$ParticipantAvg_mean)] <- mean(df$ParticipantAvg_mean, na.rm = TRUE)
table(is.na(df$ParticipantAvg_mean)) #no missing data

#Bayesian imputation
#For reference only - no need to know this right now
install.packages("Hmisc")
require(Hmisc)
set.seed(100)
df$Income_median
#you can use aregImpute function too in Hmisc, but for this data, that function did not work
imputed_ParticipantAvg<-transcan(~~ParticipantAvg+Frequency+Income_median+
                                     Hispanic+NativityForeign+NativityBothParentsFB+UnionMember, data=df, n.impute=5)
completedata<-data.frame(impute.transcan(imputed_ParticipantAvg, imputation=1, data=df, list.out=T, 
                                         pr=F, check=F))

table(is.na(completedata))

######################################################################
#SAMPLING
#Slide 29
setwd("/Users/dasomlee/Desktop/Data/STP702") #macbook
getwd()
Cat<-read.csv("Knightley_missingdata.csv", header=T, na.strings=c("","NA")) #If blank, make it NA
table(is.na(Cat))

Cat1<-na.omit(Cat) #listwise deletion

#replace missing values with the median
table(is.na(Cat$Weight)) #first identify which variable has missing data
Cat$Weight_median<-Cat$Weight
Cat$Weight_median[is.na(Cat$Weight_median)] <- mean(Cat$Weight_median, na.rm = TRUE)
table(is.na(Cat$Weight_median))

######################################################################
#SAMPLING

