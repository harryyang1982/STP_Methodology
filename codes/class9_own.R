library(QCA)
library(tidyverse)

?LR
LR

with (LC, table(SURV, DEV))
View(LR)

df <- tibble(LR)
df

with(LC, table(SURV, DEV))
#calculating the necessity inclusion for the set corresponding to the condition DEV equal to 2 can be done with 
with(LM, sum(DEV == 2 & SURV) / sum(SURV))
#the value .625 means = there are 5 cases for DEV equal to 2 where SURV is present,
#but that does not mean DEV equal to 2 is present for ALL cases where SURV is present. 
#There are 3 other cases when DEV is equal to 1
#So, the conclusion is that none of the multiple individual values of DEV are necessary for SURV

#you can also use the fuzzyand function to replace the & from the binary crisp version
#And also can remove & in the multi value crisp version
with(LC, sum(fuzzyand(DEV, SURV)) / sum(SURV))
with(LM, sum(fuzzyand(DEV == 2, SURV)) / sum(SURV))

tbl <-with (LC, table(SURV, LIT))
tbl

#now we test for trivialness 
fisher.test(tbl)
#H0: the rows and the columns are independent
#H1: the rows and the columns are interdependent
#Here the p value is 0.03595 - 
#because it is less than .05, we reject the null hypothesis of independence between row sna dcolumns 
#we are using the 95% confidence level 

table(tbl)

#We can also do
#pof stands for parameters of fit
pof("LIT <- SURV", data=LC) #doesn't work for multi value
#inclN is the same as the value that we would have gotten using the same code as above
with (LC, table(LIT, SURV))
with(LC, sum(LIT&SURV)/sum(SURV))

#RoN refers to the Relevance of Necessity 
#Lower RoN = the condition is trivial
#Higher RON = the condition is relevant 

#CovN refers to the Necessary coverage
#Lower CovN = the condition has low coverage
#Higher CovN = the condition has high coverage

#If we want to do the trivialness for multi value data,
with(LM, table(SURV, DEV))
pof("DEV{2} <- SURV", data = LM) #if you just want to look at DEV=2,
pof("DEV{1} + DEV{2} <- SURV", data = LM) #if you want to look at both DEV1 and DEV2
#expression means the expression of SOP (sum of products) - also known as the disjunctive normal form (e.g., A*D + B*C)

#What does inclN mean
#What does RoN mean? 

# practice with NF dataset

data(NF)
?NF

# Understanding the data
# Reading the help function of NF dataset shows that W is the dependent/outcome variable and there are other 4 variables, and there are 12 cases(observations).

NF

# NF is a fuzzy-set data, thus I'll conduct supserSubset for getting the inclusion threshold is 0.9 and RoN is 0.6
superSubset(NF, outcome = "W", incl.cut = 0.9, ron.cut = 0.6)
superSubset(NF, outcome = "W", incl.cut = 0.9, ron.cut = 0.6, relation = "sufficiency")
#superSubset outputs tables of '~A+~M'.
# inclusion is 0.956, RoN is 0.691, covN is 0.793

# get the PRI
# AON
pof("~A -> W", data = NF, relation = "necessity")
pof("W", "~A", data = NF, relation = "necessity")

# AOS
pof("W", "~U", data = NF, relation = "sufficiency")
pof("~U -> W", data = NF, relation = "sufficiency")

with(NF, sum(fuzzyand(A, W)) / sum(A))
pof("~A + ~M -> W", data = NF)


XYplot(NF$A, NF$W)
XYplot(NF$A, 1- NF$W)

XYplot(NF$M, NF$W)
XYplot(NF$M, 1- NF$W)

# finding
# Dealing with necessiity, Negated variable A & M has strong relatiships with outcome variable W. 
# Dealing with sufficiency, Negated variable U has strong relationships with outcome variable W.