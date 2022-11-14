#slide 11
?LR #always read the data description first 

#here we are using LC because it is the binary crisp set
with (LC, table(SURV, DEV))
#The table shows that there are 8 cases of SURV happening and for all of them, DEV is happening too 
#Just in case the formatting of the table confuses you, I have also copied the table onto the lecture slide (slide 77) all cleaned up 

#The table shows that 8 cases of SURV is happening 
#And for all 8, DEV is happening too 
#The inclusion of the outcome SURV in the causal condition DEV is then-
with(LC, sum(DEV&SURV)/sum(SURV))
#Here, & means that are are looking at the intersection between the binary values 

#############################################################
#slide 14

#we are going to use LM because it is multi value sets
with(LM, table (SURV, DEV))

#calculating the necessity inclusion for the set corresponding to the condition DEV equal to 2 can be done with 
with(LM, sum(DEV == 2 & SURV) / sum(SURV))
#the value .625 means = there are 5 cases for DEV equal to 2 where SURV is present,
#but that does not mean DEV equal to 2 is present for ALL cases where SURV is present. 
#There are 3 other cases when DEV is equal to 1
#So, the conclusion is that none of the multiple individual values of DEV are necessary for SURV

#############################################################
#slide 16

#we are using LF because it is calibrated to fuzzy sets
#fuzzyand function allows the intersection of fuzzy intersection 
with(LF, sum(fuzzyand(DEV, SURV)) / sum(SURV))

#you can also use the fuzzyand function to replace the & from the binary crisp version
#And also can remove & in the multi value crisp version
with(LC, sum(fuzzyand(DEV, SURV)) / sum(SURV))
with(LM, sum(fuzzyand(DEV == 2, SURV)) / sum(SURV))

#############################################################
#slide 22
#Let's have a look at LIT (level of literacy) and SURV
tbl <-with (LC, table(SURV, LIT))
tbl

#The condition LIT is a necessary condition, with a necessity inclusion of 1 for the outcome SURV
#WHY AM I SAYING THIS? REFER TO THE TABLE tbl
#Now we have identified their necessity relationship, we can test for trivialness by testing their independence

#now we test for trivialness 
fisher.test(tbl)
#Here the p value is 0.03595 - because it is less than .05, we reject the null hypothesis of independence between row sna dcolumns 
#we are using the 95% confidence level 

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
#CovS = sufficient coverage
#CovU = unique coverage

#If we want to do the trivialness for multi value data,
with(LM, table(SURV, DEV))
pof("DEV{2} <- SURV", data = LM) #if you just want to look at DEV=2,
pof("DEV{1} + DEV{2} <- SURV", data = LM) #if you want to look at both DEV1 and DEV2
#expression means the expression of SOP (sum of products) - also known as the disjunctive normal form (e.g., A*D + B*C)

#What does inclN mean
#What does RoN mean? 

#for fuzzy set
with(LF, sum(fuzzyand(LIT, SURV)) / sum(LIT))
pof("LIT <- SURV", data = LF)
#You can also use this functino to calculate RoN
with(LF, sum(1 - LIT) / sum(1 - fuzzyand(LIT, SURV)))

#You can also add a plot
XYplot(LIT, SURV, data = LF, jitter = TRUE, relation = "necessity")
text(LF$LIT, LF$SURV, row.names(LF), cex=0.6, pos=4, col="black")

#############################################################
#slide 24

#this function gives you an overview 
superSubset(LF, outcome = "SURV", incl.cut = 0.9)
#default argument: relation="necessity" 
#you can also use "sufficiency" - which we will discuss later
#~ is the same as ! 

#to narrow things further
superSubset(LF, outcome = "SURV", incl.cut = 0.9, ron.cut = 0.6)

#############################################################
#slide 29

#CRISP BINARY
with(LC, sum(DEV & SURV) / sum(DEV))
#the necessity test used SURV as the division variable
#Meaning: there are 8 cases where DEV and SURV are present and out of a total of 10 cases where DEV is present
#Sufficiency inclusion of .8 is high enough to conclude the level of development DEV is sufficient 

#MULTI VALUE SET
#you can also just specify if you are using the multi value set
data(LM)
with(LM, sum(DEV == 2 & SURV) / sum(DEV == 2))
#1 is great, it is sufficient for survivial of democracy if Dev =2
#If you want to double check

#OR, you can also do

pof("DEV", "SURV", data = LC, relation = "sufficiency")

LM[LM$DEV == 2, c(1, 6)] # all countries having a surviving democracy where the level of development is equal to 2

#What about DEV = 1?
with(LM, sum(DEV == 1 & SURV) / sum(DEV == 1))
#with the threshold of .8, .6 is not enough to indicate that there is a sufficiency for the outcome

#OR
pof("DEV{1} -> SURV", data = LM)

#FUZZY SET
# load the data if not previously loaded
data(LF)
# then
with(LF, sum(fuzzyand(DEV, SURV)) / sum(DEV))
#again, too low. 

#you can also use this function
pof("DEV -> SURV", data = LF)

#############################################################
#slide 31

X <- c(0.2, 0.4, 0.45, 0.5, 0.6)
Y <- c(0.3, 0.5, 0.55, 0.6, 0.7)

#Because X values are all lower than Y, we expect the inclusion score to reach the maximum
pof(X, Y, relation = "sufficiency") # "suf" is also accepted
#and it does!

#because the negation of Y should be the reverse of Y, we expect the inclusion value to be low
pof(X, 1 - Y, relation = "sufficiency")
#but that is not the case
#This is a logical fallacy, and it just doesn't make sense but it sometimes happens

#Now, we need to decide whether we want to say X as sufficient for Y or ~Y
#We use the PRI score - the one that has the higher product between the consistency score and the PRI is chosen
#~Y has the PRI of 0, so we can say that Y is a better fit compared to ~y

#We can also draw graphs to see if they are sufficient
XYplot(X, 1 - Y, relation = "sufficiency") #the direction is the opposite
XYplot(X, Y, relation = "sufficiency") #looks much better











