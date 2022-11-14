#Slide 39
#this function installs the very latest verion of the package
install.packages("QCA", repos = "dusadrian.r-universe.dev")
#if you are getting an error from line 3, use this one instead
install.packages("QCA")
library (QCA)

#we are going to use the data called "LR" in the QCA package
df<-data.frame(LR)
?LR

#############################################################
#slide 43

#the most important variable here is the DEV
sort(LR$DEV)
#Looking at the DEV variable, we need to divide the group into two 
#0 = not developed, 1 = developed

Xplot(LR$DEV, at =pretty(LR$DEV, cex=.8))
#this plot shows the distribution of DEV values

#You can also do 
Xplot(LR$DEV, at =pretty(LR$DEV, cex=.8), jitter=T)

#Also there is a function in the QCA package that lets you find the threshold
 findTh(LR$DEV)
 #this function is based on hierarchical clustering and the euclidean distance
 #To get more info, use hclustm? and distm?
 
 
#############################################################
#slide 47
 calibrate(LR$DEV, type="crisp", thresholds = 550)
#for the crisp data, we just need to specify the type and thresholds

 #This will also give you the exact same result
 recode(LR$DEV, rules = "lo:550 = 0; else = 1")
 
#############################################################
#slide 50
findTh(LR$DEV, n=2)
 
calibrate(LR$DEV, type="crisp", thresholds = "550, 850")
#now you have three groups, low income, middle income, high income
 
#Here is the recode function in case you want to use this instead
recode(LR$DEV, rules = "lo:550 = 0; 551:850 = 1; else = 2")

#############################################################
#slide 54

recode(LR$DEV, cuts = "350, 550, 850", values = "0, 0.33, 0.66, 1")
#it needs to be between 0 and 1. What values we assign between 0 and 1 is up to the researcher

#############################################################
#slide 60

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

#e=stands for complete EXCLUSION
#c=the cross point
#i=threshold for complete INCOLUSION in this set
#if we specify the e and i, it means the function will increase from left to right 
#with low raw values on the left, and high raw values on the right

#decreasing calibration of height
height_dec <- calibrate(height, thresholds = "i=165, c=175, e=185")
#here i comes before e 

plot(height, height_inc, main = "Set of tall people", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, height_dec, main = "Set of short people", xlab = "Raw data",
     ylab = "Calibrated data")

#Here depending on people's height, people have different inclusion scores
#again, noting that the threshold needs to account for cultural context 
#(e.g., the average height may need to be changed depending on gender/country)
 
#############################################################
#slide 61

triang <- calibrate(height, thresholds = "e1=155, c1=165, i1=175, i2=175, c2=185, e2=195")
trapez <- calibrate(height, thresholds = "e1=155, c1=164, i1=173, i2=177,c2=186, e2=195")
bellsh <- calibrate(height, thresholds = "e1=155, c1=165, i1=175, i2=17, c2=185, e2=195", below = 3, above = 3)
trabel <- calibrate(height, thresholds = "e1=155, c1=164, i1=173, i2=177, c2=186, e2=195", below = 3, above = 3)

plot(height, triang, main = "Triangle looking plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, trapez, main = "Trapeze looking plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, bellsh, main = "Kinda bell shaped but not really plot", xlab = "Raw data",
     ylab = "Calibrated data")

plot(height, trabel, main = "Now this is a bell shpaed plot plot", xlab = "Raw data",
     ylab = "Calibrated data")

#you need to plot your calibration to make sure that it is bell curved and not other shapes because skewness can lead to distorted data

#############################################################
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

#############################################################
#slide 77
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
#slide 80

#we are going to use LM because it is multi value sets
with(LM, table (SURV, DEV))

#calculating the necessity inclusion for the set corresponding to the condition DEV equal to 2 can be done with 
with(LM, sum(DEV == 2 & SURV) / sum(SURV))
#the value .625 means = there are 5 cases for DEV equal to 2 where SURV is present,
#but that does not mean DEV equal to 2 is present for ALL cases where SURV is present. 
#There are 3 other cases when DEV is equal to 1
#So, the conclusion is that none of the multiple individual values of DEV are necessary for SURV

#############################################################
#slide 82

#we are using LG because it is calibrated to fuzzy sets
#fuzzyand function allows the intersection of fuzzy intersection \
with(LF, sum(fuzzyand(DEV, SURV)) / sum(SURV))

#you can also use the fuzzyand function to replace the & from the binary crisp version
#And also can remove & in the multi value crisp version
with(LC, sum(fuzzyand(DEV, SURV)) / sum(SURV))
with(LM, sum(fuzzyand(DEV == 2, SURV)) / sum(SURV))

#############################################################
#slide 88
#Let's have a look at LIT (level of literacy) and SURV
tbl <-with (LC, table(SURV, LIT))
tbl

#The condition LIT is a necessary condition, with a necessity inclusion of 1 for the outcome SURV
#WHY AM I SAYING THIS? REFER TO THE TABLE tbl
#Now we have identified their necessity relationship, we can test for trivialness by testing their independence

#now we test for trivialness 
fisher.test(tbl)
#H0: the rows and the columns are independent
#H1: the rows and the columns are interdependent
#Here the p value is 0.03595 - 
#because it is less than .05, we reject the null hypothesis of independence between row sna dcolumns 
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
#slide 90

#this function gives you an overview 
superSubset(LF, outcome = "SURV", incl.cut = 0.9)
#default argument: relation="necessity" 
#you can also ue "sufficiency" - which we will discuss later
#~ is the same as ! 

#to nwrrow things further
superSubset(LF, outcome = "SURV", incl.cut = 0.9, ron.cut = 0.6)

#############################################################
#slide 95
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

LM[LM$DEV == 2, c(1, 6)] # all coutnreis having a surviving democracy where the level of development is equal to 2

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
#slide 96

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
#We use the PRI score - the one that has the higher product between the consistency score and the PIR is chosen
#~Y has the PRI of 0, so we can say that Y is a better fit compared to ~y

#We can also draw graphs to see if they are sufficient
XYplot(X, 1 - Y, relation = "sufficiency") #the direction is the opposite
XYplot(X, Y, relation = "sufficiency") #looks much better











