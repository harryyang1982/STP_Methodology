
#slide 9
#this is how you create a truth table
library (QCA)
data(LC) #loading the LC data
head(LC)
#Here you can see that BE and CZ are the same (all values are 1 for all variables). This means in the truth table,
#their combination will also be the same

truthTable(LC, outcome = "SURV", complete=T, show.cases=T)
#column OUT = 1 is the complete inclusion, 0 is otherwise. 
#Now we see that at least for BE, CZ, NL, and UK, the five conditions (variables) are all important in causing the outcome
#For GR, PT, ES, all five conditions are not present, which causes the outcome to be 0. 

truthTable(LC, outcome = "SURV", complete=T, show.cases=T, sort.by="incl, n+")
#Now it is sorted more nicely (sorted based on incl and n is increasing)

############################################################3

#slide 11
data(LF)
?LF
#Read the variables and see what they mean

ttrows <- apply(LF[, 1:5], 2, function(x) as.numeric(x > 0.5))
#we are only looking at the first four, because W is the outcome variable
#we are arbitraily setting .5 as the cut off point
rownames(ttrows) <- rownames(LF)
ttrows
#Now the fuzzy set truth table looks like the crisp set truth table 

#This functino also gives you the same truth table - but with slightly more info
#use this instead of the ttrows
ttLF<-truthTable(LF, outcome = "SURV", incl.cut = 0.8, show.cases = TRUE)
ttLF

#you can also do this to find more info about a specific case
#For example, the 6th row, AIMU values are 0101. So, that is ~A*I*~M*U
#remember, ~ means !

#Let's try the truthtable with the LF data
truthTable(LF,outcome='SURV', incl.cut=.8)
#or you can do this
truthTable(LF, outcome = "~SURV", incl.cut = 0.8)

#or you can specify two thresholds. This can be interpreted by the greyzone, where you are uncertain
#This is useful where there are contradictions

truthTable(LF, outcome = "SURV", conditions = "DEV, URB, LIT, IND, STB", complete = FALSE, show.cases = TRUE, incl.cut = 0.8)
truthTable(LF, outcome = "~SURV", conditions = "DEV, URB, LIT, IND, STB", complete = FALSE, show.cases = TRUE, incl.cut = 0.8)
#have a look at row 24. For ~SURV, it is too small with the incl of .495
#For SURV, it is still too small with a value of .709
#It has to be one or the other - this is ambiguous and contradictory

truthTable(LF, outcome = "SURV", incl.cut = c(0.8, 0.6))
#here the incl values between .6 and .8 are considered uncertain, and are marked with C (contradiction)
#The incl is the consistency score that MUST BE reported when you are writing your findings table


############################################################
#slide 15

#You can also use the minimize function
tt <- truthTable(LF, outcome = "SURV", incl.cut = c(0.8, 0.6))
minimize(tt, details = TRUE, show.cases = TRUE)

tt_neg <- truthTable(LF, outcome = "~SURV", incl.cut = c(0.8, 0.6))
minimize(tt_neg, details = TRUE, show.cases = TRUE)
