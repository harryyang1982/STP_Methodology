library(QCA)

data(NF)

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

# Inclusing score / Consistency score => finding
pof("A", "W", data = NF)
# inclN: 0.779, 
# Relevance of N
# RoN: 0.582
# covN: 0.654

# PRI
pof("W", "A", data = NF, relation = "sufficiency")
# pri is 0.674

# unique coverage


# Dealing with necessiity, Negated variable A & M has strong relatiships with outcome variable W. 
# Dealing with sufficiency, Negated variable U has strong relationships with outcome variable W.