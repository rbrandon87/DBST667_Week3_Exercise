#Exercise 3 - Brandon Russell
#Load necessary packages
install.packages("arules")
install.packages("arulesViz")
library("arules", lib.loc="~/R/win-library/3.5")
library("arulesViz", lib.loc="~/R/win-library/3.5")
#Load credit approval
credit <- read.csv(file = "CreditApproval.csv", header = TRUE, sep = ",")
#Verify information
head(credit)
str(credit)
summary(credit)
#Part2bi
#Replace missing values with mean value
credit$A2[is.na(credit$A2)]<-mean(credit$A2, na.rm=TRUE)
credit$A14[is.na(credit$A14)]<-mean(credit$A14, na.rm=TRUE)
apply(credit, 2, function (credit) sum(is.na(credit)))
#Perform discretization on int/num attributes to perform the Apriori method, and check work
credit$A2 <- discretize(credit$A2, "cluster", breaks = 6)
head(credit$A2)
credit$A3 <- discretize(credit$A3, "cluster", breaks = 6)
head(credit$A3)
credit$A8 <- discretize(credit$A8, "cluster", breaks = 6)
head(credit$A8)
credit$A11 <- discretize(credit$A11, "cluster", breaks = 6)
head(credit$A11)
credit$A14 <- discretize(credit$A14, "cluster", breaks = 6)
head(credit$A14)
credit$A15 <- discretize(credit$A15, "cluster", breaks = 6)
head(credit$A15)
str(credit)
