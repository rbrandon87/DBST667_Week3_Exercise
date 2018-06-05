options("scipen"=100, "digits"=4)
rm(bank)
#Read the CSV file.  Change the file path to the file location on your hard drive.
bank <- read.csv("Bank.csv")
View(bank)
#Remove id variable
bank$id<-NULL

#discretization
library("arules", lib.loc="~/R/win-library/3.3")
bank$age<-discretize(bank$age, "frequency", breaks=6)
bank$income<-discretize(bank$income, "frequency", breaks=6)
#factor
bank$children<-factor(bank$children)
summary(bank$children)
#Run the method with default parameters
rules<-apriori(bank)
rules
inspect(rules)
#different summort and confidence values
rules <- apriori(bank, parameter= list(supp=0.4, conf=0.7))
inspect(rules)
rules <- apriori(bank, parameter= list(supp=0.4, conf=0.7, minlen=2))
#display other measures
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), bank)
rules<-apriori(bank, parameter= list(supp=0.1, conf=0.8, minlen=2), appearance=list(rhs=c("pep=NO", "pep=YES"), default="lhs"))
inspect(rules)
#Sort the rules by lift
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
#Remove the redundant rules
rm(subset.matrix)
rm(redundant)
redundant <- is.redundant(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
redundant
which(redundant)
rules.pruned <- rules.sorted[!redundant]
summary(rules.pruned)
inspect(rules.pruned)
#Plot the rules
#install arulesViz package only once
install.packages("arulesViz")
library("arulesViz")
plot(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
plot(rules.pruned, method = "grouped")
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))

