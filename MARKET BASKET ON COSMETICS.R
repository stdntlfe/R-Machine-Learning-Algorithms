library(arules)
library(arulesViz)

PLOTmydata= read.csv(file.choose(),sep=',',colClasses ="factor")
mydata=mydata[,-1]
str(mydata)
View(mydata)
head(mydata)

rules=apriori(mydata)
summary(rules)

#REDUCE TO SMALLER NUMBER OF RULES

rules=apriori(mydata,parameter=list(minlen=2,maxlen=3,supp=.7))
inspect(rules)

#FINDING INTERESTING RULES

rules=apriori(mydata,parameter=list(minlen=2,maxlen=3,conf=.7),appearance = list(rhs=c("Foundation=1"),default="lhs"))
inspect(rules)

#VISUALIZING RULES
plot(rules)
plot(rules,method="grouped")
plot(rules,method="graph",control=list(type="items"))

rules=apriori(mydata,parameter=list(minlen=2,maxlen=3,supp=.1,conf=.5),appearance = list(rhs=c("Foundation=1"),lhs=c("Bag=1","Blush=1","Nail.Polish=1","Brushes=1","Concealer=1","Eyebrow.Pencils=1","Bronze=1","Lip.Linear=1","Mascara=1","Eye.shadow=1","Lip.Gloss=1","Lipstick=1","Eyeliner=1"),default="none"))
