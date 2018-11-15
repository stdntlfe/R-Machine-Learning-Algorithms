library(naivebayes)
library(dplyr)
library (ggplot2)
library(psych)

Entrance=read.csv(file.choose(),sep=",",header=TRUE)
str(Entrance)
summary(Entrance)

Entrance$admit=factor(Entrance$admit,levels=c(0,1),labels=c("No","YES"))
Entrance$rank=as.factor(Entrance$rank)

pairs.panels(Entrance[,-1])

Entrance %>% 
  ggplot(aes(x=gre,fill=admit))+geom_density(alpha=0.8,color="black")


set.seed(1234)
ind=sample(2,nrow(Entrance),replace=T,prob=c(0.8,0.2))
Entrance_train=Entrance[ind==1,]
Entrance_test=Entrance[ind==2,]


Entrance_Naive_model=naive_bayes(admit~.,data=Entrance_train)
Entrance_Naive_model
plot(Entrance_Naive_model)


p=predict(Entrance_Naive_model,Entrance_test,type="prob")

head(cbind(p,Entrance_test))


P1=predict(Entrance_Naive_model,Entrance_test)

tab1=table(P1,Entrance_test$admit)

sum(diag(tab1))/sum(tab1)

    