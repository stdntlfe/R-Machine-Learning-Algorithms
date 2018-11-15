library(ggplot2)
library(e1071)
IRIS=iris
str(IRIS)
qplot(Petal.Length,Petal.Width,data=IRIS,color=Species)
SVMMODEL=svm(Species~.,data=IRIS)
SVMMODEL=svm(Species~.,data=IRIS,kernel="linear")
SVMMODEL=svm(Species~.,data=IRIS,kernel="polynomial")
SVMMODEL=svm(Species~.,data=IRIS,kernel="sigmoid")
summary(SVMMODEL)
plot(SVMMODEL,data=IRIS, Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
SVMPREDICTION=predict(SVMMODEL,IRIS)
(Tab=table(SVMPREDICTION,IRIS$Species))
(SVMPERFORMANCE=sum(diag(Tab))/sum(Tab)*100)
SVMPREDICTION=predict(SVMMODEL,IRIS)

#TUNING
set.seed(123)
TUNEDSVMMODEL=tune(svm,Species~.,data=IRIS,ranges=list(epsilon=seq(0.1,0.1),cost=2^(2:9)))
BESTSVMMODEL=TUNEDSVMMODEL$best.model
summary(BESTSVMMODEL)
SVMPREDICTION=predict(BESTSVMMODEL,IRIS)
(Tab=table(SVMPREDICTION,IRIS$Species))
(SVMPERFORMANCE=sum(diag(Tab))/sum(Tab)*100)
