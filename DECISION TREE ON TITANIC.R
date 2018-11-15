library(dplyr)
library(rpart)
library(rpart.plot)
library(pROC)


set.seed(679)
path <- 'https://tinyurl.com/titanicdataanalysis'
titanic <-read.csv(path)

head(titanic)
summary(titanic)
class(titanic)
str(titanic)
is.na(titanic)


Randomize <- sample(1:nrow(titanic))
titanic <- titanic[Randomize, ]
head(titanic)
clean_titanic <- titanic %>%
select(-c(home.dest, cabin, name, X, ticket)) %>% 
  #Convert to factor level
mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
na.omit()
str(clean_titanic)

ind=sample(2,nrow(clean_titanic),replace=T,prob=c(0.8,0.2))

titanic_train=clean_titanic[ind==1,]
titanic_test=clean_titanic[ind==2,]

dim(titanic_test)
dim(titanic_train)

titanic_decision_tree_model<- rpart(survived~., data = titanic_train, method = 'class')
rpart.plot(titanic_decision_tree_model, extra = 106)

titanic_predict=predict(titanic_decision_tree_model, titanic_test, type = 'class')
titanic_predict

titanic_predict_table=table(titanic_test$survived, titanic_predict)
titanic_predict_table


(titanic_performance=sum(diag(titanic_predict_table))/sum(titanic_predict_table))*100

titanic_predict_prob=predict(titanic_decision_tree_model, titanic_test, type = 'prob')
auc=auc(titanic_test$survived,titanic_predict_prob[,2])
plot(roc(titanic_test$survived,titanic_predict_prob[,2]))

