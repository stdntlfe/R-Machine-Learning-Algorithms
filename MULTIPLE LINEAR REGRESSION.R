ENTRANCE=read.csv(file.choose(),header=T)
str(ENTRANCE)
ENTRANCE$admit=as.factor(ENTRANCE$admit)
ENTRANCE$rank=as.factor(ENTRANCE$rank)


set.seed(1234)
ind=sample(2,nrow(ENTRANCE),replace=T, prob=c(0.8,0.2))
ENTRANCE_TRAIN=ENTRANCE[ind==1,]
ENTRANCE_TEST=ENTRANCE[ind==2,]

LOGISTICMODEL=glm(admit~ gre+gpa+rank, data= ENTRANCE_TRAIN,family=binomial)
summary(LOGISTICMODEL)

head(ENTRANCE_TEST)
LOGISTICMODEL=glm(admit~ gpa+rank, data= ENTRANCE_TRAIN,family=binomial)
summary(LOGISTICMODEL)

LOGISTICPREDICTION=predict (LOGISTICMODEL,ENTRANCE_TEST, type='response')
head(LOGISTICPREDICTION)
head(ENTRANCE_TEST)

y= Intercept +(Coeffiencet of GPA * GPA)+(1*RANK3 coefficient)
y= -4.317996+(1.0708*3.61)+(1*-1.2275)
y
exp(y)/(1+exp(y))

table(LOGISTICPREDICTION,ENTRANCE_TEST)
length(LOGISTICPREDICTION)
length(ENTRANCE_TEST)
