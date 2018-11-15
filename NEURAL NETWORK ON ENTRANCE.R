library(neuralnet)

DATA=read.csv(file.choose(),sep=',',header=T)
str(DATA)

#NORMALIZATION
DATA$gre=(DATA$gre-min(DATA$gre))/(max(DATA$gre)-min(DATA$gre))
DATA$gpa=(DATA$gpa-min(DATA$gpa))/(max(DATA$gpa)-min(DATA$gpa))
DATA$rank=(DATA$rank-min(DATA$rank))/(max(DATA$rank)-min(DATA$rank))

#DATA PARTITION
set.seed(124)
ind= sample(2,nrow(DATA),replace=T,prob=c(0.7,0.3))
TRAIN_NEU=DATA[ind==1,]
TEST_NEU=DATA[ind==2,]
TRAIN_NEU
NEURALMODEL=neuralnet(admit~gre+gpa+rank,data=TRAIN_NEU,hidden=1,err.fct="ce",linear.output=FALSE)

plot(NEURALMODEL)

#PREDICTION

(PREDICTION=compute(NEURALMODEL,TEST_NEU[,-1]))
(head(PREDICTION$net.result))
head(TRAIN_NEU[1,])


