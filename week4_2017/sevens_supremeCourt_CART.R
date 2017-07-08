library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
stevens <- read.csv("dati/stevens.csv")

table(stevens$Reverse)

set.seed(3000)
spl <- sample.split(stevens$Reverse,SplitRatio = 0.7)

train <- subset(stevens,spl==TRUE)
test <- subset(stevens,spl==FALSE)

StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                      Respondent + LowerCourt + Unconst, data = train, method ="class", minbucket = 25 )
prp(StevensTree)

PredictCART <- predict(StevensTree,newdata = test, type = "class")

table(test$Reverse,PredictCART)
(41+71)/(41+36+22+71)

PredictROC <- predict(StevensTree,newdata = test)
PredictROC

pred <- prediction(PredictiROC[,2],test$Reverse)
perf <- performance(pred,"tpr","fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

StevensTree5 <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                       Respondent + LowerCourt + Unconst, data = train, method ="class", minbucket = 5 )
prp(StevensTree5)

StevensTree100 <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                        Respondent + LowerCourt + Unconst, data = train, method ="class", minbucket = 100 )
prp(StevensTree100)
