library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)

Stevens <- read.csv("dati/stevens.csv")

table(Stevens$Reverse)

set.seed(3000)
spl <- sample.split(Stevens$Reverse,SplitRatio = 0.7)

Train <- subset(Stevens,spl==TRUE)
Test <- subset(Stevens,spl==FALSE)

StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                      Respondent + LowerCourt + Unconst, data = Train, method ="class", minbucket = 25 )
prp(StevensTree)

PredictCART <- predict(StevensTree,newdata = Test, type = "class")

table(Test$Reverse,PredictCART)
(41+71)/(41+36+22+71)

PredictROC <- predict(StevensTree,newdata = Test)
PredictROC

pred <- prediction(PredictROC[,2],Test$Reverse)
perf <- performance(pred,"tpr","fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

StevensTree5 <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                       Respondent + LowerCourt + Unconst, data = Train, method ="class", minbucket = 5 )
prp(StevensTree5)

StevensTree100 <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                        Respondent + LowerCourt + Unconst, data = Train, method ="class", minbucket = 100 )
prp(StevensTree100)

Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)

StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                         Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree=200)

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)

set.seed(100)
StevensForest100 <- randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                                Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree=200)
PredictForest100 = predict(StevensForest100, newdata = Test)
table(Test$Reverse, PredictForest100)

(43+74)/(43+34+19+74)

set.seed(200)
StevensForest200 <- randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                                   Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree=200)
PredictForest200 = predict(StevensForest200, newdata = Test)
table(Test$Reverse, PredictForest200)
(44+76)/(44+33+17+76)

library(caret)
library(e1071)


# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = Train, method="class", cp = 0.18)
# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

prp(StevensTreeCV)
