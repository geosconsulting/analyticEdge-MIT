linreg <- -1.5 + (3*1) + (-0.5*5)
odds <- exp(linreg)
logit <- log(odds)
rev_odds <- 1-odds
sotto <- 1+exp(-linreg)
P = 1/sotto

quality <- read.csv("dati/quality.csv")
table(quality$PoorCare)

library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)
split
qualityTrain = subset(quality,split ==TRUE)
qualityTest = subset(quality,split == FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain,family=binomial)
summary(QualityLog)

predictTrain = predict(QualityLog,type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare,mean)

library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
