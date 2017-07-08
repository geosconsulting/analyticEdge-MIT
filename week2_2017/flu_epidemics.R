library(zoo)
FluTrain <- read.csv("dati/FluTrain.csv")
FluTest <- read.csv("dati/FluTest.csv")

maxVal <- max(FluTrain$ILI)

FluTrain[FluTrain$ILI==maxVal,]
#Alternativa
subset(FluTrain,ILI==max(FluTrain$ILI))

subset(FluTrain,Queries==max(FluTrain$Queries))

#Skewed is better using the logarithm of the 
#dependent variables
hist(FluTrain$ILI)

#plot(log(FluTrain$ILI),FluTrain$Queries)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 <- lm(log(ILI) ~ Queries,data = FluTrain)
summary(FluTrend1)

Correlation = cor(FluTrain$Queries,log(FluTrain$INI))

PredTest1 <- exp(predict(FluTrend1,newdata = FluTest ))

Observed <- FluTest$ILI[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
Predicted <- PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
relative_error <- (Observed-Predicted)/Observed
relative_error

SSE_prediction = sum((PredTest1 - FluTest$ILI)^2)
SST_prediction = sum((mean(FluTrain$ILI) - FluTest$ILI)^2)
R2 = 1 - SSE_prediction/SST_prediction
RMSE_prediction = sqrt(SSE_prediction/nrow(FluTest))

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 <- lm(log(ILI) ~ Queries + log(FluTrain$ILILag2),data = FluTrain)
summary(FluTrend2)

ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

FluTest$ILILag2[1]
FluTest$ILILag2[2] 

FluTrend2 <- lm(log(ILI) ~ Queries + log(FluTest$ILILag2),data = FluTest)
summary(FluTrend2)

PredTest2 <- exp(predict(FluTrend2,newdata = FluTest))
SSE_Pred2 <- sum((PredTest2-FluTest$ILI)^2)
RMSE_pred2 = sqrt(SSE_Pred2/nrow(FluTest))
