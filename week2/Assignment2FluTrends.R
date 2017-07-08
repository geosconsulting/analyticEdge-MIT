wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
FluTrain <- read.csv("dati/FluTrain.csv")

hist(FluTrain$ILI,breaks = 20)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1  <- lm(log(ILI) ~ Queries,data = FluTrain )
summary(FluTrend1 )

correlazione <- cor(log(FluTrain$ILI),FluTrain$Queries)

caso1 = correlazione^2
caso2 = log(1/correlazione)
caso3 = exp(-0.5*correlazione)

FluTest <- read.csv("dati/FluTest.csv")

which(FluTest$Week=="2012-03-11 - 2012-03-17")
ObservedILI<-FluTest[11,2]

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
EstimatedILI <- PredTest1[11]

summary(PredTest1)
relerr_03112012<- (ObservedILI-EstimatedILI)/ObservedILI
relerr_03112012

SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI),-2,na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

plot(FluTrain$ILILag2,log(FluTrain$ILI))

FluTrend2  <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

ILILag2t = lag(zoo(FluTest$ILI),-2,na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2t)
summary(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 <- exp(predict(FluTrend2,newdata = FluTest))
summary(PredTest2)

SSE2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE2 <- sqrt(SSE2/nrow(FluTest))
RMSE2
