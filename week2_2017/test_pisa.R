pisaTrain <- read.csv("dati/pisa2009train.csv")
pisaTest <- read.csv("dati/pisa2009test.csv")

tapply(pisaTrain$readingScore,pisaTrain$male,mean)

summary(pisaTrain)
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")
str(pisaTrain)

lmScore <- lm(readingScore ~ .,data = pisaTrain)
summary(lmScore)

SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))

predTest = predict(lmScore,newdata = pisaTest)
summary(predTest)

SSE_prediction = sum((predTest - pisaTest$readingScore)^2)
SST_prediction = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE_prediction/SST_prediction
RMSE_prediction = sqrt(SSE_prediction/nrow(pisaTest))

baseline <- mean(pisaTrain$readingScore)
SSE_baseline <- sum((predTest - pisaTest$readingScore)^2)
SST_baseline = sum((baseline - pisaTest$readingScore)^2)
R2_baseline = 1 - SSE_baseline/SST_baseline