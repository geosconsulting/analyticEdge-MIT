wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
pisaTrain <- read.csv("dati/pisa2009train.csv")
pisaTest <- read.csv("dati/pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
sort(table(pisaTrain$raceeth))

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
str(pisaTrain)

lmScore <- lm(readingScore~.,data = pisaTrain)
summary(lmScore)

SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))

predTest <- predict(lmScore,newdata = pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SSErev = sum((pisaTest$readingScore-predTest)^2)
RMSE <- sqrt(SSE/nrow(pisaTest))

SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1 - SSE/SST
R2



# PROBLEM 4.3 - BASELINE PREDICTION AND TEST-SET SSE
# What is the predicted test score used in the baseline model? 
#Remember to compute this value using the training set and not the test set.
bias = mean(pisaTrain$readingScore)
bias
SST1 = sum((pisaTest$readingScore - bias)^2)
SST1
SST_uguale = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST_uguale

# PROBLEM 4.4 - TEST-SET R-SQUARED
# What is the test-set R-squared value of lmScore?
R_uguale = 1 - SSE/SST
R_uguale
