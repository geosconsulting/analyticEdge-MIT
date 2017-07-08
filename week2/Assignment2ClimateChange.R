wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
climate <- read.csv("dati/climate_change.csv")

training_set <- subset(climate,Year<=2006)
testing_set <- subset(climate,Year>2006)

datiPlot <- climate[,3:9]

plot(datiPlot)
     
TempReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_set)
summary(TempReg)

cor(training_set)

TempRegSimple <- lm(Temp ~ MEI +  N2O + TSI + Aerosols,data = training_set)
summary(TempRegSimple)

TempRegStepped <- step(TempReg)
summary(TempRegStepped)

# Make predictions on test set
TemperaturesPredictions <- predict(TempRegStepped, newdata = testing_set)

# Compute out-of-sample R^2
SSE = sum((TemperaturesPredictions - testing_set$Temp)^2)
SST = sum((mean(training_set$Temp) - testing_set$Temp)^2)
R2 = 1 - SSE/SST

R2

