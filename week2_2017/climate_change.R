climate.data <- read.csv("dati/climate_change.csv")

train <- subset(climate.data, Year<=2006)
test <- subset(climate.data, Year>2006)

ClimateReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O 
                 + CFC.11 + CFC.12 + TSI + Aerosols,
                 data = train)
summary(ClimateReg)
cor(train)

ClimateRegSimple <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
summary(ClimateRegSimple)

ClimateregStep = step(ClimateReg)
summary(ClimateregStep)

ClimatePrediction= predict(ClimateregStep,newdata = test)
df.diff.temp = cbind(test$Year,test$Month,test$Temp,ClimatePrediction,ClimateregStep$residuals)

SSE_prediction = sum((ClimatePrediction - test$Temp)^2)
SST_prediction = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE_prediction/SST_prediction
R2