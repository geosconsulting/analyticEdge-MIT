wine <- read.csv("dati/wine.csv")

model1 = lm(Price ~ AGST,data=wine)

model1$residuals

SSE <- sum(model1$residuals^2)

model2 = lm(Price ~ AGST + HarvestRain,data=wine)

SSE2 <- sum(model2$residuals^2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop , data=wine)

SSE3 <- sum(model3$residuals^2)

model_esercizio = lm(Price ~ HarvestRain + WinterRain , data=wine)

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age , data=wine)

cor(wine$WinterRain,wine$Price)
cor(wine$HarvestRain,wine$AGST)
cor(wine)

cor(wine$HarvestRain,wine$WinterRain)

model5 = lm(Price ~ AGST + HarvestRain + WinterRain  , data=wine)
summary(model5)
