wine <- read.csv("dati/wine.csv")

model1 <- lm(Price ~ AGST, data=wine)
summary(model1)

SSE_mod1 <- sum(model1$residuals^2)

model2 <- lm(Price ~ AGST + HarvestRain , data=wine)
summary(model2)

SSE_mod2 <- sum(model2$residuals^2)

model.all.var <- lm(Price ~ . , data=wine)
summary(model.all.var)
SSE_model.all.var <- sum(model.all.var$residuals^2)

model_rains_based <- lm(Price ~ HarvestRain + WinterRain , data=wine)
summary(model_rains_based)

SSE_mod_rains_based <- sum(model_rains_based$residuals^2)
SSE_mod_rains_based

cor(wine$Age,wine$FrancePop)

plot(wine$WinterRain,log(wine$Price))
cor(wine$WinterRain,wine$Price)

cor(wine$Age,wine$FrancePop)

cor(wine)

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
wineTest <- read.csv("dati/wine_test.csv")
predictTest <- predict(model4,newdata = wineTest)

SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
testRsquared = 1 - SSE/SST

