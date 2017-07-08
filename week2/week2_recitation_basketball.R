NBA <- read.csv("dati/NBA_train.csv")

table(NBA$W,NBA$Playoffs)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff,NBA$W)

WinsReg <- lm(W~PTSdiff,data=NBA)
summary(WinsReg)

soglia_ptfdiff <- (42-41)/0.0326

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK , data = NBA)
summary(PointsReg)
SSE <- sum(PointsReg$residuals^2)
RMSE <- sqrt(SSE/nrow(NBA))
mean(NBA$PTS)

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK , data = NBA)
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK , data = NBA)
summary(PointsReg3)

PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
RMSE4 <- sqrt(SSE4/nrow(NBA))
