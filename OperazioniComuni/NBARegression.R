NBA <- read.csv("dati/NBA_train.csv")

#posso verificare quante volte team è arrivato ai playoffs
#per ogni numero di vittorie es 42 vittorie portano ad arrivare ai playoff
table(NBA$W,NBA$Playoffs)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS

#sicuramente esiste una relazione lineare tra i punti segnati e le vittorie
plot(NBA$diff,NBA$W)

WinsReg <- lm(W~PTSdiff,data = NBA)
summary(WinsReg)

#Regression equations
#w = 41 + 0.0326*(PTSdiff)

#con questa differenze di punti si va ai playoff 
#perche permettono di vincere almeno 42 games?
pts.Diff.playoff = (42-41)/0.0326

#Adesso come faccio a fare almeno la differenza sopra descritta?
#Vediamo i vari aspetti della squadra rimbalzi, difesa,etc... 
#Adesso i punti sono la variabile dipendente
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK , data = NBA)
summary(PointsReg)

SSE <- sum(PointsReg$residuals^2)
#Facciamo un errore di punti come dopo
RMSE <- sqrt(SSE/nrow(NBA))

#Il numeoro punti medi è come dopo quindi l'errore sopra è piccolo
mean(NBA$PTS)

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK , data = NBA)
summary(PointsReg2)

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK , data = NBA)
summary(PointsReg3)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL , data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
#Facciamo un errore di punti come dopo
RMSE4 <- sqrt(SSE4/nrow(NBA))

NBA_test <- read.csv("dati/NBA_test.csv")
PointsPredictions = predict(PointsReg4, newdata = NBA_test) 

SSE_prediction = sum((PointsPredictions - NBA_test$PTS)^2)
SST_prediction = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE_prediction/SST_prediction
RMSE_prediction = sqrt(SSE_prediction/nrow(NBA_test))










































