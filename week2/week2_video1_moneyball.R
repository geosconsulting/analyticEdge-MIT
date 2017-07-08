baseball <- read.csv("dati/baseball.csv")

moneyball <-subset(baseball,Year<2002)

moneyball$RD <-moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W,pch=19,col="green")

WinsReg <- lm(W ~ RD,data = moneyball)
abline(WinsReg,col="red")
summary(WinsReg)

RunsReg <- lm(RS ~ OBP + SLG + BA , data = moneyball)
summary(RunsReg)

RunsRegNoBA <- lm(RS ~ OBP + SLG , data = moneyball)
summary(RunsRegNoBA)

EChavez <- -804.63 + (2737.77*0.338)+(1584.91*0.540)
JGiambi <- -804.63 + (2737.77*0.391)+(1584.91*0.450)
FMenechino <- -804.63 + (2737.77*0.369)+(1584.91*0.374)
GMyers <- -804.63 + (2737.77*0.313)+(1584.91*0.447)
CPena <- -804.63 + (2737.77*0.361)+(1584.91*0.500)
