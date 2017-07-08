baseball <- read.csv("dati/baseball.csv")
moneyball <- subset(baseball, Year<2002)
str(moneyball)
moneyball$RD <- moneyball$RS - moneyball$RA

plot(moneyball$RD,moneyball$W, xlab = 'Difference between Runs allowed and Runs', ylab = 'Wins')

WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

numero_runs <- function(valore){
  return(WinsReg$coefficients[1] + (WinsReg$coefficients[2]*valore))
}
numero_wins <- calc_val(135)

necessary_runs = (95.0 - WinsReg$coefficients[1]) / WinsReg$coefficients[2]

rd_ra <- 713-614
equazione_video <- 80.8814 + 0.1058 * rd_ra

RunsReg <- lm(RS ~ OBP + SLG , data = moneyball)
summary(RunsReg)

runs_case1 <-  -804.63 + (2737.77*0.311) + (1584.91 * 0.405)
runs_case2 <-  -837.38 + (2913.60*0.297) + (1514.29 * 0.370)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
cor(teamRank,wins2013)