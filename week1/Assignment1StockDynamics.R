IBM <- read.csv("dati/IBMStock.csv")
GE <- read.csv("dati/GEStock.csv")
ProcterGamble<- read.csv("dati/ProcterGambleStock.csv")
CocaCola<- read.csv("dati/CocaColaStock.csv")
Boeing<- read.csv("dati/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

mean(IBM$StockPrice)
summary(GE$StockPrice)
summary(CocaCola$StockPrice)
summary(Boeing$StockPrice)
summary(ProcterGamble$StockPrice)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,type='l',col='red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col='blue',lty=4)
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col='blue')
lines(GE$Date[301:432], GE$StockPrice[301:432],col='green')
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col='violetred4')
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col='yellowgreen')
legend("topright",legend=c("Coca","Procter","GE","IBM","Boeing"),col=c("red","blue","green","violetred4","yellowgreen"),lty=1)
abline(v=as.Date(c("2004-01-01")), lwd=2,col="red",lty=2)
abline(v=as.Date(c("2005-12-31")), lwd=2,col="red",lty=2)

avg_hist <- mean(IBM$StockPrice)
vect <- tapply(IBM$StockPrice, months(IBM$Date), mean)
vect_sopra <- vect[vect>avg_hist]
vect_sopra
vect

which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))


tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)

