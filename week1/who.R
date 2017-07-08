WHO  <- read.csv("dati/WHO.csv")
WHO.Europe <- subset(WHO,Region == "Europe")
write.csv(WHO.Europe,"WHO_Europe.csv")

mean(WHO$Under15)
sd(WHO$Under15)

summary(WHO$Under15)

which.min(WHO$Under15)
WHO$Country[86]

WHO$Country[which.max(WHO$Under15)]
plot(WHO$GNI,WHO$FertilityRate)
Outliers <- subset(WHO, GNI>10000 & FertilityRate>2.5)

nrow(Outliers)

Outliers[c("Country","GNI","FertilityRate")]

mean(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]

WHO$Country[which.max(WHO$LiteracyRate)]

hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy~WHO$Region, xlab="",ylab="Life Expectancy", main="Life Expectancy of Country by Region")

table(WHO$Region)
summary(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate,WHO$Region, min,na.rm=T)

tapply(WHO$ChildMortality, WHO$Region, mean)
