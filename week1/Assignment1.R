mvt  <- read.csv("dati/mvtWeek1.csv")

max(mvt['ID'])
min(mvt['Beat'])

table(mvt$Arrest)
summary(mvt$LocationDescription=='ALLEY')
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Year <- as.numeric(format(DateConvert,'%Y'))

mvt$Date = DateConvert

which.min(table(mvt$Month))

table(mvt$Weekday)
which.max(table(mvt$Weekday))

Arresti <- table(mvt$Month,mvt$Arrest)
which.max(Arresti[,2])

hist(mvt$Date, breaks=100)

boxplot(mvt$Date ~ mvt$Arrest)

FurtiAuto_2001 <- mvt[mvt$Year==2001,]
summary(FurtiAuto_2001)

arresti_2001 <- table(FurtiAuto_2001$Arrest)
prop.table(arresti_2001)

FurtiAuto_2007 <- mvt[mvt$Year==2007,]
arresti_2007 <- table(FurtiAuto_2007$Arrest)
prop.table(FurtiAuto_2007)

FurtiAuto_20012 <- mvt[mvt$Year==2012,]
arresti_2012 <- table(FurtiAuto_20012$Arrest)
prop.table(arresti_2012)

sort(table(mvt$LocationDescription))

Top5 <- subset(mvt, 
               LocationDescription=="STREET" | 
               LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
               LocationDescription=="ALLEY" |
               LocationDescription=="GAS STATION" |
               LocationDescription=="DRIVEWAY - RESIDENTIAL")

Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5$LocationDescription)
prop.table(table(Top5$LocationDescription,Top5$Arrest),1)

table(Top5$LocationDescription,Top5$Arrest)
TheftGasStation <- subset(mvt, LocationDescription=="GAS STATION")

which.max(table(TheftGasStation$Weekday))

TheftResidential <- subset(mvt, LocationDescription=="DRIVEWAY - RESIDENTIAL")
which.min(table(TheftResidential$Weekday))
