mvt <- mvtWeek1
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

mvt$DateConverted <- DateConvert

median(mvt$DateConverted)

mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)

min(table(mvt$Month))

table(mvt$Weekday)

max(table(mvt$Weekday))

table(mvt$Arrest,mvt$Month)
table(mvt$DateConverted,mvt$Arrest)

hist(mvt$DateConverted,breaks = 100)

boxplot(mvt$DateConverted ~ mvt$Arrest)

tabella_arresti_anno <- data.frame(table(mvt$Arrest,mvt$Year))

sort(table(mvt$LocationDescription))
Top5 <- subset(mvt,c(LocationDescription == 'STREET' | 
                     LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' |
                     LocationDescription == 'ALLEY' |
                     LocationDescription == 'GAS STATION' |
                     LocationDescription == 'DRIVEWAY - RESIDENTIAL'))
Top5$LocationDescription <- factor(Top5$LocationDescription)

pippo <- table(Top5$LocationDescription,Top5$Arrest)

dt_top5 <- data.frame(pippo)

gasStationThefts <- subset(Top5,LocationDescription == 'GAS STATION')
table(gasStationThefts$Weekday)


DRIVEWAYRESIDENTIALThefts <- subset(Top5,LocationDescription == 'DRIVEWAY - RESIDENTIAL')
table(DRIVEWAYRESIDENTIALThefts$Weekday)
