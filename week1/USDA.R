USDA  <- read.csv("dati/USDA.csv")

which.max(USDA$Sodium)

USDA$Description[265]

names(USDA)

HighSodium <- subset(USDA,Sodium>10000)
nrow(HighSodium)

HighSodium$Description
match("CAVIAR",USDA$Description)

USDA$Sodium[4154]

sodio_caviale <- USDA$Sodium[match("CAVIAR",USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium,na.rm = T)

plot(USDA$Protein,USDA$TotalFat,xlab = "Protein", ylab = "Fat",main = "Protein vs Fat", col='Red')

hist(USDA$VitaminC,xlab = "Vitamin C", main = "Hist Vitmin C", xlim = c(0,100),breaks = 2000)

boxplot(USDA$Sugar,main="BoxPlot Sugar Level",ylab="Sugar (g)")

USDA$Sodium[1] > mean(USDA$Sodium,na.rm = T)
USDA$Sodium[50] > mean(USDA$Sodium,na.rm = T)

HighSodium <- USDA$Sodium > mean(USDA$Sodium,na.rm = T)
str(HighSodium)

USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm = T))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))

table(USDA$HighSodium)
table(USDA$HighSodium,USDA$HighFat)

tapply(USDA$Iron, USDA$HighProtein, mean,na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, max,na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, summary,na.rm=T)
