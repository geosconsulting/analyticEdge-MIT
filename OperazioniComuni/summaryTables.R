USDA <- read.csv("dati/USDA.csv")

# USDA$HighFat <- (USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
# USDA$HighSugar <- (USDA$Sugar > mean(USDA$Sugar,na.rm = TRUE))
# USDA$HighCarb <- (USDA$Carbohydrate > mean(USDA$Carbohydrate,na.rm = TRUE))
# USDA$HighSodium <- (USDA$Sodium > mean(USDA$Sodium,na.rm = TRUE))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein,na.rm = TRUE))

table(USDA$HighSodium)
table(USDA$HighSodium,USDA$HighFat)

tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=TRUE)
tapply(USDA$VitaminC,USDA$HighCarb,max,na.rm=TRUE)
tapply(USDA$VitaminC,USDA$HighCarb,summary,na.rm=TRUE)
