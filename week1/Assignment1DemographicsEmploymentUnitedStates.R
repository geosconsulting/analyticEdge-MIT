CPS <- read.csv("dati/CPSData.csv")

sort(table(CPS$Industry))
sort(table(CPS$Region))

sort(table(CPS$State))
which.max(table(CPS$State))

tab_cittadini <- table(CPS$Citizenship)
prop.table(tab_cittadini)
sum_cit <- 0.88832615 + 0.05386818
sum_cit

summary(CPS)

ispanici <- CPS[CPS$Hispanic==1,]
table(ispanici$Race)

summary(CPS)
new_CPS <- CPS[rowSums(is.na(CPS))>0,]

prop.table(table(CPS$Region, is.na(CPS$Married)))
prop.table(table(CPS$Sex, is.na(CPS$Married)))
prop.table(table(CPS$Age, is.na(CPS$Married)))
prop.table(table(CPS$Citizenship, is.na(CPS$Married)))

table(CPS$State, is.na(CPS$MetroAreaCode))
prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)),1)

tapply(is.na(CPS$MetroAreaCode),CPS$State, mean)

MetroAreaMap <- read.csv("dati/MetroAreaCodes.csv")
CountryMap <- read.csv("dati/CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
na_nuova_var <- CPS[is.na(CPS$MetroArea),]
sort(table(CPS$MetroArea))

tab <- prop.table(table(CPS$MetroArea,CPS$Race=='Asian'),1)*100
table(tab[,2]>20)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=T))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
na_nuova_var <- CPS[is.na(CPS$Country),]

outside <- CPS[CPS$Country!='North America',]

sort(table(outside$Country))

NY_NJ_PA <- CPS[CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",]
NY_NJ_PA_na.rm <- NY_NJ_PA[!is.na(NY_NJ_PA$Country),]
NY_NJ_PA_na.rm_NOUSA <- NY_NJ_PA_na.rm[NY_NJ_PA_na.rm$Country!='United States',]

prop.table(table(NY_NJ_PA_na.rm$Country!='United States'))

CPS[which.max(table(CPS$MetroArea,CPS$Country=='Brazil')),]

table(CPS$MetroArea,CPS$Country=='Brazil')
table(CPS$MetroArea,CPS$Country=='Somalia')
