table(WHO$Region)
tapply(WHO$Over60,WHO$Region,mean)
tapply(WHO$LiteracyRate,WHO$Region,min)
tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=TRUE)
