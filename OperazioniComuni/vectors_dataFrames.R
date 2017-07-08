Country <- c('Italy','USA','UK')
Age <- c(12,23,11)
Countries <- data.frame(Country,Age)
Countries$Income <- c(100,200,140)

Country <- c('Greece','France')
Age <- c(15,21)
Income <- c(56,99)
NewCountries <- data.frame(Country,Age,Income)


AllCountries<-rbind(Countries,NewCountries)
