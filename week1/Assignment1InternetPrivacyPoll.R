poll <- read.csv("dati/AnonymityPoll.csv")

table(poll$Smartphone==0)
table(poll$Smartphone==1)


summary(poll$Smartphone)

table(poll$State,poll$Region)
