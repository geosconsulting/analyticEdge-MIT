songs <- read.csv("dati/songs.csv")

songs_2010 <- subset(songs,songs$year=='2010')

table(songs$year)

summary(songs)
