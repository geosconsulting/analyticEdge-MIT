# Read in the dataset
songs = read.csv("dati/songs.csv")

songs_2010 <- subset(songs,year=='2010')

songs_jacko <- subset(songs,artistname=='Michael Jackson')

table(songs_jacko$Top10)

songs_jacko_top10 <- subset(songs_jacko,Top10==1)
songs_jacko_top10$songtitle

songs_jacko_top10[c("songtitle","Top10")]
songs_jacko[c("songtitle","Top10")]

table(songs$timesignature)
max_tempo = max(songs$tempo)
subset(songs$songtitle,songs[,9] == max_tempo)

SongsTrain <- subset(songs,year<=2009)
SongsTest <- subset(songs,year==2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[,!(names(SongsTrain)%in%nonvars)]
SongsTest = SongsTest[,!(names(SongsTest)%in%nonvars)]

Model1 = glm(Top10~ ., data = SongsTrain, family = binomial )
summary(Model1)

cor(SongsTrain$loudness,SongsTrain$energy)

Model2 = glm(Top10~ . - loudness, data = SongsTrain, family = binomial)
summary(Model2)

Model3 = glm(Top10~ . - energy, data = SongsTrain, family = binomial)
summary(Model3)

pred1 = predict(Model3,newdata = SongsTest,type="response")
table(SongsTest$Top10, pred1 >= 0.45)

accuracy <- (309+19)/(309+19+40+5)
accuracy

accuracy_baseline <- (309+5)/(309+19+40+5)
accuracy_baseline

specificity <- 309/(309+5)
sensitivity <- 19/(40+19)
