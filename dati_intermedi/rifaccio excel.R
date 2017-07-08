dipe_vect <- c(2,2,8)
indipe_vect <- c(0,1,1)

matrice_valori <- data.frame(cbind(indipe_vect,dipe_vect))
colnames(matrice_valori) <- c("indipe","dipe")

plot(matrice_valori$indipe,matrice_valori$dipe,pch=19,col='red')

mod_lineare <- lm(dipe~indipe,data=matrice_valori)
residuali <- mod_lineare$residuals

SSE = sum(mod_lineare$residuals^2)
