#### 1) ####

LETRAS = c('A', 'B')

simularLetra <- function() {
	return(sample(LETRAS, 1))
}

checkAllLettersInDictionary <- function(w){
	for(letra in strsplit(w, "")[[1]]){
		if(!letra %in% LETRAS){
			stop(paste("La letra", letra, "no esta en el conjunto posible:", paste(LETRAS, collapse=", ")))
		}
	}
}

cuantoTardaEnEscribir <- function(w) {
	checkAllLettersInDictionary(w) # Por las dudas, para que no tarde infinito si alguna letra no es opcion
	size <- nchar(w)
	monoStr <- ""
	for(i in 1:size) {
		monoStr <- paste(monoStr, simularLetra(), sep="")
	}
	pasos <- size
	while(monoStr != w) {
		monoStr <- substr(monoStr, 2, size)
		monoStr <- paste(monoStr, simularLetra(), sep="")
		pasos <- pasos + 1
	}
	return(pasos)
}
#### Fin 1) ####

#### 2) ####

getPasos <- function(w, rep){
	pasos_vec = c()
	for(i in 1:rep){
		print(i)
		pasos_vec <- c(pasos_vec, cuantoTardaEnEscribir(word))
	}
	return(pasos_vec)
}

words = c("AA", "AB", "AAA", "AAB", "ABA", "BABA", "BABAB")
mrep = 1000

for(word in words){
	pasos_vec = getPasos(word, mrep)
	prom = sum(pasos_vec)/length(pasos_vec)
	print(paste("Para escribir la palabra", word, "se necesita en promedio", prom))
	png(paste('/home/cherny/histograma_', word, '.png', sep=""))
	# agrego un 0 porque si no se unen los primeros dos valores
	hist(c(0, pasos_vec), xlab=paste("Tiempo hasta tipear palabra", word), breaks=max(pasos_vec), main=paste("Histograma para", mrep, "repeticiones"))
	abline(v=prom, col="red")
	dev.off()
}

#### Fin 2) ####
