orden_maximo = function(vector){
	n = length(vector)
	ans = c(1)
	indices = rep(-1, n)
	for (i in 1:n){
		indices[vector[i]] = i
	}
	for (i in 2:n){
		ending_here = 1
		if(indices[i-1]<indices[i]){
			ending_here = ans[i-1]+1
		}
		ans = c(ans, ending_here)
	}
	return(max(ans))
}


shuffle = function(perm){
	n = length(perm)
	K = rbinom(1, n, 0.5)
	lugaresRandom = sample(seq(1, n), K, replace=FALSE)
	lugaresRandom = sort(lugaresRandom, decreasing=FALSE)
	ans = rep(-1, n)
	deArriba = 1
	for (i in 1:K){
		ans[lugaresRandom[i]] = perm[deArriba]
		deArriba = (deArriba+1)
	}
	for (i in 1:n){
		if(ans[i]==-1){
			ans[i] = perm[deArriba]
			deArriba = (deArriba+1)
		}
	}
	return(ans)
}

get_random_perm = function(n){
	return( sample(seq(1,n), n, replace=FALSE) )
}

esperanza = function(vector){
	return (sum(vector)/length(vector))
}

desv = function(vector){
	return(sd(vector))
}

# Grafica. Le paso el valor de la esperanza del orden de una permutacion random y su desviacion
# para meter ese valor en el medio con otro color y compararlo con los otros
graficar = function(esperanzas, desviaciones, esperanza_random_perm, desv_random_perm){
	len = length(esperanzas)
	barras = len+1
        pos_agregada = 10
        y = c(esperanzas[1:pos_agregada], esperanza_random_perm, esperanzas[(pos_agregada+1):len])
        y = matrix(y, 1, barras)
        y.means = apply(y, 2, mean)
        y.sd = c(desviaciones[1:pos_agregada], desv_random_perm, desviaciones[(pos_agregada+1):len])
        barx = barplot(y.means, names.arg=c(seq(1,pos_agregada), "rand_perm", seq(pos_agregada+1, length(esperanzas))), ylim=c(0,30), col=c( rep("blue", pos_agregada), "red", rep("blue", (len-pos_agregada))), axis.lty=1, xlab="Shuffles", ylab="Esperanza de orden maximo")
        error.bar(barx,y.means, y.sd)
}


# Pone las barras de error en el grafico hecho
error.bar = function(x, y, upper, lower=upper, length=0.1,...){
        if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper)){
                stop("vectors must be same length")
        }
        arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}



# MAIN, donde se hace todo

N_GRANDE=100
CARTAS=52

# Obtengo la esperanza y la desviacion estandar del orden maximo de una permutacion al azar, realizando 100 experimentos
ordenes_random_permutations = c()
for (i in 1:N_GRANDE){
	perm = get_random_perm(CARTAS)
	orden = orden_maximo(perm)
	ordenes_random_permutations = c(ordenes_random_permutations, orden)
}
valor_esperado_permutacion_random = esperanza(ordenes_random_permutations)
desviacion_permutacion_random = desv(ordenes_random_permutations)
print(paste("Esperanza de orden maximo de permutacion al azar: ", valor_esperado_permutacion_random))
print(paste("Desviacion estandar orden maximo de permutacion al azar: ", desviacion_permutacion_random))
# --------------------------------------------------- #


# Obtengo las esperanzas y las desviaciones del orden maximo dada una cierta cantidad de shuffles, para luego graficar
esperanzas_ordenes_segun_shuffles = c()
desviaciones_segun_shuffles = c()
MAX_SHUFFLES=20
for (numero_de_shuffles in 1:MAX_SHUFFLES){
	ordenes_shuffle = c()
	for (num in 1:N_GRANDE){
		mazo = seq(1, CARTAS)
		for (i in 1:numero_de_shuffles){
			mazo = shuffle(mazo)
		}
		ordenes_shuffle = c(ordenes_shuffle, orden_maximo(mazo))
	}
	esperanzas_ordenes_segun_shuffles = c(esperanzas_ordenes_segun_shuffles, esperanza(ordenes_shuffle))
	desviaciones_segun_shuffles = c(desviaciones_segun_shuffles, desv(ordenes_shuffle))
}
GRAPH_FILE_PATH = "/home/cherny/Estadistica/grafico.png"
png(GRAPH_FILE_PATH)
graficar(esperanzas_ordenes_segun_shuffles, desviaciones_segun_shuffles, valor_esperado_permutacion_random, desviacion_permutacion_random)
dev.off()
