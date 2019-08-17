
# Obtengo un vector de nboot realizaciones de la mediana muestral
obtener.realizaciones = function(data, nboot = 500){
  realizaciones = rep(NA,nboot)
  for(i in 1:nboot){
    muestra.nueva = sample(x = data, size = length(data), replace = T)
    realizaciones[i] = median(muestra.nueva)
  }
  return(realizaciones)
}


estimar.varianza.boot = function(data){
  realizaciones = obtener.realizaciones(data)
  return(var(realizaciones))
}

met.1 = function(data, alfa = 0.05){
  centro = median(data)
  varianza = estimar.varianza.boot(data)
  return(list(inf = centro - qnorm(1 - alfa/2) * sqrt(varianza),
              sup = centro + qnorm(1 - alfa/2) * sqrt(varianza)))
}

met.2 = function(data, alfa = 0.05){
  realizaciones = obtener.realizaciones(data)
  # print(realizaciones)
  return(list(inf = quantile(realizaciones, alfa/2), sup = quantile(realizaciones, 1 - alfa/2)))
}



simul = function(muestras, mediana.real){
  n.muestras = nrow(muestras)
  long.met.1 = rep(NA,n.muestras)
  cub.met.1 = rep(NA,n.muestras)
  long.met.2 = rep(NA,n.muestras)
  cub.met.2 = rep(NA,n.muestras)
  
  for(i in 1:n.muestras){
    print(paste("Muestra numero ", i))
    muestra = muestras[i,]
    ic.met.1 = met.1(muestra)
    long.met.1[i] = ic.met.1$sup - ic.met.1$inf
    cub.met.1[i] = (ic.met.1$sup > mediana.real && ic.met.1 < mediana.real)
    ic.met.2 = met.2(muestra)
    long.met.2[i] = ic.met.2$sup - ic.met.2$inf
    cub.met.2[i] = (ic.met.2$sup > mediana.real && ic.met.2 < mediana.real)
  }
  return(list(long.met.1 = mean(long.met.1), long.met.2 = mean(long.met.2),
              cub.met.1 = mean(cub.met.1), cub.met.2 = mean(cub.met.2) ))
}

set.seed(1234)
muestras = matrix(NA,500,30)
for(i in 1: nrow(muestras)){
  muestras[i,] = rnorm(ncol(muestras))
}
simul(muestras, mediana.real = 0)


set.seed(1234)
muestras = matrix(NA,200,100)
for(i in 1: nrow(muestras)){
  muestras[i,] = rt(ncol(muestras), df = 1)
}
simul(muestras, mediana.real = 0)


set.seed(1234)
muestras = matrix(NA,200,30)
for(i in 1: nrow(muestras)){
  muestras[i,] = rexp(ncol(muestras),rate = 1)
}
simul(muestras, mediana.real = log(2))




