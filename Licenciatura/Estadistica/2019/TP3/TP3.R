# Sebastian Cherny y Melanie Sclar

set.seed(1234)

calcularIntervalos = function(muestra, q_perc, alpha = 0.05, nboot = 2000){
  estimador_muestra = quantile(muestra, q_perc)
  quantiles_boot = rep(NA,nboot)
  n = length(muestra);
  for(j in 1:nboot){
    muestra.boot = sample(muestra, size = n, replace = T)
    quantiles_boot[j] = quantile(muestra.boot, q_perc)
  }
  var_quantiles_boot = var(quantiles_boot)
  metodo_1 = c(estimador_muestra - sqrt(var_quantiles_boot) * qnorm(1-alpha/2) , estimador_muestra + sqrt(var_quantiles_boot) * qnorm(1-alpha/2))
  metodo_2 = c(quantile(quantiles_boot, alpha/2), quantile(quantiles_boot, 1-alpha/2))
  return(list(metodo1=metodo_1, metodo2=metodo_2))
}

show_results = function(muestra){
	for(q in c(0.25, 0.5, 0.75)){
		l = calcularIntervalos(muestra, q)
		print(paste("Para el percentil ", q, "obtuve los siguientes intervalos de nivel 0.05: Metodo 1 = (", l$metodo1[1], ", ", l$metodo1[2], ") ; Metodo 2 = (", l$metodo2[1], ", ", l$metodo2[2], ")"));
		print(paste("Cuando el percentil real de la muestra es: ", quantile(muestra, q)));
		print("--------------------------------------------------");
	}
}

read_file_and_show_results = function(filepath){
	if (filepath == ""){
		filepath = paste(getwd(), "datos_titanic.csv", sep="/")
	}
	MyData <- read.csv(file=filepath, header=TRUE, sep=",")
	show_results(MyData$Fare);
}

main = function(){
	args = commandArgs(trailingOnly=TRUE)
	filepath = "";
	if (length(args) > 0) {
		filepath = args[1];
	}
	read_file_and_show_results(filepath);
}

main();

#[1] "Para el percentil  0.25 obtuve los siguientes intervalos de nivel 0.05: Metodo 1 = ( 7.83063484058902 ,  7.99016515941098 ) ; Metodo 2 = ( 7.8958 ,  8.05 )"
#[1] "Cuando el percentil real de la muestra es:  7.9104"
#[1] "--------------------------------------------------"
#[1] "Para el percentil  0.5 obtuve los siguientes intervalos de nivel 0.05: Metodo 1 = ( 13.0274600194023 ,  15.8809399805977 ) ; Metodo 2 = ( 13 ,  15.7417 )"
#[1] "Cuando el percentil real de la muestra es:  14.4542"
#[1] "--------------------------------------------------"
#[1] "Para el percentil  0.75 obtuve los siguientes intervalos de nivel 0.05: Metodo 1 = ( 28.176966950659 ,  33.823033049341 ) ; Metodo 2 = ( 29 ,  34.6550325 )"
#[1] "Cuando el percentil real de la muestra es:  31"
#[1] "--------------------------------------------------"

