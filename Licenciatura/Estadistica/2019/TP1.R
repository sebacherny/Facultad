#!/usr/bin/env Rscript

# Link: http://cms.dm.uba.ar/academico/materias/2docuat2019/estadistica_M/2019/tp1_2019.pdf

args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
	print(args[1]);
	set.seed(args[1]);
}


calc_p_n_r <- function(n, r) {
	# 1 es el peor, n el mejor
	if (n==r) {
		return(0);
	}
	n_iter = 1000;
	exitos = 0;
	for (i in 1:n_iter) {
		muestra = sample(n);
		mejor_de_los_primeros = 0;
		for (j in 1:r) {
			mejor_de_los_primeros = max(c(mejor_de_los_primeros, muestra[j]))
		}
		elijo = muestra[n];
		for (j in (r+1):n) {
			if (muestra[j] > mejor_de_los_primeros) {
				elijo = muestra[j];
				break;
			}
		}
		if (elijo == n){
			exitos = exitos + 1;
		}
	}
	return (exitos/n_iter);
}

get_vector_of_probas_iterating_r <- function(N) {
	vals_y = c();
        for (r in 1:(N-1)){
                proba_n_r = calc_p_n_r(N, r);
                vals_y = c(vals_y, proba_n_r);
        }
	return (vals_y);
}

get_arg_max <- function(vec) {
	if(length(vec) == 1){
		return(1);
	}
	max_index = 1;
	max_val = vec[1];
	for (i in 2:length(vec)) {
		if(vec[i] > max_val) {
			max_val = vec[i];
			max_index = i;
		}
	}
	return (max_index);
}

get_r_n <- function(n) {
	return(get_arg_max(get_vector_of_probas_iterating_r(n)));
}

graph_function <- function(X, Y, filename, adjust_line) {
	FILES_FOLDER = getwd();
	filepath = paste(FILES_FOLDER, filename, sep="/");
	png(filepath);
	plot(X, Y, type="h"); print(paste("Ver archivo ", filepath));
	if(!missing(adjust_line)){
		line_fit = lm(Y ~ X);
		print(line_fit);
		abline(line_fit);
	}
	dev.off();
}



ej3 <- function() {
	print("Ej 3");
	N = 50;
	vals_x = 1:(N-1);
	vals_y = get_vector_of_probas_iterating_r(N);
#	print(paste("Para n = ", n, ", conviene descartar lxs primerxs ", r_conveniente, " candidates, teniendo asi una proba de ", mejor_proba, " de elegir a le mejor"));
	graph_function(vals_x, vals_y, "ej3.png");
}
#ej3();

ej4 <- function() {
	print("Ej 4");
	vals_x = 2:50;
	vals_y = c();
	for (n in vals_x) {
		r_n = get_r_n(n);
		vals_y = c(vals_y, r_n);
	}
	graph_function(vals_x, vals_y, "ej4.png", TRUE);
}
ej4();

ej5 <- function() {
	print("Ej 5");
	for (n in c(100, 300, 500)) {
		r_n = get_r_n(n);
		print(paste("n = ", n, " ; r_n = ", r_n, " ; n/r_n = ", n/r_n, sep=""));
	}
}
ej5();
