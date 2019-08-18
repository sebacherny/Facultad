#!/usr/bin/env Rscript

# Link: http://cms.dm.uba.ar/academico/materias/2docuat2019/estadistica_M/2019/tp1_2019.pdf
# Overleaf: https://www.overleaf.com/project/5d586d18da93ba52ff25d2f5

args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
	print(paste("Using seed", args[1]));
	set.seed(args[1]);
}

get_chosen_value <- function(r, n, our_sample) {
        best_of_first_elements = 0;
        for (j in 1:r) {
                best_of_first_elements = max(c(best_of_first_elements, our_sample[j]));
        }

        chosen_value = our_sample[n];
        for (j in (r+1):n) {
                if (our_sample[j] > best_of_first_elements) {
                        chosen_value = our_sample[j];
                        break;
                }
        }
        return (chosen_value);
}

calc_p_n_r <- function(n, r, n_iter=1000) {
	# 1 is best, n is the worst
	if (n == r) {
                return (0);
	}
	successes = 0;
	for (i in 1:n_iter) {
		our_sample = sample(n);
		if (get_chosen_value(r, n, our_sample) == n) {
			successes = successes + 1;
		}
	}
	return (successes/n_iter);
}

get_vector_of_probas_iterating_r <- function(N) {
	vals_y = c();
        for (r in 1:(N-1)) {
                prob_n_r = calc_p_n_r(N, r);
                vals_y = c(vals_y, prob_n_r);
        }
	return (vals_y);
}

get_arg_max <- function(vec) {
	if (length(vec) == 1) {
		return(1);
	}
	max_index = 1;
	max_val = vec[1];
	for (i in 2:length(vec)) {
		if (vec[i] > max_val) {
			max_val = vec[i];
			max_index = i;
		}
	}
	return (max_index);
}

get_r_n <- function(n) {
	return (get_arg_max(get_vector_of_probas_iterating_r(n)));
}

get_avg_r_n <- function (n, n_iters=10) {
        sum_r_n = 0;
        for (iter in c(1:n_iters)) {
                r_n = get_r_n(n);
                sum_r_n = sum_r_n + r_n;
        }
        return (sum_r_n / n_iters);
}

graph_function <- function(X, Y, filename, xlabel, ylabel, adjust_line) {
	FILES_FOLDER = getwd();
	filepath = paste(FILES_FOLDER, filename, sep="/");
	png(filepath);
	plot(X, Y, type="h", xlab=xlabel, ylab=ylabel);
        print(paste("See file ", filepath));
	if (!missing(adjust_line)) {
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
	graph_function(vals_x, vals_y, "ej3.png", "r", "p_{n,r}");
}

ej4 <- function() {
	print("Ej 4");
	vals_x = 2:50;
	vals_y = c();
	for (n in vals_x) {
		r_n = get_avg_r_n(n);
		vals_y = c(vals_y, r_n);
	}
	graph_function(vals_x, vals_y, "ej4.png", "n", "r_n", TRUE);
}

scatter_plot <- function(X, Y, filename, xlabel, ylabel) {
	FILES_FOLDER = getwd();
	filepath = paste(FILES_FOLDER, filename, sep="/");
	png(filepath);
	plot(X, Y, xlab=xlabel, ylab=ylabel, pch=19);
        print(paste("See file ", filepath));
	dev.off();
}


ej5 <- function(num_samples=60) {
	print("Ej 5");
        n_array = c();
        fraction_array = c();
	for (n in c(1:num_samples)) {
                real_n = n * 5;
                avg_r_n = get_avg_r_n(real_n);
                print(paste("n = ", real_n, " ; avg_r_n = ", avg_r_n, " ; n/r_n = ", real_n/avg_r_n, sep=""));

                n_array = c(n_array, real_n);
                fraction_array = c(fraction_array, real_n/avg_r_n);
	}

        scatter_plot(n_array, fraction_array, "ej5.png", "n", "r_n");
}

#ej3();
#ej4();
ej5();
