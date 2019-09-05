
procedure_one_person <- function(p) {
	dice <- sample(1:6, 1)
	if (dice <= 2) {
		return (runif(1) <= p)
	}
	return (sample(0:1, 1))
}

p_emv <- function(n, p, q=0) {
	reality <- 0
    for (person in 1:n) {
        reality <- reality + (runif(1) <= p) * (runif(1) >= q)
    }
#    print(paste(reality, n))
    return (reality / n)
}

p_a <- function(n, p) {
	reality <- 0
    for (person in 1:n) {
        reality <- reality + procedure_one_person(p)
    }
    return (3 * reality / n - 1)
}

p_b <- function(n, p) {
    tmp <- p_a(n, p)
    if (0 <= tmp && tmp <= 1) {
		return (tmp);
	}
	if (tmp > 1) {
		return (1)
	}
	return (0)
#    return (tmp * (0 <= tmp && tmp <= 1) + (tmp > 1))
}

monte_carlo <- function(func, n, p, n_rep=10) {
	result <- 0
	for (essay in 1:n_rep) {
		
		result <- result + ((func(n, p) - p) ^ 2)
	}
	return (result / n_rep)
}

ej2 <- function(n=1000, p=0.2) {
	print(paste("El ECM de P_a es: ", monte_carlo(p_a, n, p)))
	print(paste("El ECM del P_emv es: ", monte_carlo(p_emv, n, p)))
}

ej3 <- function(n=1000, p=0.2) {
	print(paste("El ECM de P_b es: ", monte_carlo(p_b, n, p)))
}

ej4 <- function(n=1000, p=0.2, q=0.4) {
	set_q <- (function(n, p) p_emv(n, p, q))
	e_q <- monte_carlo(set_q, n, p)
	return (e_q)
}

ej5 <- function(n_sample=100, n=1000, p=0.2) {
	q_vec <- c()
	e_q_vec <- c()
	for (i in 0:n_sample) {
		q <- i / n_sample
		q_vec <- c(q_vec, q)
		e_q_vec <- c(e_q_vec, ej4(n, p, q))
	}

	a <- monte_carlo(p_a, n, p)
	b <- monte_carlo(p_b, n, p)
	print(paste(a, b));
	FILES_FOLDER = getwd();
	filepath = paste(FILES_FOLDER, "ej5.png", sep="/");
	png(filepath);
	plot(q_vec, e_q_vec, xlab="q", ylab="e_q", pch=19);
	abline(h=a);
	abline(h=b);
	print(paste("See file ", filepath));
	dev.off();
}

ej2();
ej3();
ej4();
ej5();

