# -*- coding: utf-8 -*-
import warnings
import numpy as np
from time import time
from numpy.linalg import norm
from numpy.linalg import solve
from numpy.linalg import eigvals
from math import sqrt
from common_methods import *
from f_tp_1 import *

H_DERIVADA = 0.00001

def get_min_autovalor(A):
	return min(eigvals(A))


def newton_LM(f, x0, eps, kmax, metodo, gamma=0.4):
	iteraciones = 0
	x=x0
	while norm(gradiente(f, x)) > eps and iteraciones < kmax:
		B = hessiano(f, x)
		mu = get_min_autovalor(B)
		if mu<=0:
			B=B+(-mu+gamma)*np.identity(len(x0))
		d = solve(B, -gradiente(f, x))
		t = get_metodo(metodo)(f, x, d)
		x=x+t*d
		iteraciones += 1
	return x, iteraciones

def get_next_matrix_DFP(H, f, x, next_x):
	q = gradiente(f, next_x) - gradiente(f, x)
	p = next_x - x
	return H + np.asmatrix(p).T*np.asmatrix(p)/(q.T*q) - H*np.asmatrix(q).T*np.asmatrix(q)*H/((q.T*(H*np.asmatrix(q).T))[0,0])


def cuasi_newton(f, x0, metodo, eps, kmax):
	iteraciones = 0
	x = x0
	next_x = x + x
	H = np.identity(len(x0))
	while norm(gradiente(f, x)) > eps and iteraciones < kmax and norm(next_x - x) > eps:
		d = -1*(np.dot(H, gradiente(f, x)))
		try:
			d = np.array([d[0,0], d[0,1]])
		except:
			pass
		t = get_metodo(metodo)(f, x, d)
		next_x = x + t*d
		H = get_next_matrix_DFP(H, f, x, next_x)
		x = next_x
		iteraciones += 1
	return x
		

def get_minimizer_function(f_name):
	if f_name == "metodo_gradiente": return metodo_gradiente
	if f_name == "gradientes_conjugados": return gradientes_conjugados
	if f_name == "cuasi_Newton": return cuasi_newton
	raise Exception("Unknown function: {}".format(f_name))

def vector_matrix_vector_as_number(v, M):
	return (np.asmatrix(v)*(M*np.asmatrix(v).T))[0,0]

def punto_de_cauchy(f, x, delta):
	fk = f(x)
	gk = gradiente(f, x)
	B = hessiano(f, x)
	gBg = vector_matrix_vector_as_number(gk, B)
	tita = 1 if gBg <= 0 else min(norm(fk)**3/(delta*gBg), 1)
	return -(tita*delta/norm(gk))*gk



def region_de_confianza(f, x0, eps=1e-5, kmax=1000, delta0=1, eta=0.2):
	iteraciones = 0
	x = x0
	delta=delta0
	while norm(gradiente(f, x)) > eps and iteraciones < kmax:
		dk = punto_de_cauchy(f, x, delta)
		ared = f(x) - f(x+dk)
		B = hessiano(f, x)
		qk = lambda y : f(x) + np.dot(gradiente(f, x), (y-x)) + 0.5*vector_matrix_vector_as_number((y-x), B)
		mk = lambda d : qk(x + d)
		pred = mk(0) - mk(dk)
		ro_k = ared/pred
		if ro_k > eta:
			next_x = x+dk
		else:
			next_x = x
		if ro_k < 0.25:
			delta/=2.
		else:
			if ro_k > 0.75 and norm(dk) == delta:
				delta *= 2.
		x = next_x
		iteraciones += 1
	return x, iteraciones
		

def consigna2():
	start_consigna = time()
	KMAX = 1000
	EPS = 1e-5
	S = [lambda f, x0: metodo_gradiente(f, x0, metodo="armijo", kmax=KMAX, eps=EPS), lambda f, x0: newton_LM(f, x0, metodo="armijo", kmax=KMAX, eps=EPS),
		lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=1, eta=0.2), lambda f, x0: gradientes_conjugados(f, x0, metodo="wolfe", kmax=KMAX, eps=EPS)]
	S_names = ["Metodo gradiente", "Newton LM", "Region de confianza", "Gradientes conjugados"]
	#S.append(lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=1, eta=0.2))
	#S_names.append("Region de confianza modificada")
	P = [globals()["f"+str(n)] for n in xrange(21)]
	tabla_C = []
	M = 1e8
	minCol = [M] * len(P)
	for s in S:
		fila = []
		for j, p in enumerate(P):
			t_start = time()
			x, iters = s(p, puntos_iniciales[int(p.__name__[1:])])
			total = time() - t_start
			if norm(x) > 1e5 or iters >= KMAX:
				cost = M
			else:
				cost = total
			fila.append(cost)
			minCol[j] = min(minCol[j], cost)
		tabla_C.append(fila)
	print "Matriz C"
	print_nice_table(tabla_C)
	print "-" * 30
	desempenio = []
	for i in xrange(len(S)):
		fila = []
		for j in xrange(len(P)):
			fila.append(tabla_C[i][j]/minCol[j])
		desempenio.append(fila)
	print "Matriz de desempenio"
	print_nice_table(desempenio)
	print "+" * 30
	robustez = [sum(1.0 for cost in fila if cost < M)/len(P) for fila in tabla_C]
	for algo_name, algo_robustez in zip(S_names, robustez):
		print "Robustez de {} = {}".format(algo_name, algo_robustez)
	print "-" * 50
	eficiencia = [sum(1.0 for r in fila if r == 1)/len(P) for fila in desempenio]
	for algo_name, algo_efi in zip(S_names, eficiencia):
		print "Eficiencia de {} = {}".format(algo_name, algo_efi)
	print "\n" + "-" * 50 + "\n"
	print "En total tarde", time() - start_consigna
	graph_desempenio(desempenio)




consigna2()
