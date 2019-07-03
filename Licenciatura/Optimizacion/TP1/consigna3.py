# -*- coding: utf-8 -*-
import warnings
import numpy as np
from time import time
from numpy.linalg import norm
from numpy.linalg import solve
from numpy.linalg import eigvals
from math import sqrt
from f_tp_2 import *
from common_methods import *

H_DERIVADA = 0.00001



def vector_matrix_vector_as_number(v, M):
	return np.dot(v, np.dot(M, v))
	return (np.asmatrix(v)*(M*np.asmatrix(v).T))[0,0]

def punto_de_cauchy(f, x, delta):
	fk = f(x)
	gk = gradiente(f, x)
	B = hessiano(f, x)
	gBg = vector_matrix_vector_as_number(gk, B)
	tita = 1 if gBg <= 0 else min(norm(gk)**3/(delta*gBg), 1)
	return -(tita*delta/norm(gk))*gk



def region_de_confianza(f, x0, eps, kmax, delta0=1, eta=0.2):
	iteraciones = 0
	x = x0
	delta=delta0
	while norm(gradiente(f, x)) > eps and iteraciones < kmax and delta > eps:
		dk = punto_de_cauchy(f, x, delta)
		ared = f(x) - f(x+dk)
		B = hessiano(f, x)
		qk = lambda y : f(x) + np.dot(gradiente(f, x), (y-x)) + 0.5*vector_matrix_vector_as_number((y-x), B)
		mk = lambda d : qk(x + d)
		pred = mk(0) - mk(dk)
		with np.errstate(divide='raise'):
			try:
				ro_k = ared/pred
			except:
				print "aaaaaaaaaaa", ared, pred, x, f(x), dk, mk(0), mk(dk)
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
		
def consigna3():
	start_consigna = time()
	KMAX = 300
	EPS = 1e-7
	S = []
	params = [(1, 0.2), (1.55, 0.3), (0.55, 0.15), (2, 0.01)]
	m={}
        S.append(lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=1., eta=0.2))
        S.append(lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=1.55, eta=0.3))
        S.append(lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=0.55, eta=0.15))
        S.append(lambda f, x0: region_de_confianza(f, x0, eps=EPS, kmax=KMAX, delta0=2., eta=0.01))
#	S = [lambda f, x0: region_de_confianza(f, x0, eps=1e-7, kmax=KMAX, delta0=delta_par, eta=eta_par) for delta_par, eta_par in [(1, 0.2), (1.55, 0.3), (0.55, 0.15), (2, 0.01)]]
	P = [globals()["f"+str(n)] for n in xrange(20)]
	tabla_C = []
	M = 1e8
	minCol = [M] * len(P)
	for s in S:
		fila = []
		for j, p in enumerate(P):
			t_start = time()
			x, iters = s(p, puntos_iniciales[int(p.__name__[1:])])
			total = time() - t_start
			if norm(x) > 1e5 or iters >= 300:
				cost = M
			else:
				cost = iters
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
			fila.append(tabla_C[i][j]*1.0/minCol[j])
		desempenio.append(fila)
	print "Matriz de desempenio"
	print_nice_table(desempenio)
	print "+"*30
        robustez = [sum(1.0 for cost in fila if cost < M)/len(P) for fila in tabla_C]
	for param, robus in zip(params, robustez):
		print "Con los parametros {}, robustez = {}".format(param, robus)
	print "-" * 30
	eficiencia = [sum(1.0 for r in fila if r == 1)/len(P) for fila in desempenio]
	for param, efi in zip(params, eficiencia):
		print "Con los parametros {}, eficiencia = {}".format(param, efi)
	print "\n" + "-" * 50 + "\n"
	print "En total tarde", time() - start_consigna
	graph_desempenio(desempenio)

consigna3()
