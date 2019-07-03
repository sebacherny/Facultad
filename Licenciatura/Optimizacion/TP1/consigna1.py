# -*- coding: utf-8 -*-

import numpy as np
from time import time
from numpy.linalg import norm
from numpy.linalg import solve
from numpy.linalg import eigvals
from math import sqrt
from common_methods import *
import sys

H_DERIVADA = 0.00001


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
	return x, iteraciones
		

def get_minimizer_function(f_name):
	if f_name == "metodo_gradiente": return metodo_gradiente
	if f_name == "gradientes_conjugados": return gradientes_conjugados
	if f_name == "cuasi_Newton": return cuasi_newton
	raise Exception("Unknown function: {}".format(f_name))


def consigna1(f, case_for_seed):
	if case_for_seed is not None:
		np.random.seed(case_for_seed)
	random_points = [np.random.rand(2)*10 - 5 for _ in xrange(150)]
	for method_str, longitud_de_paso in [("metodo_gradiente", "aurea"), ("gradientes_conjugados", "aurea"), ("cuasi_Newton", "aurea")]:
		t_start = time()
		minimizer_points = []
		best_value = float('inf')
		best_point = None
		for p_index, p in enumerate(random_points):
#			if p_index % 30 == 0: print "Voy por el punto {} del método {}".format(p_index, method_str)
			p_min, _ = get_minimizer_function(method_str)(f, p, metodo=longitud_de_paso, eps=1e-5, kmax=750)
			val_at_p = f(p_min)
			if best_value > val_at_p:
				best_value = val_at_p
				best_point = p_min
		print "Método {}, tiempo = {} segundos, mínimo encontrado en el punto {}, de valor {}".format(method_str, time() - t_start, best_point, best_value)


if __name__ == "__main__":
	if len(sys.argv) > 1:
		case_for_seed = int(sys.argv[1])
	else:
		case_for_seed = None
	consigna1(langermann, case_for_seed)
