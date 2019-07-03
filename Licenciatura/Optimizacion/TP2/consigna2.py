import numpy as np
import sys
import time


def getCanonico(i, x):
	return np.array([0]*i + [1] + [0]*(len(x)-i-1))

def getVecino(x):
	h=0.5
	vecinos = [x+h*getCanonico(i, x) for i in xrange(len(x))]
	vecinos += [x-h*getCanonico(i, x) for i in xrange(len(x))]
	vecinos = [v for v in vecinos if all(abs(coord) <= 10 for coord in v)]
	indx = int(np.random.rand()*len(vecinos))
	return vecinos[indx]

def sim_ann(f, x0, mult=0.95, T0=25000, T_min=100, get_vecino_func=getVecino):
	T = T0
	s = x0
	best_s = s
	best_f_s = f(s)
	last_f_s = best_f_s
	while T > T_min:
		t = get_vecino_func(s)
		T = mult * T
		f_t = f(t)
		if f_t < last_f_s:
			s = t
			last_f_s = f_t
			if f_t < best_f_s:
				best_s = t
				best_f_s = f_t
		elif np.e**((last_f_s-f_t)/T) > np.random.rand():
			s = t
			last_f_s = f_t
	return best_s, best_f_s


def holder_table(x):
    return -np.abs(np.sin(x[0])*np.cos(x[1])*np.exp(abs(1-np.sqrt(x[0]**2+x[1]**2)/np.pi)))

def schaffer_2(x):
    return 0.5 + (np.sin(x[0]**2 - x[1]**2)**2-0.5)/(1+0.001*(x[0]**2+x[1]**2)**2)


def _main():
	t_start = time.time()
	if len(sys.argv) > 1:
		np.random.seed(int(sys.argv[1]))
	for f in [holder_table, schaffer_2]:
		answer_point, answer_val = None, None
		for _ in xrange(2000):
			p = np.random.rand(2)*10
			p_sol, p_sol_val = sim_ann(f, p)
			if answer_val is None or answer_val > p_sol_val:
				answer_val = p_sol_val
				answer_point = p_sol
                print "Para la funcion {}, el punto de menor valor encontrado es el {}, donde el valor de la funcion es {}".format(f.__name__, answer_point, answer_val)
	print "Tardo {} segundos en correr".format(round(time.time()-t_start, 2))

_main()

