import numpy as np
import random
import time
import sys
from argparse import ArgumentParser


def getTotalLength(camino, dist):
	total = 0
	for i in xrange(len(camino)):
		total += dist[camino[i]][camino[i-1]]
	return total

def mejor_camino(dist, camino_inicial=None, max_iters=3000):
	C = len(dist)
	if camino_inicial is None:
		l = range(C)
		random.shuffle(l)
		camino_inicial = tuple(l)
	total_actual = getTotalLength(camino_inicial, dist)
	best_camino = camino_inicial
	best_total = total_actual
	iters = 0
	while iters < max_iters:
		mejor_cambiando_2 = total_actual
		first_to_change = None
		second_to_change = None
		for v1 in xrange(0, C-1):
			for v2 in xrange(v1+2, C):
				v1_pre = v1-1
				v1_pos = (v1+1)%C
				v2_pre = v2-1
				v2_pos = (v2+1)%C
				if v2 in (v1_pre, v1_pos, v1) or v1 in (v2_pre, v2_pos, v2):
					continue
				valor_cambiando_camino = total_actual - dist[camino_inicial[v1_pre]][camino_inicial[v1]] - dist[camino_inicial[v1_pos]][camino_inicial[v1]] - dist[camino_inicial[v2_pos]][camino_inicial[v2]] + - dist[camino_inicial[v2_pre]][camino_inicial[v2]] + dist[camino_inicial[v1_pre]][camino_inicial[v2]] + dist[camino_inicial[v2_pre]][camino_inicial[v1]] + dist[camino_inicial[v2_pos]][camino_inicial[v1]] + dist[camino_inicial[v1_pos]][camino_inicial[v2]]
				if valor_cambiando_camino < mejor_cambiando_2:
					mejor_cambiando_2 = valor_cambiando_camino
					first_to_change = v1
					second_to_change = v2
		if first_to_change is not None:
			total_actual = mejor_cambiando_2
			l = list(camino_inicial)
			l[first_to_change], l[second_to_change] = l[second_to_change], l[first_to_change]
			camino_inicial = tuple(l)
			if best_total > total_actual:
				best_total = total_actual
				best_camino = camino_inicial
		else:
			l=invert_some_of_first_cities(camino_inicial, random.randint(1, len(camino_inicial)-1))
			camino_inicial = tuple(l)
			total_actual = getTotalLength(camino_inicial, dist)
		iters += 1
	return best_total, best_camino

def invert_some_of_first_cities(l, k=None):
	if k is None:
		k = random.randint(0, len(l)-1)
	return tuple(list(l)[:k][::-1] + list(l)[k:])


def random_transform_camino(l):
	r1=random.randint(0, len(l)-1)
	r2=random.randint(0, len(l)-1)
	if r2 == r1:
		r2=random.randint(0, len(l)-1)
	if r2 == r1:
		r2 = r1 - 1
	modified=list(l)
	modified[r1], modified[r2] = modified[r2], modified[r1]
	return tuple(modified)

def genetic_algo(dist, population=None):
	C = len(dist)
	if population is None:
		population = []
		for _ in xrange(200):
			l=range(C)
			random.shuffle(l)
			population.append((getTotalLength(l, dist), tuple(l)))
	for _ in xrange(1000):
		population=sorted(population)[:1+len(population)/2]
		if abs(population[0][0] - population[-1][0]) <= 1e-6:
			break
		random.shuffle(population)
		new_pop = population[:]
		for i in xrange(0, len(population)):
			padre=population[i][1]
			hijo = invert_some_of_first_cities(padre)
			if np.random.rand() >= 1.0/len(hijo):
				hijo = random_transform_camino(hijo)
			res_1 = getTotalLength(hijo, dist)
			new_pop.append((res_1, hijo))
		population = new_pop
	return sorted(population)[0]

def MST(dist):
	C = len(dist)
	edges = []
	father = range(C)
	def getFather(x):
		if father[x] == x: return x
		father[x] = getFather(father[x])
		return father[x]
	sumaMST  = 0
	for a, row in enumerate(dist):
		for b, val in enumerate(row):
			if a!=b:
				edges.append((val, a, b))
	edges = sorted(edges)
	grafo = {x: [] for x in xrange(C)}
	for val, a, b in edges:
		pa, pb = getFather(a), getFather(b)
		if pa != pb:
			father[pa] = pb
			grafo[a].append((b, val))
			grafo[b].append((a, val))
			sumaMST += val
	return sumaMST

def do_LKH(dist, T=None):
	C=len(dist)
	if T is None:
		T=range(C)
		random.shuffle(T)
	actual_best = getTotalLength(T, dist)
	for _ in xrange(1000):
		mejor_vecino = None
		for hasta in xrange(C):
			for desde in xrange(hasta+2, C):
				newT=[]
				for i in xrange(hasta):
					newT.append(T[i])
				for i in xrange(desde, C):
					newT.append(T[i])
				for i in xrange(hasta, desde):
					newT.append(T[i])
				assert sorted(newT) == range(C)
				vecino = getTotalLength(newT, dist)
				if vecino < actual_best:
					mejor_vecino=tuple(newT)
					actual_best=vecino
		if mejor_vecino is not None:
			T=mejor_vecino
		else:
			break
	return actual_best, T

def _get_menor_arista(dist):
	return min([sorted(row)[1] for row in dist]) # descarto el primer valor que sera 0 por la distancia a si mismo

def _main():
	args = _parse_args()
	matriz_distancias = np.load(args.matrix_file)
        matriz_distancias = [[float(x) for x in row] for row in matriz_distancias]
        C = len(matriz_distancias)
        assert C == len(matriz_distancias), "La matriz no es cuadrada"
	MST_value = MST(matriz_distancias)
	menor_arista = _get_menor_arista(matriz_distancias)
	print "El valor del MST es {} y la menor arista mide {}, por lo que la cota minima es {} y el mejor camino debe medir a lo sumo {}".format(MST_value, menor_arista, MST_value + menor_arista, MST_value * 2)
	if args.amount_of_seeds:
		_main_varios_seeds(args, matriz_distancias)
	else:
		_main_with_seed(args, args.seed, matriz_distancias)

def _main_with_seed(args, my_seed, matriz_distancias):
	t_start = time.time()
	CADA_CUANTO_IMPRIMO = 5
	SECONDS_TO_PRINT = 10
	NEXT_SECOND_TO_PRINT = SECONDS_TO_PRINT
	if my_seed is not None:
		seed_int = int(my_seed)
		print "Corriendo con seed {}".format(seed_int)
		np.random.seed(seed_int)
		random.seed(seed_int)
	if args.rapido:
		how_many_first_cicle = 1
		how_many_iterations = 10
		how_many_to_cut_consec = 10
	elif args.lento:
		how_many_first_cicle = 10
		how_many_iterations = 60
		how_many_to_cut_consec = 20
	else:
		how_many_first_cicle = 5
		how_many_iterations = 40
		how_many_to_cut_consec = 10
	poblacion = []
	best_camino = None
	no_mejor_consec = 0
	segundo_para_imprimir = CADA_CUANTO_IMPRIMO
	for starter in xrange(how_many_first_cicle):
		best_camino = None
		for _ in xrange(how_many_iterations):
			if segundo_para_imprimir < time.time() - t_start:
				print "."
				segundo_para_imprimir = time.time() - t_start + CADA_CUANTO_IMPRIMO
				if int(time.time() - t_start) >= NEXT_SECOND_TO_PRINT:
					print "Pasaron {} segundos".format(NEXT_SECOND_TO_PRINT)
					NEXT_SECOND_TO_PRINT += SECONDS_TO_PRINT
			total, camino = mejor_camino(matriz_distancias, best_camino)
			if camino != best_camino:
				total, camino = do_LKH(matriz_distancias, camino)
			poblacion.append((total, camino))
			if camino == best_camino:
				no_mejor_consec += 1
				if no_mejor_consec == how_many_to_cut_consec:
					break
			else:
				no_mejor_consec = 0
			best_camino = camino
	total, camino = genetic_algo(matriz_distancias, poblacion)
	if args.lento:
		for _ in xrange(50):
			if segundo_para_imprimir < time.time() - t_start:
				print "."
				segundo_para_imprimir = time.time() - t_start + CADA_CUANTO_IMPRIMO
				if int(time.time() - t_start) >= NEXT_SECOND_TO_PRINT:
					print "Pasaron {} segundos".format(NEXT_SECOND_TO_PRINT)
					NEXT_SECOND_TO_PRINT += SECONDS_TO_PRINT
			total, camino = mejor_camino(matriz_distancias, camino)
			total, camino = do_LKH(matriz_distancias, camino)
	elif not args.rapido:
		for _ in xrange(20):
			if segundo_para_imprimir < time.time() - t_start:
				print "."
				segundo_para_imprimir = time.time() - t_start + CADA_CUANTO_IMPRIMO
				if int(time.time() - t_start) >= NEXT_SECOND_TO_PRINT:
					print "Pasaron {} segundos".format(NEXT_SECOND_TO_PRINT)
					NEXT_SECOND_TO_PRINT += SECONDS_TO_PRINT
			total, camino = mejor_camino(matriz_distancias, camino)
			total, camino = do_LKH(matriz_distancias, camino)
	print "-" * 40
	seconds = time.time() - t_start
	print "Despues de {} segundos, se encontro que el mejor recorrido mide {} y una manera de realizarlo es la siguiente:\n{}".format(round(seconds, 1), total, camino)
	print "-" * 40
	return total, camino, seconds

def _parse_args():
	parser = ArgumentParser(argument_default=None)
        parser.add_argument("--rapido", dest='rapido', default=False, action='store_true')
	parser.add_argument("--lento", dest='lento', default=False, action='store_true')
        parser.add_argument("--seed", dest='seed')
        parser.add_argument("--amount-of-seeds", dest='amount_of_seeds')
        parser.add_argument("--archivo", dest='matrix_file')
        args, _ = parser.parse_known_args()
	return args

def _main_varios_seeds(args, matriz_distancias):
	args = _parse_args()
	total_secs = 0
	totals_sum = 0
	amount = int(args.amount_of_seeds) if args.amount_of_seeds else 2
	best_length = float('inf')
	worst_length = float('-inf')
	for seed_num in xrange(amount):
		total, camino, seconds = _main_with_seed(args, seed_num, matriz_distancias)
		totals_sum += total
		total_secs += seconds
		best_length = min(best_length, total)
		worst_length = max(worst_length, total)
	print "Tardando un promedio de {} (total {}), el promedio de los caminos es de {}. El mejor camino fue de {} y el peor de {}".format(round(total_secs*1.0/amount, 2), round(total_secs, 2), round(totals_sum*1.0/amount, 2), best_length, worst_length)

_main()
