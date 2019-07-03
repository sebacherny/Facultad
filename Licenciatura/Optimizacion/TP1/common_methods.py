import numpy as np
from numpy.linalg import norm
from math import sqrt
from math import log10, floor
import matplotlib.pyplot as plt


H_DERIVADA = 0.000001

def langermann(x, d=2, m=5, c=(1, 2, 5, 2, 3), a=np.array([[3, 5, 2, 1, 7], [5, 2, 1, 4, 9]]).T):
    return sum(c[i] * np.exp(-1 / np.pi * sum((x[j] - a[i, j]) ** 2 for j in range(d))) *
               np.cos(np.pi * sum((x[j] - a[i, j]) ** 2 for j in range(d))) for i in range(m))

def derivada_parcial_forward(f, a, i, h=H_DERIVADA):
        canonico = np.array([0]*i + [1] + [0]*(len(a)-i-1))
        return (f(a+h*canonico) - f(a))/h

def derivada_parcial2(f,x,i):
        return derivada_parcial_forward(f, x, i)
        h = 0.1
        formula_dado_h = lambda H : derivada_parcial_forward(f, x, i, H)
        z = formula_dado_h(h)
        h/=2
        y = formula_dado_h(h)
        error = norm(y-z)
        eps=1e-8
        while error>eps and y!=np.nan and y!=np.inf and y!=0:
                z=y
                h/=2
                y=formula_dado_h(h)
                error=norm(y-z)
        return z


def gradiente(f, a):
        l=[]
        for i in range(len(a)):
                l.append(derivada_parcial2(f,a,i))
        return np.array(l)

def hessiano(f, a):
        l=[]
        for i in range(len(a)):
                l.append(gradiente(lambda x : derivada_parcial2(f,x,i), a))
        return np.array(l)



def aurea(f, x, d, eps=1e-5, ro=1):
        tita1=(3-sqrt(5.0))/2.
        tita2=1-tita1
        a=0
        s=ro
        b=2*ro
        fi_b = f(x+b*d)
        fi_s = f(x+s*d)
        while fi_b < fi_s:
                a=s
                s=b
                b=2*b
                fi_b = f(x+b*d)
                fi_s = f(x+s*d)
        u=a+tita1*(b-a)
        v=a+tita2*(b-a)
        fi_u=f(x+u*d)
        fi_v = f(x+v*d)
        while b-a>eps:
                if fi_u<fi_v:
                        b=v
                        v=u
                        u=a+tita1*(b-a)
                        fi_v = fi_u
                        fi_u=f(x+u*d)
                else:
                        a=u
                        u=v
                        v=a+tita2*(b-a)
                        fi_u=fi_v
                        fi_v=f(x+v*d)
        return (u+v)/2.

def armijo(f, x, d, gamma=0.7, eta=0.45):
        t=1
        while f(x+t*d) > f(x) + eta*t*(np.dot(gradiente(f, x), d)):
                t=gamma*t
        return t

def wolfe(f, x, d, c1=0.5, c2=0.75):
        alfa=0
        t=1
        beta=np.inf
        itera=0
        while itera < 50 and t>1e-10:
                itera += 1
                if f(x+t*d) > f(x) + c1*t*(np.dot(gradiente(f, x), d)):
                        beta=t
                        t=0.5*(alfa+beta)
                elif np.dot(gradiente(f, x+t*d), d) < c2*(np.dot(gradiente(f, x), d)):
                        alfa=t
                        t=2*alfa if beta == np.inf else 0.5*(alfa+beta)
                else:
                        break
        return t

def get_metodo(s):
        if s=="armijo": return armijo
        if s=="wolfe": return wolfe
        if s=="aurea": return aurea
        raise Exception("que onda " + str(s))


def metodo_gradiente(f, x0, metodo, eps, kmax):
        iteraciones=0
        x=x0
        while norm(gradiente(f, x)) > eps and iteraciones < kmax:
                d=-1*gradiente(f, x)
                t = get_metodo(metodo)(f, x, d)
                x=x+t*d
                iteraciones+=1
        return x, iteraciones

def _round_number(x, significant=2):
	if not isinstance(x, float):
		return x
	return round(x, significant-int(floor(log10(abs(x))))-1)

def print_nice_table(table):
        max_len = 0
        for i in table:
                for j in i: max_len = max(max_len, len(str(_round_number(j))))
        for i in table:
                j_tmp = []
                for j in i:
                        j_tmp.append( (max_len - len(str(_round_number(j)))) * " " + str(_round_number(j)) + "|")
                print " ".join(j_tmp)

def gradientes_conjugados(f, x0, metodo, eps, kmax):
        iteraciones = 0
        x = x0
        d = -1*gradiente(f, x)
        n = 20#len(x0)
        next_x = x + x
        while norm(gradiente(f, x)) > eps and iteraciones < kmax:
                t = get_metodo(metodo)(f, x, d)
                next_x = x + t*d
                if (iteraciones + 1) % n != 0:
                        try:
                                beta = (gradiente(f, next_x)*gradiente(f, next_x))/(gradiente(f, x)*gradiente(f, x))
                        except:
                                beta = 0
                                print "WARNING"
                                print f.__name__
                                print x
                                print f(x)
                                print gradiente(f, x)
                                print "+"*30
                                print "+"*30
                else:
                        beta = 0
                d = -1*gradiente(f, next_x) + beta*d
                iteraciones += 1
                x = next_x
        return x, iteraciones

def graph_desempenio(desempenio):
	X = [i*1.0/5 for i in xrange(50)]
	Y = []
	for i in xrange(4):                                   
		l=[]
		for x in X:
			cant=0
			for val in desempenio[i]:
				if val<=x:
					cant+=1
			l.append(cant*1.0/len(desempenio[0]))
		Y.append(l)
	plt.plot(X, Y[0], 'r.', X, Y[1], 'bs', X, Y[2], 'g^', X, Y[3], 'm.')
	plt.show()
