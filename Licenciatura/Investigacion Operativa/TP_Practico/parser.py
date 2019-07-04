from re import *
import subprocess

Success = "SCIP Status        : problem is solved [optimal solution found]\n"

def SCIP(parametros):
	#
	# Esta funcion deberia armar el modelo de programacion lineal. No tiene argumentos definidos, tienen que completarlos.
	#

	with open("output.txt", "w") as f:
		
		os.system("./zimpl modelo.zpl") # genera el archivos .lp
		os.system("./scip -q -l output.txt -f modelo.lp") # corre el solver SCIP en el modelo y escribe el resultado en el archivo output.txt

	# chequea si el problema fue resuelto

	with open("output.txt", "r") as f:
		fi = f.readlines()
	ans = Success in fi

	return ans

def solve():
	#
	# Estas lineas abren el libro ("book.txt") y lo parsean de forma util en la variable text.
	# Concretamente, limpian todos los signos de puntuacion exceptos los '.' y transforman todo
	# a minuscula. La variable text es una lista con las oraciones del libro.
	#

	regenter = compile('(\n)\\1+')
	regspace = compile('( )\\1+')
	regex = compile('[^a-zA-Z .]')

	with open("book.txt", "r") as f:
  		text = f.read()
	
	text = regex.sub('', regspace.sub(' ',regenter.sub(' ', text)) ).lower().split('.')
	
	#
	# Vamos a recorrer todos los fragmentos (listas de oraciones consecutivas) que tengan longitud total (suma de letras) entre lo y hi.
	#

	l = 0
	r = 0
	totlen = 0
	L = 13 # esta es la longitud total del nombre del autor, habria que adaptarla segun cada libro.
	lo = 7 * L
	hi = 13 * L

	right = True
	i = 0
	while (l <= r) and r < len(text):
		# Cada fragmento consta de todas las oraciones comprendidas entre los indices l inclusive y r exclusive.
		if right:
			linea = text[r]
			for c in linea:
				cod = ord(c) - ord('a')
				if(0 <= cod and cod < 26):
					totlen += 1

			r += 1
			if totlen > hi:
				right = False
			else:				
				if totlen >= lo:
					print text[l:r]									
					# Corro SCIP sobre este parrafo y actuo en consecuencia
					# ans = SCIP(parametros)
		else:
			linea = text[l]
			for c in linea:
				cod = ord(c) - ord('a')
				if(0 <= cod and cod < 26):
					totlen -= 1
			l += 1
			if totlen < lo:
				right = True
			else:
				if totlen <= hi:
					print text[l:r]
					# Corro SCIP sobre este parrafo y actuo en consecuencia
					# SCIP(parametros)					
	print "Mala suerte"
	return

solve()
