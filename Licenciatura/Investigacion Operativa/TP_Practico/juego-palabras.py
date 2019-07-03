#!/usr/bin/python
# -*- coding: utf-8 -*-

from re import *
import sys
import time

MIN_LENGTH = 7
MAX_LENGTH = 12

def get_diccionario():
	d=open("/home/cherny/CF/diccionario.txt", "r")
	ret={}
	for pal in d.readlines():
		add = pal.replace("\n", "").replace("'", "")
		if len(add)>=MIN_LENGTH and len(add)<=MAX_LENGTH:
			if add[0] not in ret: ret[add[0]]=[]
			ret[add[0]].append(add)
	return ret

diccionario = get_diccionario()

def solveRecursive(letras, nombre, tengo=[]):
	if nombre=="" and all(v==0 for v in letras.itervalues()): return tengo
	if nombre=="": return []
	quedan = sum(letras.itervalues())
	if quedan<len(nombre)*MIN_LENGTH or quedan>len(nombre)*MAX_LENGTH: return []
	if any(nombre.count(x)>letras[x] for x in nombre): return []
	for pal in diccionario[nombre[0]]:
		if len(pal)>=MIN_LENGTH and len(pal)<=MAX_LENGTH and all(letras[c]>=pal.count(c) for c in pal):
			#if len(tengo)<=3:
			#	print "puedo usar ", pal, len(tengo)
			#	print time.strftime("%H:%M")
			tengo.append(pal)
			for c in set(pal): letras[c]-=pal.count(c)
			x = solveRecursive(letras, nombre[1:], tengo)
			if x: return x
			tengo.remove(pal)
			for c in set(pal): letras[c]+=pal.count(c)
	return []
			

def solveBacktracking(line, nombre):
	line = line.lower()
	letras = {}
	for i in xrange(ord('a'), ord('z')+1): letras[chr(i)]=0
	for i in line:
		if i>='a' and i<='z':
			if not i in letras: letras[i]=0
			letras[i]+=1
	for i in nombre:
		if not i in line or nombre.count(i)>letras[i]:
			return []
	return solveRecursive(letras, nombre)

def solve():
	#
	# Estas lineas abren el libro ("book.txt") y lo parsean de forma util en la variable text.
	# Concretamente, limpian todos los signos de puntuacion exceptos los '.' y transforman todo
	# a minuscula. La variable text es una lista con las oraciones del libro.
	#

	regenter = compile('(\n)\\1+')
	regspace = compile('( )\\1+')
	regex = compile('[^a-zA-Z .]')

	with open("/home/cherny/ozmaofoz.txt", "r") as f:
  		text = f.read()
	
	text = regex.sub('', regspace.sub(' ',regenter.sub(' ', text)) ).lower().split('.')
	
	#
	# Vamos a recorrer todos los fragmentos (listas de oraciones consecutivas) que tengan longitud total (suma de letras) entre lo y hi.
	#

	l = 0
	r = 0
	totlen = 0
	# nombre="Hume Fergus".lower().replace(" ", "") # book3
	# nombre="Agassiz Louis".lower().replace(" ", "") # book4
	# nombre="Willett Edward".lower().replace(" ", "") # book5
	nombre="Ozma of Oz".lower().replace(" ", "") # book5
	L = len(nombre) # esta es la longitud total del nombre del autor, habria que adaptarla segun cada libro.
	lo = MIN_LENGTH * L
	hi = MAX_LENGTH * L
	for line in text[100:]:
		if len(line)>=lo and len(line)<=hi:
			#line="ypointing oppugners"
			print line
			juego = solveBacktracking(line, nombre)
			if juego:
				print line
				print juego
				return

solve()
