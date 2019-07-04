set companies := { read "companies.txt" as "<s+>" } ;
set territories := { read "territories.txt" as "<1n>" } ;
set I := { read "intervalos.txt" as "<1n>" comment "#" } ;

param E[territories] := read "territories.txt" as "<1n> 3n" ;
param p[territories*companies] := read "ofrecen_servicio.txt" as "<2n, 1s> 3n" comment "##" ; 
param minT[I] := read "intervalos.txt" as "<1n> 2n" comment "#" ; #<1> 0, <2> 20, <3> 40, <4> 60, <5> 80, <6> 100, <7> 150, <8> 200, <9> 300, <10> 400, <11> 500, <12> 600, <13> 700;
param maxT[I] := read "intervalos.txt" as "<1n> 3n" comment "#" ; #<1> 19, <2> 39, <3> 59, <4> 79, <5> 99, <6> 149, <7> 199, <8> 299, <9> 399, <10> 499, <11> 599, <12> 699, <13> 709;
param c[I*companies] := read "precios.txt" as "<1n, 2s> 3n" skip 1 ;

var x[<i,j> in companies*territories] integer;
var y[<i,t> in companies*I] binary;
var z[<i,t> in companies*I] integer;
var esAsignado[companies] binary;



minimize cost : sum<i,t> in companies*I : c[t,i]*z[i,t];
subto totalEscuelas : forall <j> in territories:
			sum <i> in companies: x[i,j] == E[j];
subto caenEnRango : forall <i,t> in companies*I:
			maxT[t] >= sum <j> in territories : y[i,t]*x[i,j]
			and
			y[i,t]*minT[t] <= sum <j> in territories : x[i,j]
			;
subto asignoTodasEnRango : forall <i,t> in companies*I:
		z[i,t] >= y[i,t] * (sum <j> in territories : x[i,j]) ;
subto unicoRango: forall <i> in companies:
			sum <t> in I : y[i,t] == 1;
subto asignoSoloSiPuedo: forall <j,i> in territories*companies:
				if p[j,i] == 0
				then x[i,j] == 0 end;
subto asignoTodoEnAlgunRango: forall <i> in companies:
				sum <t> in I: z[i,t] == sum <j> in territories: x[i,j];
subto seteoNuevaVariable: forall <i> in companies:
				esAsignado[i] <= sum <t> in I: z[i,t]*y[i,t];
subto alMenosDos: sum <i> in companies: esAsignado[i] >= 2;
