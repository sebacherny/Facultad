set companies := { read "companies.txt" as "<s+>" } ;
set territories := { read "territories.txt" as "<1n>" } ;

param E[territories] := read "territories.txt" as "<1n> 3n" ;
param p[territories*companies] := read "ofrecen_servicio.txt" as "<2n, 1s> 3n" comment "##" ; 
param precio[<i,j> in companies*territories] := read "precioPorTerritorioSegunCompartan.txt" as "<1s, 2n> 3n" ;


var x[<i,j> in companies*territories] integer;


minimize cost : sum<i,j> in companies*territories : precio[i,j]*x[i,j];
subto totalEscuelas : forall <j> in territories:
			sum <i> in companies: x[i,j] == E[j];
subto asignoSoloSiPuedo: forall <j,i> in territories*companies:
				if p[j,i] == 0
				then x[i,j] == 0 end;
