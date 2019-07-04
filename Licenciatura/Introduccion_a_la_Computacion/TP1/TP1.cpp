#include<iostream>
#include<string>

using namespace std;

int getNumberFromString(string s){
	int number=0; // Este sera el numero que obtendremos de la string s.
	int len=s.size();
	for(int i=0; i<len; i++){
		/* Este for recorre los caracteres de la string de izquierda, que
		 * deberan ser todos numericos, es decir estar entre 0 y 9 inclusive.
		 * Como la string sera la representacion en el sistema decimal del numero,
		 * si tengo los primeros x caracteres, agregar al numero el caracter x+1-esimo sera
		 * multiplicar al numero que tengo por 10 (para tener el mismo numero con un 0
		 * a la derecha, y luego sumar el caracter x+1-esimo.
		 * Multiplicar por 10 es facil porque siempre vengo guardando en number un numero
		 * entero, pero para sumar el caracter como el numero que contiene debere hacer
		 * "caracter - '0'", ya que resta los valores ascii entre el caracter dado y el 0.
		 */
		number=number*10+(s[i]-'0');
	}
	return number;
}

bool esPrimo(int n){
	if(n==1)return false; // Este primer caso lo veo a mano para que valga lo explicado abajo.
	for(int i=2; i*i<=n; i++){
		/* Voy a ver si i es un divisor de n. Si encuentro alguno que divida a n, ya se que el
		 * numero no es primo. De lo contrario, lo es.
		 * Para esto, basta con buscar hasta la raiz cuadrada del numero, ya que si i es un
		 * divisor de n, luego n/i tambien (y asi i*(n/i)=n ). Entonces, si i>sqrt(n) y
		 * (n/i)>sqrt(n) ==> i*(n/i) = n > sqrt(n)*sqrt(n) = n, absurdo. Luego, si n tiene algun
		 * divisor, seguro tendra alguno menor o igual a su raiz cuadrada.
		 * Si n=1, no se entraria nunca a este ciclo y la funcion devovleria true, por eso el
		 * primer if de la funcion.
		 */
		if(n%i==0)return false;
	}
	return true;
}

int cantidadPrimosMenoresOIguales(int n){
	int cant=0;
	for(int i=2; i<=n; i++){
		// i recorre todos los numeros menores o iguales a n desde el 2 (el primer numero primo).
		cant+=esPrimo(i); //sumar esPrimo(i) como numero es como sumar 1 si da true y 0 si no.
	}
	return cant;
}

int cantidadDivisoresPrimos(int n){
	int d=2; // El primer posible divisor primo es el 2, ya que es el primer numero primo.
	
	int cant=0; // Esta variable cuenta la cantidad de divisores primos, el valor a devolver.
	
	while(n>1){ // A n lo voy a ir dividiendo por sus divisores primos, y va a dejar de tener divisores primos cuando llegue a 1 (es decir, le voy sacando de a un divisor primo de su factorizacion hasta que no hay mas).
		if(n%d==0){
			cant++; // sabemos que d es primo por el comentario (#) de mas abajo
		}
		while(n%d==0){
			/* (#) aca, garantizamos que los divisores d que dividen a n son primos, sacandole todas
			* las apariciones de d de su factorizacion. Es decir, si tuvieramos que d=p*d' y d|n para algun
			* primo p y d' distinto de 1, entonces p<d, por lo que al numero p lo revisamos antes,
			* pero en ese caso dividimos a n por p tantas veces como aparecia en su factorizacion,
			* absurdo porque d|n => p|n . Entonces si d|n a esta altura del algoritmo, debe ser primo.
			*/
			n/=d;
		}
		d++;
	}
	return cant;
}

/* En las siguientes funciones, uso basicamente la misma idea de "cantidadDivisoresPrimos" de
 * dividir a n por su divisor d tantas veces como aparezca en la factorizacion de d
 * para garantizar que si d|n, d es primo.
 */

int iesimoDivisorPrimo(int n, int i){
	int d=2;
	int cant=0; // Esta variable nuevamente contara la cantidad de divisores primos que vamos encontrando.
	while(n>1){
		if(n%d==0){
			cant++;
			if(cant==i){
				// Cuando estamos contando el iesimo numero primo, queremos devolverlo.
				return d;
			}
		}
		while(n%d==0){
			n/=d;
		}
		d++;
	}
	/* La funcion llega aca si nunca evaluo como true la expresion cant==i,
	 * y como cant empieza en 0 y cada vez que suma 1 se compara en esa expresion,
	 * e i es natural, si nunca dio true es porque cant<i, entonces devuelve -1.
	 */
	return -1;
}

int potenciaIesimoDivisorPrimo(int n, int i){
	int d=2;
	int cant=0; // Este contador nuevamente cuenta cuantos divisores primos vamos encontrando
	while(n>1){
		if(n%d==0){
			cant++;
			if(cant==i){
				/* Si cant vale i, es que estamos en el iesimo primo, por lo que queremos saber
				 * cuantas veces aparece. Inicializo exponente en 0 y mientras d|n,
				 * sumo uno a exponente y divido a n por d.
				 */
				int exponente=0;
				while(n%d==0){
					n/=d;
					exponente++;
				}
				return exponente;
			}
		}
		while(n%d==0){
			n/=d;
		}
		d++;
	}
	/* Al igual que en la funcion "iesimoDivisorPrimo", si la funcion llega hasta aca es que
	 * la cantidad  divisores primos de n es menor que i, por lo que la funcion devuelve -1.
	 */
	return -1;
}



int main(int argc, char *argv[]){
	if(argc<3){
		// Todas las funciones reciben al menos un parametro, por lo que al menos debera haber 3 parametros (se incluye el nombre del ejecutable).
		cout<<"No se ingresaron suficientes parametros. Se debe indicar nombre de la funcion y sus parametros."<<endl;
		return 0;
	}
	
	string functionName = argv[1]; // Esta variable guarda el nombre de la funcion a ejecutar, ya que argv[0] sera "./nombreEjecutable".
	string firstNumberString = argv[2]; // Esta variable guarda como string el primer numero dado como parametro (n en todas las funciones).
	int firstNumber = getNumberFromString(firstNumberString);
	/* Esta variable guarda a "n" como numero entero (la funcion esta explicada en su codigo).
	 * Lo hago de esta manera ya que hice el tp en mi casa y no pude probarlo con el g++
	 * del dc, y en mi computadora debo importar la biblioteca stdlib.h para poder usar atoi.
	 * Creo que es una sutileza que no hace al tp, pero bueno, hice la funcion para que
	 * si por alguna razon debiera usar "atoi", solo se debe cambiar cada aparicion de esta
	 * funcion por atoi y todo andaría igual.
	 */
	 
	
	int secondNumber; // Esta variable solo la inicializo mas abajo, si existe un segundo numero.
	if(argc>3){
		string secondNumberString = argv[3];
		secondNumber = getNumberFromString(secondNumberString);
		/* Al igual que con "firstNumber", obtengo el segundo numero entero dado como parametro
		 * en el caso de que exista, que garantizo con el if(argc>3)
		 */
	}
	
	/* Aca abajo simplemente evaluo el valor del nombre de la funcion dado al llamar al ejecutable
	 * y segun su valor, ejecuto la funcion pedida con sus parametros debidos.
	 */
	if(functionName=="esPrimo"){
		if (esPrimo(firstNumber)){
			cout << "si" << endl;
		}else{
			cout << "no" << endl;
		}
	}else if(functionName=="cantidadPrimosMenoresOIguales"){
		cout << cantidadPrimosMenoresOIguales(firstNumber) << endl;
	}else if(functionName=="cantidadDivisoresPrimos"){
		cout << cantidadDivisoresPrimos(firstNumber) << endl;
	}else if(functionName=="iesimoDivisorPrimo"){
		int respuesta = iesimoDivisorPrimo(firstNumber, secondNumber); // Guardo lo que devuelve la funcion
		if(respuesta!=-1){
			// Si n tiene por lo menos i divisores primos, muestro en pantalla el iesimo.
			cout << respuesta << endl;
		}else{
			// Si no, muestro la frase debida en pantalla.
			cout << firstNumber << " no tiene " << secondNumber << " divisores primos"<<endl;
		}
	}else if(functionName=="potenciaIesimoDivisorPrimo"){
		int respuesta = potenciaIesimoDivisorPrimo(firstNumber, secondNumber);
		if(respuesta!=-1){ // Esto es igual al caso anterior
			cout << respuesta << endl;
		}else{
			cout << firstNumber << " no tiene " << secondNumber << " divisores primos"<<endl;
		}
	}else{
		cout<<"Funcion invalida"<<endl;
	}
}
