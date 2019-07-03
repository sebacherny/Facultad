
#include<bits/stdc++.h>
using namespace std;

#define forn(i,n) for(int i=0;i<(int)(n); i++)
#define pb push_back
#define mp make_pair

struct comprador{
    int initialRow, initialCol;
    int actualRow, actualCol;
    bool hasProduct;
    int tarda;
};

struct posDis{
    int x, y, dis;
};

struct local{
    int x, y;
    int ind;
    bool vende;
};

int R, C;
vector< vector<char> > tablero;
vector<comprador> compradores;
vector<local> locales;
vector< vector< posDis > > distFromCompradores;
map< pair<int,int>, int> indiceLocal;
int dx[4] = {1, -1, 0, 0};
int dy[4] = {0, 0, 1, -1};


bool adentro(int x, int y){
    return x>=0 && x<R && y>=0 && y<C;
}

map< pair<int,int>, bool> compradorPaso;
vector<int> compranEnLocal(50*50, 50*50);

local llevoALocalMasCercanoSinVisitar(int i, vector<bool> paso){
    comprador com = compradores[i];
    queue<posDis> q;
    vector< vector<bool> > visitado(R, vector<bool>(C, false));
    posDis inicial;
    inicial.x=com.actualRow;
    inicial.y=com.actualCol;
    inicial.dis=0;
    q.push(inicial);
    visitado[inicial.x][inicial.y]=true;
    local voy;
    voy.x=-1;
    int menorDistancia=50*50;
    while(!q.empty()){
        posDis actual = q.front();
        q.pop();
        if(tablero[actual.x][actual.y]=='L'){
            int indice = indiceLocal[mp(actual.x, actual.y)];
            if(!paso[indice]){
                local estoy = locales[indice];
                if(actual.dis<menorDistancia || (actual.dis==menorDistancia && mp(estoy.x, estoy.y)<mp(voy.x, voy.y))){
                    menorDistancia = actual.dis;
                    voy=estoy;
                }
            }
        }
        forn(d, 4){
            int nx=actual.x+dx[d];
            int ny=actual.y+dy[d];
            if(adentro(nx, ny) && !visitado[nx][ny] &&  tablero[nx][ny]!='X'){
                visitado[nx][ny]=true;
                posDis newVal;
                newVal.x=nx;
                newVal.y=ny;
                newVal.dis=actual.dis+1;
                q.push(newVal);
            }
        }
    }
    if(voy.x!=-1){
		compradores[i].actualRow = voy.x;
		compradores[i].actualCol = voy.y;
		compradores[i].tarda+=menorDistancia;
	}
    //cout<<"el comprador de "<<compradores[i].initialRow<<","<<compradores[i].initialCol<<endl;
    //cout<<"va al local en "<<voy.x<<","<<voy.y<<" tardando "<<compradores[i].tarda<<endl;
    //cout<<"-----------------------"<<endl;
    return voy;
}

struct evento{
    int c;
    int l;
    int tiempo;
};

bool compEventos(evento a, evento b){
    return a.tiempo<b.tiempo;
}

int getValueEgoistas(){
    vector< vector<bool> > paso(compradores.size(), vector<bool>( locales.size()));
    vector< vector< pair<int, int> > > tiempos(compradores.size());
    forn(i, compradores.size()){
        forn(intento, locales.size()){
            local va = llevoALocalMasCercanoSinVisitar(i, paso[i]);
            if(va.x!=-1){
                compranEnLocal[va.ind]=min(compranEnLocal[va.ind], compradores[i].tarda);
                paso[i][va.ind]=true;
                tiempos[i].pb(mp(va.ind, compradores[i].tarda));
            }else{
				break;
			}
        }
    }
    vector<evento> eventos;
    forn(i, compradores.size()){
        forn(j, tiempos[i].size()){
            evento ev;
            ev.c=i;
            ev.l=tiempos[i][j].first;
            ev.tiempo=tiempos[i][j].second;
            eventos.pb(ev);
        }
    }
    sort(eventos.begin(), eventos.end(), compEventos);
    int ultimo=0;
    forn(i, eventos.size()){
        evento ev=eventos[i];
        if(locales[ev.l].vende && !compradores[ev.c].hasProduct){
            ultimo=ev.tiempo;
            locales[ev.l].vende=false;
            compradores[ev.c].hasProduct=true;
        }
    }
    return ultimo;
}


vector<bool> visitada;
vector<int> unidaA;

bool hayMatchingParaLaPersona(int persona, vector< vector<int> > & grafo){
    forn(i, grafo[persona].size()){
        int tarea = grafo[persona][i];
        if (!visitada[tarea]){
            visitada[tarea] = true;
            if (unidaA[tarea] == -1 || hayMatchingParaLaPersona(unidaA[tarea], grafo) ){
                unidaA[tarea] = persona;
                return true;
            }
        }
    }
    return false;
}


int maximoMatching(vector< vector<int> > & grafo){
    unidaA.assign(locales.size(), -1);
    int result = 0;
    forn(persona, compradores.size()){
        visitada.assign(locales.size(), false);
        if (hayMatchingParaLaPersona(persona, grafo)){
            result++;
        }
    }
    return result;
}


int getValueCooperando(){
    int INF = 50*3;
    int lo=-1, hi=INF;
    while(hi-lo>1){
        int mi=(hi+lo)/2;
        vector< vector<int> > graphInBinary;
        forn(i, distFromCompradores.size()){
            vector<int> compradorActual;
            forn(j, distFromCompradores[i].size()){
                posDis miro = distFromCompradores[i][j];
                if(miro.dis<=mi){
                    compradorActual.pb(indiceLocal[mp(miro.x, miro.y)]);
                }
            }
            graphInBinary.pb(compradorActual);
        }
        if(maximoMatching(graphInBinary)==(int)compradores.size()){
            hi=mi;
        }else{
            lo=mi;
        }
    }
    return (hi==INF ? -1 : hi);
}

void initializeDistFromCompradores(){
    forn(i, compradores.size()){
        comprador actual = compradores[i];
        vector< posDis > distFromComprador;
        queue< posDis > q;
        vector< vector<bool> > visitado(R, vector<bool>(C, false));
        posDis inicial;
        inicial.x=actual.initialRow;
        inicial.y=actual.initialCol;
        inicial.dis=0;
        q.push(inicial);
        visitado[actual.initialRow][actual.initialCol]=true;
        while(!q.empty()){
            posDis now = q.front();
            if(tablero[now.x][now.y]=='L'){
                //cout<<"distancia : "<<now.dis<<endl;
                distFromComprador.pb(now);
            }
            q.pop();
            forn(d, 4){
                int nx=now.x+dx[d];
                int ny=now.y+dy[d];
                if(adentro(nx, ny) && !visitado[nx][ny] && tablero[nx][ny]!='X'){
                    visitado[nx][ny]=true;
                    posDis newVal;
                    newVal.x=nx;
                    newVal.y=ny;
                    newVal.dis=now.dis+1;
                    q.push(newVal);
                }
            }
        }
        distFromCompradores.pb(distFromComprador);
    }
}

void solve(){
    forn(i, R){
        vector<char> row;
        forn(j, C){
            char x;
            cin>>x;
            row.pb(x);
            if(x=='C'){
                comprador com;
                com.initialRow=i;
                com.initialCol=j;
                com.actualRow=i;
                com.actualCol=j;
                com.hasProduct=false;
                com.tarda=0;
                compradores.pb(com);
            }else if(x=='L'){
                local l;
                l.x=i;
                l.y=j;
                l.ind=locales.size();
                l.vende=true;
                indiceLocal[mp(i,j)]=locales.size();
                locales.pb(l);
            }
        }
        tablero.pb(row);
    }
    initializeDistFromCompradores();
    int cooperando = getValueCooperando();
    int egoistas = getValueEgoistas();
    cout<<cooperando<<" "<<egoistas<<endl;
}

int main(){
    while(cin>>R>>C && R){
        tablero.clear();
        compradores.clear();
        locales.clear();
        distFromCompradores.clear();
        indiceLocal.clear();
        solve();
    }
}
