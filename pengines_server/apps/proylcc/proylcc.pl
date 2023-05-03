:- module(proylcc, 
	[  
		join/4,               % Evolución de la combinación de celdas del camino
		suma_camino_pot_dos/3 % Computa el resultado de la suma de un camino
	]).

%:- use_module(library(random)).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	RGrids = [[0 | Ns], [N2 | Ns]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones de generación numérica %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * gen_random_pot(+LimInferior, +LimSuperior, -Random)
 * 
 * Calcula una potencia de dos aleatoria a partir de un rango de potencias posibles (relacionadas con las potencias en juego).
 * LimInferior debe ser, como mínimo, 1.
 */
gen_random_pot(LimInferior, LimSuperior, Rand):-
	random(LimInferior, LimSuperior, RandAux), %random/3 existe; genera un random pertenenciente al rango [LimInferior, LimSuperior)
	Rand is 2 ^ RandAux.

/**
 * pot_dos(+N, -Pot)
 * 
 * Calcula la potencia de dos más cercana y mayor que N (o N, en caso de ser potencia de dos).
 */
pot_dos(N, Pot):-
	Pot is 2 ^ ceiling((log(N)/log(2))). %Cálculos aritméticos que determinan la menor potencia de dos buscada.
									     %ceiling/1 existe.

/**
 * indice_raiz(+N, -Pot)
 * 
 * Computa el índice de una raíz con radicando N (donde N es potencia de 2); se desea saber cual es el exponente tal que 2 ^ Pot = N
 * Se utilizan propiedades logarítmicas:
 *    --> Definición de logaritmo: log_a(b) = c <==> a ^ c = b
 *    --> Propiedad de cambio de base del logaritmo: log_a(b) = log(b)/log(a)
 */
indice_raiz(N, Indice):-
    Indice is round(log(N)/log(2)).

/**
 * generar_rango(+Grilla, -LimInferior, -LimSuperior)
 * 
 * Computa los límites de un rango para generar potencias aleatorias basadas en los valores actuales de la grilla
 */
generar_rango(Grilla, LimInferior, LimSuperior):-
	max_grilla(Grilla, Max),
	indice_raiz(Max, Indice),
	LimSuperior is Indice,
	get_limite_inferior(LimSuperior, LimInferior).

/**
 * get_limite_inferior(+LimSuperior, -LimInferior)
 * 
 * Computa un límite inferior de un rango a partir de un límite superior; la diferencia entre uno y otro es de, a lo sumo, 8 unidades.
 */
get_limite_inferior(LimSuperior, LimInferior):-
	(LimSuperior > 8, LimInferior is LimSuperior - 8);
	LimInferior is 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones de manipulación de listas %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * suma_camino(+Grilla, +Camino, -Suma).
 *
 * Calcula la suma de los números de un camino. El camino contiene las ubicaciones relativas de los elementos en la grilla.
 */
suma_camino(_, [], 0).
suma_camino(Grilla, [[X | Y] | Camino], Suma):-
	get_elemento(Grilla, X, Y, Elem),
	suma_camino(Grilla, Camino, SumaAux),
	Suma is SumaAux + Elem.

/**
 * suma_camino_pot_dos(+Grilla, +Camino, -Suma).
 *
 * Calcula el resultado de los números de un camino. Busca la potencia de dos más próxima e inferior a esa suma.
 */
suma_camino_pot_dos(Grilla, [[X | Y] | Camino], SumaPot2):-
	suma_camino(Grilla, [[X | Y] | Camino], Suma),
	pot_dos(Suma, SumaPot2).

/**
 * max_numero(+Lista, -Max).
 *
 * Encuentra el mayor elemento de la lista Lista y lo unifica en Max.
 */
max_numero(Lista, Max):-
	max_list(Lista, Max). %max_list/2 existe.

/**
 * ultimo(?Lista, ?X)
 * 
 * Encuentra el último elemento de una lista y lo unifica en X
 */
ultimo([X], X).
ultimo([_ | Xs], U) :-
    ultimo(Xs, U).

/**
 * aplanar(+Grilla, -ListaAplanada)
 * 
 * Dado que una grilla es una lista de listas, se computa una única lista conteniendo todos los elementos de la grilla
 */
aplanar([], []).
aplanar([Lista|Resto], Aplanada) :-
    aplanar(Resto, NuevaAplanada),
    append(Lista, NuevaAplanada, Aplanada).

/**
 * camino_posiciones(+ListaCoordenadas, +CantColumnas, -ListaPosiciones)
 * 
 * Similar a ubicacion_en_grilla (ver más abajo).
 * Crea una lista de índices que refieren a la ubicación relativa de cada coordenada de la grilla, comenzando en cero.
 */
camino_posiciones([], _, []).
camino_posiciones([[F | C] | Coordenadas], CantColumnas, [Pos | PosRestantes]):-
	ubicacion_en_grilla(F, C, CantColumnas, Pos),
	camino_posiciones(Coordenadas, CantColumnas, PosRestantes).
	
/**
 * cambiar_todas(+Elem, +LimInferior, +LimSuperior, +ListaCambiar, -ListaCambiada)
 * 
* Cambia todas las apariciones de Elem en ListaCambiar por una potencia de dos generada aleatoriamente a partir de un rango determinado por [LimInferior, LimSuperior) y la unifica en ListaCambiada
 */
cambiar_todas(_, _, _, [], []).
cambiar_todas(X, LimInferior, LimSuperior, [X | Xs], [Y | Z]) :-
    gen_random_pot(LimInferior, LimSuperior, Y),
    cambiar_todas(X, LimInferior, LimSuperior, Xs, Z).
cambiar_todas(X, LimInferior, LimSuperior, [Z | Zs], [Z | R]) :-
    Z \= X,
    cambiar_todas(X, LimInferior, LimSuperior, Zs, R).

/**
 * cambiar_en_pos(+Lista, +Indice, +Valor, -ListaActualizada)
 * 
 * Cambia el valor de una posición Pos de la lista Lista por Valor. La modificación se refleja en ListaACtualizada.
 */
cambiar_en_pos([_|Resto], 0, NuevoValor, [NuevoValor|Resto]).
cambiar_en_pos([Cabeza|Resto], Index, NuevoValor, [Cabeza|NuevoResto]) :-
    Index > 0,
    NuevoIndex is Index - 1,
    cambiar_en_pos(Resto, NuevoIndex, NuevoValor, NuevoResto).

/**
 * agrupar(+Lista, -ListaAgrupada)
 * 
 * Unifica en ListaAgrupada una lista compuesta por los mismos elementos que Lista pero organizada en duplas.
 *
 * Ejemplo: Lista = [a, b, c, d, e, f]
 *          ListaAgrupada = [[a, b], [c, d], [e, f]]
 */
agrupar([X1, X2], [[X1, X2]]).
agrupar([X1 | [X2 | X3]], [Y | Ys]):-
    Y = [X1, X2],
    agrupar(X3, Ys).

/*
 * get_coordenadas(?Coordenada, ?I, ?J)
 * 
 * Se computan los índices correspondientes a una dupla que representa una coordenada
 */
get_coordenadas(Coordenada, I, J):-
	Coordenada = [I, J].

/*
 * tamanio(+Lista, -Tamaño)
 * 
 * Computa el largo de una lista Lista y lo unifica en Tamaño
 */
tamanio([],0).
tamanio([_|Y], N):-
    tamanio(Y, N1),
    N is N1 + 1.

/*
 * concatenar(+Lista1, +Lista2, -ListaConcatenada)
 * 
 * Dadas dos listas Lista1 y Lista2 computa su concatenación y la unifica, como una nueva lista fruto de ambas, en ListaConcatenada
 */
concatenar([], X, X).
concatenar([X | Xs], Y, [X | Zs]) :-
    concatenar(Xs, Y, Zs).

/*
 * crear_coordenadas(+Filas, +Columnas, -Coordenadas)
 * 
 * Dada una cantidad de filas Filas y una cantidad de columnas Columnas, se crea una lista de coordenadas Coordenadas, con todas las coordenadas existentes de una grilla de cantidad de elemento igual que filas x columnas
 */
crear_coordenadas(Filas, Columnas, Coordenadas) :-
	NroFila is Filas - 1,
	coordenadas_filas(NroFila, Columnas, CoordAux),
    invertir(CoordAux, Coordenadas).
	
/*
 * coordenadas_filas(+NroFila, +CantColumnas, -Coordenadas)
 * 
 * Predicado auxiliar de crear_coordenadas
 * Sabiendo la cantidad de filas que posee una grilla y su cantidad de columnas, se crean, fila a fila, las coordenadas correspondientes a cada una.
 */
coordenadas_filas(0, CantColumnas, Coordenadas):-
	coordenadas_fila_puntual(0, CantColumnas, Coordenadas).
coordenadas_filas(NroFila, CantColumnas, CoordenadasReturn):-
	coordenadas_fila_puntual(NroFila, CantColumnas, CoordenadasAux),
	FilaSiguiente is NroFila - 1, 
	coordenadas_filas(FilaSiguiente, CantColumnas, Coordenadas),
	concatenar(CoordenadasAux, Coordenadas, CoordenadasReturn).        % Se realiza para evitar tener listas, de listas de coordenadas.
	
/*
 * coordenadas_fila_puntual(+NroFila, +NroCol, -Coordenadas)
 * 
 * Predicado auxiliar de crear_coordenadas
 * Dada una fila NroFila y sabiendo la cantidad de elementos que contiene (CantColumnas), se computan todas las coordenadas asociadas a esa fila y se unifican en Coordenadas
 */
coordenadas_fila_puntual(NroFila, 1, [[NroFila, 0]]).
coordenadas_fila_puntual(NroFila, NroCol, [[NroFila, C] | Coordenadas]):-
	C is NroCol - 1,
	coordenadas_fila_puntual(NroFila, C, Coordenadas).

/*
 * invertir(+Lista, -ListaInvertida)
 * 
 * Dada una lista Lista invierte su contendio y lo unifica en ListaInvertida
 */
invertir([], []).
invertir([Y | Ys], Z) :-
    invertir(Ys, Zaux),
    insertar_f(Y, Zaux, Z).

/*
 * insertar_f(+Elemento, +Lista, +ListaNueva)
 * 
 * Dado un elemento Elemento y una lista Lista, se agrega el elemento Elemento al final de la misma y unifica la nueva lista en ListaNueva
 */
insertar_f(X, [], [X]).
insertar_f(X, [Y | Ys], [Y | Zs]) :-
    insertar_f(X, Ys, Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones de manipulación de grilla %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * get_elemento(+Grilla, +Fila, +Columna, -Elem)
 * 
 * Encuentra el elemento Elem ubicando en (Fila, Columna) de la Grilla.
 */
get_elemento(Grilla, Fila, Columna, Elem):-
	nth0(Fila, Grilla, FilaAux),  %nth0/3 existe; FilaAux es la fila ubicada en la posicion Fila.
	ultimo(Columna,  Col),        %Se utiliza el predicado ultimo porque Columna es una lista (por sintaxis de Prolog)
	nth0(Col, FilaAux, Elem).     %               Elem es el elemento ubicado en la posicion columna.

/**
 * set_suma_grilla(+GrillaVieja, +Camino, +Suma, ,-GrillaNueva)
 * 
 * Setea en la última posición del camino la suma computada del camino de elementos de la grilla que se unieron.
 * Camino es una lista que contiene coordenadas.
 */
set_suma_grilla(GrillaVieja, Camino, Suma, GrillaNueva):-
	ultimo(Camino, [NroFila | NroCol]),					     % Busco la coordenada correspondiente
	nth0(NroFila, GrillaVieja, FilaEntera),                  % Guardo la fila completa correspondiente a NroFila
	cambiar_en_pos(FilaEntera, NroCol, Suma, FilaAux),
	cambiar_en_pos(GrillaVieja, NroFila, FilaAux, GrillaNueva).

/**
 * ubicacion_en_grilla(+Fila, +Columna, +CantColumnas, -Pos)
 * 
 * Computa y unifica en Pos, la posición de la lista donde se guardan las coordenadas de los elementos de la grilla, de una coordena particular
 * 
 * Ejemplo gráfico:
 *  [[1, 2, 3],     --> 2 está en (0, 1); 0 * 3 + 1 = 1
 *   [4, 5, 6],     --> 5 está en (1, 1); 1 * 3 + 1 = 4
 *   [7, 8, 9], 
 *   [10, 11, 12]], --> 11 está en (3, 1); 3 * 3 + 1 = 10
 */
ubicacion_en_grilla(Fila, Columna, CantColumnas, Pos):-
	Pos is Fila*CantColumnas + Columna.

/**
 * max_grilla(+Grilla, -Max)
 * 
 * Computa el mayor elemento contenido en la grilla
 */
max_grilla(Grilla, Max):-
	aplanar(Grilla, Lista),
	max_numero(Lista, Max).

/**
 * reemplazar_ceros(+Grilla, +LimInferior, +LimSuperior, -GrillaCompleta)
 * 
 * Dada una grilla conteniendo 0 (representa posiciones a completar), reemplaza esos elementos por valores válidos que son potencias de 2.
 * Estos nuevos valores son potencias de dos aleatorias generadas a partir de un intervalo numérico [LimInferior, LimSuperior).
 */
reemplazar_ceros([], _, _, []).
reemplazar_ceros([FilaCeros | RestoFilasCeros], LimInferior, LimSuperior, [Fila | RestoFilas]):-
    cambiar_todas(0, LimInferior, LimSuperior, FilaCeros, Fila),
    reemplazar_ceros(RestoFilasCeros, LimInferior, LimSuperior, RestoFilas).

/**
 * pos_ceros(+Lista, +Indice, -PosCeros).
 * 
 * Computa en una lista las posiciones donde hay ceros. 
 * El mapeo de los indices donde los hay corresponden a un orden de posiciones de coordenadas de matriz (ver camino_posiciones más arriba).
 * Indice determina el inicio del mapeo de posiciones.
 * Lista es una lista conteniendo los elementos de la grilla en forma aplanada.
 */
pos_ceros([], _, []).
pos_ceros([0 | Lista], Indice, [Indice | PosCeros]):-
	IndiceAux is Indice + 1,
	pos_ceros(Lista, IndiceAux, PosCeros).
pos_ceros([_ | Lista], Indice, PosCeros):-
	IndiceAux is Indice + 1,
	pos_ceros(Lista, IndiceAux, PosCeros).

/**
 * pos_ceros_grilla(+Grilla, +GrillaCoordenadas, -Coordenadas)
 * 
 * Dada una grilla Grilla con valores numéricos, una GrillaCoordenadas conteniendo coordenadas de la matriz, crea una lista Coordenadas que contiene las duplas donde hay elementos que son igual que cero.
 */
pos_ceros_grilla(Grilla, GrillaCoordenadas, Coordenadas):-
	aplanar(Grilla, ListaAplanada),
	pos_ceros(ListaAplanada, 0, PosCeros),
	findall(Y, (member(X, PosCeros), unificacion_posIndex_posCoordenadas(GrillaCoordenadas, X, ListaCoordenadas),
				member(Y, ListaCoordenadas)), CoordenadasCeros),
	agrupar(CoordenadasCeros, Coordenadas).

/**
 * unificacion_posIndex_posCoordenadas(+ListaCoordenadas, +Indice, -Coordenada)
 * 
 * Computa la coordenada relativa a un indexado de posición de la grilla en juego (ver ubicacion_en_grilla mas arriba)
 */
unificacion_posIndex_posCoordenadas([[F, C] | _], 0, [F, C]).
unificacion_posIndex_posCoordenadas([_ | FCs], Indice, Rta):-
    I is Indice - 1,
    unificacion_posIndex_posCoordenadas(FCs, I, Rta).	

/*
 * set_ceros_grilla(+Grilla, +ListaCoordenadas, -GrillaCeros)
 * 
 * Dada una grilla Grilla y una lista conteniendo coordenadas ListaCoordenadas, establece en los índices de Grilla que pertenezcan a ListaCoordenadas, un cero como elemento.
 */
set_ceros_grilla(Grilla, [], Grilla).
set_ceros_grilla(GrillaVieja, [Primera | Resto], GrillaNueva):-
    get_coordenadas(Primera, NroFila, NroCol),
    nth0(NroFila, GrillaVieja, Fila),
    cambiar_en_pos(Fila, NroCol, 0, FilaNueva),
    cambiar_en_pos(GrillaVieja, NroFila, FilaNueva, GrillaAux),
    set_ceros_grilla(GrillaAux, Resto, GrillaNueva).