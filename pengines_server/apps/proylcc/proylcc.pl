:- module(proylcc, 
	[  
		join/4,                % Evolución de la combinación de celdas del camino
		suma_camino_pot_dos/3, % Computa el resultado de la suma de un camino
		booster/3              %
	]).

:- use_module(library(clpfd)).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Path, RGrids):-
	/*
	 * Dado que los predicados: 
	 *    suma_camino_pot_dos/3,
	 *    set_suma_grilla/4,
	 *    set_ceros_grilla/3,
	 *    burbujear_ceros/2,
	 *    reemplazar_ceros/4
	 * manipulan las grillas como una lista de filas, se unifica en GrillaAgrupada una representación de Grid con esta estructura                     
	 */
	agrupar(Grid, NumOfColumns, GrillaAgrupada),
	suma_camino_pot_dos(GrillaAgrupada, Path, Suma),
	set_suma_grilla(GrillaAgrupada, Path, Suma, GrillaSuma),
	borrar_ultimo(Path, PathSinUltimo), %Se busca el Path sin el ultimo elemento porque sino setea un cero en el lugar que debe contener a la suma de elementos del camino
	set_ceros_grilla(GrillaSuma, PathSinUltimo, GrillaCeros),
	burbujear_ceros(GrillaCeros, GrillaBurbujeada),
	generar_rango(GrillaSuma, LimInferior, LimSuperior), 
	reemplazar_ceros(GrillaBurbujeada, LimInferior, LimSuperior, GrillaCompleta),
	aplanar(GrillaCeros, GrillaCerosAplanada),
	aplanar(GrillaBurbujeada, GrillaBurbujeadaAplanada),
	aplanar(GrillaCompleta, GrillaCompletaAplanada),
	RGrids = [GrillaCerosAplanada, GrillaBurbujeadaAplanada, GrillaCompletaAplanada]. % Corrección devolución

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

/*
 * agrupar(+Lista, +CantColumnas, -Grilla)
 * 
 * Dada una lista Lista y una antidad de columnas CantColumnas unifica en Grilla una lista de listas de cantidad CantColumnas de los elementos de Lista
 */
agrupar([], _, []).
agrupar(Lista, CantColumnas, [Fila|RestoFilas]) :-
    tamanio(Fila, CantColumnas),
    append(Fila, Resto, Lista),
    agrupar(Resto, CantColumnas, RestoFilas).
	
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
cambiar_en_pos([_|Resto], [0], NuevoValor, [NuevoValor|Resto]).
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
agrupar(Lista, ListaAgrupada):-
    agrupar(Lista, 2, ListaAgrupada).

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
 * borrar_ultimo(+Lista, -ListaSinUltimo)
 * 
 * Dada una lista Lista unifica en ListaSinUltimo a Lista sin su ultimo elemento
 */
borrar_ultimo([_], []).
borrar_ultimo([X | Xs], [X | Z]) :-
    borrar_ultimo(Xs, Z).

/*
 * mover_ceros_izquierda(+Lista, -ListaBurbujeada)
 * 
 * Desplaza todos los elementos iguales que cero al inicio de la lista.
 */
mover_ceros_izquierda(Lista, ListaBurbujeada):-
    findall(X, (member(X, Lista), X = 0), Ceros),
    findall(Y, (member(Y, Lista), Y\=0), NoCeros),
    append(Ceros, NoCeros, ListaBurbujeada).

/*
 * elimina_repetidos(+Lista, -ListaSinRepetidos)
 * 
 * Dada una lista Lista, unifica en ListaSinRepetidos los elementos de Lista de forma única.
 */
elimina_repetidos([], []).
elimina_repetidos([X | Xs], Ys) :-
    buscar(X, Xs),
    elimina_repetidos(Xs, Ys).
elimina_repetidos([X | Xs], [X | Ys]) :-
    elimina_repetidos(Xs, Ys).

/*
 * buscar(+Elem, +Lista)
 * 
 * Determina si Elem pertence a Lista
 */
buscar(X, [Y | Ys]) :-
    X = Y;
    buscar(X, Ys).

/*
 * comparten_elementos(+Lista1, +Lista2)
 * 
 * Determina si existe algún elemento tal que pertenece tanto a Lista1 como a Lista2.
 */
comparten_elementos(Lista1, Lista2):-
	buscar(Elem, Lista1),
    buscar(Elem, Lista2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operaciones de manipulación de grilla %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * get_elemento(+Grilla, +Fila, +Columna, -Elem)
 * 
 * Encuentra el elemento Elem ubicando en (Fila, Columna) de la Grilla.
 * Columna es una lista conteniendo un único elemento que representa la columna.
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
    max_list(Lista, Max).

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

/*
 * burbujear_ceros(+Grilla, -GrillaBurbujeada)
 * 
 * Dada una grilla conteniendo elementos numéricos, se unifica en GrillaBurbujeada su correspondiente con los valores iguales que cero en la superficie de cada columna
 * La estrategia consiste en buscar su grilla traspuesta y desplazar los ceros en las columnas
 */
burbujear_ceros(Grilla, GrillaBurbujeada):-
	transpose(Grilla, GrillaTraspuesta),
	subir_ceros(GrillaTraspuesta, GrillaBurbujeadaAux),
	transpose(GrillaBurbujeadaAux, GrillaBurbujeada).
	
/*
 * subir_ceros(+Grilla, -Grilla)
 * 
 * Predicado auxiliar a burbujear_ceros. 
 */
subir_ceros([], []).
subir_ceros([Fila | Resto], [FilaBurbujeada | RestoBurbujeado]):-
	mover_ceros_izquierda(Fila, FilaBurbujeada),
	subir_ceros(Resto, RestoBurbujeado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * booster(+Grilla, +CantColumnas, -GrillaEfectoResultante)
 * 
 * GrillaEfectoResultante es la lista de grillas representando el efecto, en etapas, de combinar las celdas de elementos iguales adyacentes
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */
booster(Grid, NumOfColumns, RGrids):-
    grupos_iguales(Grid, NumOfColumns, GrillaMatriz, GruposCaminos),
    aplicar_efecto(GrillaMatriz, NumOfColumns, GruposCaminos, GrillasEvolucion),
    ultimo(GrillasEvolucion, GrillaCeros),
    burbujear_ceros(GrillaCeros, GrillaBurbujeada),
	generar_rango(GrillaBurbujeada, LimInferior, LimSuperior), 
	reemplazar_ceros(GrillaBurbujeada, LimInferior, LimSuperior, GrillaCompleta),
    aplanar(GrillaCeros, GrillaCerosAplanada),
    aplanar(GrillaBurbujeada, GrillaBurbujeadaAplanada),
    aplanar(GrillaCompleta, GrillaCompletaAplanada),
    RGrids = [GrillaCerosAplanada, GrillaBurbujeadaAplanada, GrillaCompletaAplanada].

/*
 * grupos_iguales(+Grid, +NumOfColumns, -GrillaMatriz, -Grupos)
 * 
 * Unifica en Grupos, a partir de Grid y NumOfColumns, los elementos que son iguales y adyacentes de la grilla.
 * Unifica en GrillaMatriz los elementos de Grid en forma matricial.
 */
grupos_iguales(Grid, NumOfColumns, GrillaMatriz, Grupos):-
    tamanio(Grid, CantElementos),
    agrupar(Grid, NumOfColumns, GrillaMatriz),
    buscar_caminos_boostear(Grid, GrillaMatriz, 0, CantElementos, NumOfColumns, Caminos),
    eliminar_listas_un_elemento(Caminos, CaminosFinales),
    buscar_grupos_booster(CaminosFinales, CaminosFinales, GruposRepetidos),
    concatenar_caminos(GruposRepetidos, Grupos).

/*
 * buscar_caminos_boostear(+Elementos, +Grilla, +Posicion, +CantElementos, +CantColumnas, -Caminos)
 * 
 * A partir de los elementos ingresados computa y unifica una lista Caminos que contiene una lista de caminos de vecinos de elementos iguales.
 * En caso de que un elemento no tenga una posición adyacente igual a sí, la lista de caminos referida a esta posición, contiene la posición que se está controlando.
 */
buscar_caminos_boostear([], _, _, _ , _, []).
buscar_caminos_boostear([Elem | RestoElementos], Grilla, Posicion, CantElementos, CantColumnas, [CaminoElem | RestoCaminos]):-
    pos_elementos_booster(Elem, Grilla, Posicion, CantElementos, CantColumnas, CaminoElem),
    PosSiguiente is Posicion + 1,
    buscar_caminos_boostear(RestoElementos, Grilla, PosSiguiente, CantElementos, CantColumnas, RestoCaminos).

/*
 * pos_elementos_boostear(+Elem, +Grilla, +Posicion, +CantElementos, +CantColumnas, -PosBoostear)
 *
 * Dado el elemento Elem con el cual se debe comparar, se computa una lista PosBoostear que contiene las posiciones adyacentes y siguientes de la grilla Grilla a Elem ubicado en la posicion Posicion, tales que sus elementos son iguales que Elem
 */
% (1) Caso para evaluar los elementos de la grilla que están en la primer columna pero no están en la última fila
pos_elementos_booster(Elem, Grilla, Posicion, CantElementos, CantColumnas, PosBoostear):-
    Resto is Posicion mod CantColumnas,
    Resto = 0,
    Posicion < CantElementos - CantColumnas, 
    Pos1 is Posicion+1, Pos2 is Posicion+CantColumnas, Pos3 is Pos1+Pos2-Posicion,
	PosicionesEvaluar = [Pos1, Pos2, Pos3],
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, PosicionesEvaluar, PosBoostearAux),
    append([Posicion], PosBoostearAux, PosBoostearConVacio),
    borrar_todas([], PosBoostearConVacio, PosBoostear). 
% (2) Caso para evaluar los elementos de la grilla que no están en columnas extremas y no están en la última fila
pos_elementos_booster(Elem, Grilla, Posicion, CantElementos, CantColumnas, PosBoostear):-
    Resto is Posicion mod CantColumnas,
    ColumnaTope is CantColumnas - 1,
    Resto \= 0, Resto \= ColumnaTope,
    Posicion < CantElementos - CantColumnas, 
    Pos1 is Posicion+1, Pos2 is Posicion+CantColumnas-1, Pos3 is Posicion+CantColumnas, Pos4 is Posicion+CantColumnas+1,
	PosicionesEvaluar = [Pos1, Pos2, Pos3, Pos4],
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, PosicionesEvaluar, PosBoostearAux),
    append([Posicion], PosBoostearAux, PosBoostearConVacio),
    borrar_todas([], PosBoostearConVacio, PosBoostear). 
% (3) Caso para evaluar los elementos de la grilla que están en la última columna pero no están en la última fila
pos_elementos_booster(Elem, Grilla, Posicion, CantElementos, CantColumnas, PosBoostear):-
    Resto is Posicion mod CantColumnas,
    ColumnaTope is CantColumnas - 1,
    Resto = ColumnaTope,
    Posicion < CantElementos - CantColumnas, 
    Pos1 is Posicion+CantColumnas-1, Pos2 is Posicion+CantColumnas,
	PosicionesEvaluar = [Pos1, Pos2],
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, PosicionesEvaluar, PosBoostearAux),
    append([Posicion], PosBoostearAux, PosBoostearConVacio),
    borrar_todas([], PosBoostearConVacio, PosBoostear).
% (4) Caso para evluar los elementos de la grilla que están en la última fila pero no en la última columna
pos_elementos_booster(Elem, Grilla, Posicion, CantElementos, CantColumnas, PosBoostear):-
    Posicion >= CantElementos - CantColumnas,
    Posicion < CantElementos, 
    PosEvaluar is Posicion + 1,
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, [PosEvaluar], PosBoostearAux),
    append([Posicion], PosBoostearAux, PosBoostearConVacio),
    borrar_todas([], PosBoostearConVacio, PosBoostear). 
% (5) Caso para evaluar el último elemento de la grilla
pos_elementos_booster(_, _, Posicion, CantElementos, _, [Posicion]):- 
    Posicion is CantElementos - 1.

/*
 * pos_elementos_booster_iteracion(+Elem, +Grilla, +CantColumnas, +PosEvaluar, -PosEvaluadas)
 * 
 * Se recorre la lista PosEvaluar y se unifica en PosEvaluadas una lista cuyos elementos son aquellas posiciones donde los elementos almacenados son iguales que Elem
 */
pos_elementos_booster_iteracion(_, _, _, [], []).
pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, [PosEvaluar | RestoPosEvaluar], [Pos | RestoEvaluado]):-
    obtener_coordenada(PosEvaluar, CantColumnas, F, C),
    get_elemento(Grilla, F, [C], ElemEvaluar),
    ElemEvaluar = Elem,
    Pos = PosEvaluar,
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, RestoPosEvaluar, RestoEvaluado).
pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, [PosEvaluar | RestoPosEvaluar], [Pos | RestoEvaluado]):-
    obtener_coordenada(PosEvaluar, CantColumnas, F, C),
    get_elemento(Grilla, F, [C], ElemEvaluar),
    ElemEvaluar \= Elem,
    Pos = [],
    pos_elementos_booster_iteracion(Elem, Grilla, CantColumnas, RestoPosEvaluar, RestoEvaluado).

/*
 * borrar_todas(+Elem, +Lista, -ListaSinElem)
 * 
 * Dados un elemento Elm y una lista Lista se unifica en ListaSinElem a Lista sin ninguna aparicion de Elem
 */
borrar_todas(_, [], []).
borrar_todas(X, [X | Xs], Z) :-
    borrar_todas(X, Xs, Z).
borrar_todas(X, [Y | Ys], [Y | Z]) :-
    Y \= X,
    borrar_todas(X, Ys, Z).

/*
 * eliminar_listas_un_elemento(+ListaOriginal, -ListaResultante)
 * 
 * Elimina de la ListaOriginal, una lista de sublistas, todas aquellas sublistas que tengan longitud uno y unifica la lista resultante en ListaResultante
 */
eliminar_listas_un_elemento([], []).
eliminar_listas_un_elemento([X|Xs], Ys) :-
    tamanio(X, T),
    T = 1,
    eliminar_listas_un_elemento(Xs, Ys).
eliminar_listas_un_elemento([X|Xs], [X|Ys]) :-
    tamanio(X, T),
    T > 1,
    eliminar_listas_un_elemento(Xs, Ys).

/*
 * obtener_coordenada(+Pos, +CantColumnas, -Fila, -Columna)
 * 
 * Dada una posición relativa referida al orden del elemento de un conjunto dado, computa los índices Fila y Columna. 
 * Predicado inverso a ubicacion_en_grilla/4 (más arriba)
 */
obtener_coordenada(Pos, CantColumnas, Fila, Columna):-
     Fila is Pos div CantColumnas,
     Columna is Pos mod CantColumnas.

/*
 * buscar_grupos_booster(+ListaCaminosAdyacentes, +ListaCaminosAdyacentes, -Grupos)
 * 
 * Dada una lista de porciones de caminos de elementos vecinos adyacentes iguales, computa viendo, elemento a elemento, en que otros caminos está presente la posicion del camino
 * Este predicado avanza sobre las listas de posiciones que conforman un posible camino.
 */
buscar_grupos_booster([], _, []).
buscar_grupos_booster([PrimerCamino | RestoCaminosAdy], Caminos, [PrimerGrupo | RestoGrupos]):-
    buscar_grupos_booster_aux(PrimerCamino, Caminos, PrimerGrupoAux),
    aplanar(PrimerGrupoAux, PrimerGrupoRepetidos),
    elimina_repetidos(PrimerGrupoRepetidos, PrimerGrupo),
    buscar_grupos_booster(RestoCaminosAdy, Caminos, RestoGrupos).

/*
 * buscar_grupos_booster_aux(+ListaPosicionesAdyacentes, +ListaCaminosAdyacentes, -Grupo)
 * 
 * Dada una lista de posiciones que representan lugares a boostear, computa viendo, posicion a posicion, en que otros caminos está presente esa posicion.
 * Este predicado avanza sobre las posiciones que conforman a un camino en particular.
 */ 
buscar_grupos_booster_aux([], _, []).
buscar_grupos_booster_aux([PrimerElem | RestoElems], Caminitos, [PrimerPorcion | RestoGrupo]):-
    findall(X, (member(X, Caminitos), member(PrimerElem, X)), PrimerPorcionAux),
    aplanar(PrimerPorcionAux, PrimerPorcion),   
    buscar_grupos_booster_aux(RestoElems, Caminitos, RestoGrupo).

/*
 * concatenar_caminos(+ListaCaminos, -ListaCaminosUnificada)
 * 
 * Unifica en ListaCaminosUnificada una lista que contiene caminos que surgen a partir de la simplificación de la union de caminos con posiciones comunes entre ellos.
 */
concatenar_caminos([], []).
concatenar_caminos([Pos1 | Posiciones1], [Pos2 | Posiciones2]):-
    concatenar_comparten(Posiciones1, Pos1, Pos2, Aux),
    concatenar_caminos(Aux, Posiciones2).

/*
 * concatenar_comparten(+ListasConcatenar, +ListaComparacion, -ListaResultante, -ListaAcumulador)
 * 
 * Concatena sublistas pertenecientes a ListasConcatenar en una lista que comparten al menos un elemento (se compara con los elementos de ListaComparacion).
 */
concatenar_comparten([], Lista, Lista, []).
concatenar_comparten([Sublista | RestoSublistas], Lista, ListaResultante, ListaAux):-
    comparten_elementos(Sublista, Lista),
    append([Sublista, Lista], ListaAppend),
    elimina_repetidos(ListaAppend, ListaSinRepetidos),
    concatenar_comparten(RestoSublistas, ListaSinRepetidos, ListaResultante, ListaAux).
concatenar_comparten([Sublista | RestoSublistas], Lista, ListaResultante, [Sublista | SublistasAux]):-
    concatenar_comparten(RestoSublistas, Lista, ListaResultante, SublistasAux).

/*
 * aplicar_efecto(+Grilla, +CantColumnas, +Caminos, -EvolucionGrilla)
 * 
 * A partir de la grilla Grilla y los caminos Caminos que deben explotarse, se unifica en EvoucionGrilla las diferentes instancias por las que pasa grilla al usar el booster
 */
aplicar_efecto(_, _, [], []).
aplicar_efecto(Grilla, CantColumnas, [Camino | Caminos], [PrimerGrilla | GrillasSiguientes]):-
    listaPos_a_listaCoordenadas(Camino, CantColumnas, CaminoCoord),
    suma_camino_pot_dos(Grilla, CaminoCoord, Suma),
    pos_setear_booster(CaminoCoord, Pos), % Corrección de devolución del proyecto 1
    set_suma_grilla(Grilla, Pos, Suma, GrillaSuma),
    Pos = [ContenidoPos], % Corrección de devolución del proyecto 1
    borrar_todas(ContenidoPos, CaminoCoord, CaminoSinPos), % Corrección de devolución del proyecto 1
    set_ceros_grilla(GrillaSuma, CaminoSinPos, PrimerGrilla),
    aplicar_efecto(PrimerGrilla, CantColumnas, Caminos, GrillasSiguientes).

/*
 * listaPos_a_listaCoordenadas(+Posiciones, +CantColumnas, -Coordenadas)
 * 
 * Sabiendo que las posiciones tienen un mapeo a pares ordenados (i,j) que codifican una coordenada, se unifica en Coordenadas una lista con dicho mapeo, para cada posicion de Posiciones
 */
listaPos_a_listaCoordenadas([], _, []).
listaPos_a_listaCoordenadas([PrimerPosicion | RestoPosiciones], CantColumnas, [PrimerCoordenada | RestoCoordenadas]):-
	obtener_coordenada(PrimerPosicion, CantColumnas, F, C),
    get_coordenadas(PrimerCoordenada, F, C),
    listaPos_a_listaCoordenadas(RestoPosiciones, CantColumnas, RestoCoordenadas).

/*
 * CORRECCIÓN DEVOLUCIÓN
 * 
 * pos_setear_booster(+Camino, -Pos)
 * 
 * Dado un camino de posiciones, unifica como unico elemento de la lista Pos a la coordenada que contiene a la posición ubicada más abajo y a la derecha de la grilla.
 */
pos_setear_booster(Camino, [Pos]):-
    findall(F,
           (member(Coordenada, Camino), Coordenada = [F, _C]),
            Filas),
    max_list(Filas, FilaMayor),
    findall(C,
           (member(Coordenada, Camino), Coordenada = [FilaMayor, C]),
            ColumnasFilaMayor),
    max_list(ColumnasFilaMayor, ColumnaMayor),
    Pos = [FilaMayor, ColumnaMayor].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * movida_maxima(+Grilla, +CantColumnas, -SumaCaminoMaximo, -CaminoMaximo)
 * 
 * Calcula el camino (CaminoMaximo) que consigue el mayor número (SumaCaminoMaximo) a partir de la configuración actual
 */
movida_maxima(Grilla, CantColumnas, SumaCaminoMaximo, CaminoMaximo):-
    agrupar(Grilla, CantColumnas, GrillaAgrupada),
    buscar_caminos_grilla(Grilla, Grilla, GrillaAgrupada, CantColumnas, 0, Caminos),
    aplanar(Caminos, CaminosAplanados),
    tamanio(CaminosAplanados, Tamanio),
    Tamanio > 0,
    !,
    max(Grilla, GrillaAgrupada, CantColumnas, CaminosAplanados, 0, Maximos),
    ultimo(Maximos, CaminoMaximo),
    suma_camino_pot_dos(GrillaAgrupada, CaminoMaximo, SumaCaminoMaximo).
movida_maxima(_Grilla, _CantColumnas, 0, []).

/*
 * buscar_caminos_grilla(+Elementos, +Grilla, +GrillaAgrupada, +CantColumnas, +Indice, -Caminos)
 * 
 * Busca todos los caminos que pueden formarse con la configuración actual de la grilla.
 * - Elementos: lista que contiene los elementos de la grilla.
 * - Grilla: análogo a Elementos; se utiliza para ir avanzando sobre la lista.
 * - GrillaAgrupada: Elementos en representación matricial; se utilizan las tres debido a la implementación base del juego.
 *
 * Indice: Posicion actual de control; permite junto a CantColumnas, conocer la coordenada del elemento.
 *
 * Delega y hace uso de buscar_caminos_celda
 */    
buscar_caminos_grilla(_, [], _GrillaAgrupada, _, _, []).
buscar_caminos_grilla(Grilla, [PrimerCelda | RestoCeldas], GrillaAgrupada, CantColumnas, Indice, [CaminoPrimerCelda | RestoCaminos]):-
    adyacentes_validas(Grilla, GrillaAgrupada, Coordenada, CantColumnas, Adyacentes),
    obtener_coordenada(Indice, CantColumnas, F, C),
    get_coordenadas(Coordenada, F, C),
    buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PrimerCelda, Adyacentes, [Indice], [[F, C]], CaminoPrimerCelda),
    length(CaminoPrimerCelda, Tamanio),
    Tamanio > 1,
    IndiceSiguiente is Indice + 1,
    buscar_caminos_grilla(Grilla, RestoCeldas, GrillaAgrupada, CantColumnas, IndiceSiguiente, RestoCaminos),
    !.
buscar_caminos_grilla(Grilla, [_ | RestoCeldas], GrillaAgrupada, CantColumnas, Indice, RestoCaminos):-
    IndiceSiguiente is Indice + 1,
    buscar_caminos_grilla(Grilla, RestoCeldas, GrillaAgrupada, CantColumnas, IndiceSiguiente, RestoCaminos).
    
/*
 * buscar_caminos_celda(+Elementos, +GrillaAgrupada, +CantColumnas, +PotDos, +Adyacentes, +Visitados, +Camino, -CaminosResultantes)
 * 
 * Busca los caminos que pasan por la posicion que alberga PotDos y suceden a Visitados.
 * Delega la creacion del camino en crear_caminos/9
 */    
buscar_caminos_celda(_, _, _, _, [], _, Camino, [Camino]).
buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDos, [Ady | RestoAdyacentes], Visitados, Camino, CaminosResultantes):-
    length(Camino, 1),
    not(member(Ady, Visitados)),
    obtener_coordenada(Ady, CantColumnas, F, C),
    get_elemento(GrillaAgrupada, F, [C], PotDosAdyacente),
    PotDosAdyacente =:= PotDos,
    crear_caminos(Grilla, GrillaAgrupada, CantColumnas, PotDos, PotDosAdyacente, [Ady | RestoAdyacentes], Visitados, Camino, CaminosResultantes).
buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDos, [Ady | RestoAdyacentes], Visitados, Camino, CaminosResultantes):-
    length(Camino, TamanioCamino),
    TamanioCamino > 1,
    not(member(Ady, Visitados)), 
    obtener_coordenada(Ady, CantColumnas, F, C),
    get_elemento(GrillaAgrupada, F, [C], PotDosAdyacente),
    (PotDosAdyacente =:= PotDos ; PotDosAdyacente =:= PotDos*2),
    crear_caminos(Grilla, GrillaAgrupada, CantColumnas, PotDos, PotDosAdyacente, [Ady | RestoAdyacentes], Visitados, Camino, CaminosResultantes).
buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDos, [_ | RestoAdyacentes], Visitados, Camino, CaminosResultantes):-
    buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDos, RestoAdyacentes, Visitados, Camino, CaminosResultantes).

/*
 * crear_caminos(+Grilla, +GrillaAgrupada, +CantColumnas, +PotDos, +PotDosAdyacente, +Adyacentes, +Visitados, +CaminoActual, -CaminosResultantes)
 * 
 * Crea de forma efectiva los caminos que pasan por una celda particular y son adyacentes a un elemento puntual.
 */    
crear_caminos(Grilla, GrillaAgrupada, CantColumnas, PotDos, PotDosAdyacente, [Ady | RestoAdyacentes], Visitados, CaminoActual, CaminosResultantes):-
    append(Visitados, [Ady], VisitadosActualizado),
    obtener_coordenada(Ady, CantColumnas, F, C),
    get_coordenadas(Coordenada, F, C),
    append(CaminoActual, [Coordenada], CaminoActualActualizado),
    adyacentes_validas(Grilla, GrillaAgrupada, Coordenada, CantColumnas, Adyacentes),
    buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDosAdyacente, Adyacentes, VisitadosActualizado, CaminoActualActualizado, CaminosAux1),
    buscar_caminos_celda(Grilla, GrillaAgrupada, CantColumnas, PotDos, RestoAdyacentes, Visitados, CaminoActual, CaminosAux2),
    append([CaminoActualActualizado], CaminosAux1, CaminosAux),
    append(CaminosAux, CaminosAux2, CaminosResultantes),
    !.
    
/*
 * adyacentes_validas(+Grilla, +Coord, +CantColumnas, -AdyacentesValidas)
 * 
 * Dada una grilla numerica representada como una lista de elementos de la grilla Grilla, una coordenada Coord y CantColumnas de la grilla, 
 * computa y unifica en AdyacentesValidas aquellas coordenadas adyacentes a Coord que cumplen con la restricción de que alberguen una potencia de dos 
 * igual a la alojada en Coord o inmediatamente superior.
 */
adyacentes_validas(GrillaPlana, GrillaMatriz, [F, C], CantColumnas, AdyacentesValidas):-
    length(GrillaPlana, Tamanio),
    get_elemento(GrillaMatriz, F, [C], ElementoActual),
    indice_raiz(ElementoActual, IndiceActual),
     
    coordenadas_adyacentes([F, C], CantColumnas, Tamanio, Adyacentes),
    
    IndiceSiguiente is IndiceActual + 1,
        
    findall(Pos,
            (member(Coord, Adyacentes),
             get_coordenadas(Coord, Faux, Caux),
             get_elemento(GrillaMatriz, Faux, [Caux], ElementoAux),
             indice_raiz(ElementoAux, IndiceAux),
             (IndiceAux = IndiceActual;
              IndiceAux = IndiceSiguiente),
              ubicacion_en_grilla(Faux, Caux, CantColumnas, Pos)),
            AdyacentesValidas).
    
/*
 * coordenadas_adyacentes(+Coordenada, +CantColumnas, +Tamanio, -Adyacentes)
 * 
 * Dada una coordenadas Coordenada y restricciones dimensionales (CantColumnas y Tamanio), computa y unifica en Adyacentes todas las coordenadas adyacentes a Coordenada
 */
coordenadas_adyacentes([F, C], CantColumnas, Tamanio, Adyacentes):-
    FArriba is F - 1,
    FAbajo is F + 1,
    CIzquierda is C - 1,
    CDerecha is C + 1,
    C1 = [FArriba, CIzquierda],
    C2 = [FArriba, C],
    C3 = [FArriba, CDerecha],
    C4 = [F, CIzquierda],
    C5 = [F, CDerecha],
    C6 = [FAbajo, CIzquierda],
    C7 = [FAbajo, C],
    C8 = [FAbajo, CDerecha],
    check_positions([C1, C2, C3, C4, C5, C6, C7, C8], CantColumnas, Tamanio, Adyacentes).
    
/*
 * check_positions(+Coordendas, +CantColumnas, +Tamanio, -CoordenadasChequeadas)
 *
 * Computa y unifica en CoordenadasChequeadas aquellas coordenadas pertenecientes a Coordenadas que no se caen de la matriz, 
 * según restricciones dimensionales determinadas por CantColumnas y Tamanio
 */
check_positions([[F, C]], CantColumnas, Tamanio, [[F, C]]):-
    (F >= 0, F < Tamanio/CantColumnas,
        C >= 0, C < CantColumnas),
    !.
check_positions([[_F, _C]], _, _, []).
check_positions([[F, C] | R], CantColumnas, Tamanio, [[F, C] | R_ok]):-
    CantFilas is Tamanio/CantColumnas,
    (F >= 0, F < CantFilas,
        C >= 0, C < CantColumnas,
    check_positions(R, CantColumnas, Tamanio, R_ok)),
    !.
check_positions([[_F, _C] | R], CantColumnas, Tamanio, R_ok):-
    check_positions(R, CantColumnas, Tamanio, R_ok).

/*
 * max(+Grilla, +GrillaAgrupada, +CantColumnas, +Maximos, +MaximoActual, -Maximos)
 * 
 * Dada una grilla con una cierta configuración y una lista Maximos que contiene caminos, compara el valor que computa cada uno de ellos con MaximoActual
 * Unifica en Maximos los caminos que producen una suma mayor que MaximoActual
 */  
max(_, _, _, [], _, []).
max(Grilla, GrillaAgrupada, CantColumnas, [PrimerMaximo | Maximos], MaximoActual, [PrimerMaximo | NuevoCamino]):-
    suma_camino_pot_dos(GrillaAgrupada, PrimerMaximo, SumaCaminoMaximo),
    SumaCaminoMaximo > MaximoActual,
    max(Grilla, GrillaAgrupada, CantColumnas, Maximos, SumaCaminoMaximo, NuevoCamino),
    !.
max(Grilla, GrillaAgrupada, CantColumnas, [_ | Maximos], MaximoActual, Maximo):-
    max(Grilla, GrillaAgrupada, CantColumnas, Maximos, MaximoActual, Maximo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * maximo_adyacente(+Grilla, +CantColumnas, -SumaMaximoAdyacente, -CaminoAdyacente)
 * 
 *  Calcule y y unifica en CaminoAdyacente el camino que consiga generar el número más grande posible adyacente a otro igual (preexistente)
 *  SumaMaximoAdyacente unifica con la menor potencia de dos mayor o igual a la suma del camino CaminoAdyacente
 */
maximo_adyacente(Grilla, CantColumnas, SumaMaximoAdyacente, CaminoAdyacente):-
    agrupar(Grilla, CantColumnas, GrillaAgrupada),
    buscar_caminos_grilla(Grilla, Grilla, GrillaAgrupada, CantColumnas, 0, Caminos),
    aplanar(Caminos, CaminosAplanados),
    length(CaminosAplanados, Tamanio),
    Tamanio > 0,
    !,
    max_list(Grilla, MaxPotencia),
    
    %Elimino los caminos de un solo elemento
    findall(C,
            (member(C, CaminosAplanados),
             length(C, T),
             T > 1),
            CaminosAplanadosAux),
    
    %Elimino los caminos que computan sumas superiores al mayor elemento de la grilla
    findall(C,
            (member(C, CaminosAplanadosAux),
             suma_camino_pot_dos(GrillaAgrupada, C, SumaC),
             (SumaC < MaxPotencia; SumaC = MaxPotencia)),
            CaminosSumaMenoresMaximoRepetidos),
    
    elimina_repetidos(CaminosSumaMenoresMaximoRepetidos, CaminosSumaMenoresMaximo),
    
    encontrar_camino_retornar(GrillaAgrupada, CantColumnas, CaminosSumaMenoresMaximo, CaminoAdyacente),
    length(CaminoAdyacente, TamanioCaminoRetorno),
    TamanioCaminoRetorno > 0,
    !,
    suma_camino_pot_dos(GrillaAgrupada, CaminoAdyacente, SumaMaximoAdyacente).

/*
 * encontrar_caminos_retornar(+GrillaAgrupada, +CaminosConSumaAdyacentes, -CaminoEfectivo)
 * 
 * A partir de la grilla de elementos en forma matricial GrillaAgrupada y una lista CaminosConSumaAdyacentes (una lista que contiene caminos cuya suma
 *   es igual al valor de alguno de los elementos adyacentes al ultimo bloque), se encuentra el que efectivamente va a mostrarse (el que compute mayor valor)
 */
encontrar_camino_retornar(GrillaAgrupada, CantColumnas, CaminosConSumaAdyacentes, CaminoEfectivo):-
    aplanar(GrillaAgrupada, GrillaLlana),
    max_list(GrillaLlana, Max),
    findall(Pot,
            (member(Pot, GrillaLlana),
             (Pot < Max; Pot = Max), Pot > 0),
            PotenciasValidas),
    %Ordena las potencias de dos que están en la grilla de mayor a menor
    sort(0, @>, PotenciasValidas, PotenciasValidasOrdenadas),  
    camino_retornar(PotenciasValidasOrdenadas, CaminosConSumaAdyacentes, GrillaAgrupada, CantColumnas, CaminoEfectivo).

/*
 * camino_retornar(+PotenciasDos, +CaminosCandidatos, +GrillaAgrupada, -CaminoRetornar)
 * 
 * A partir de las potencias de dos en juego en el tablero PotenciasDos y los posibles CaminosCandidatos a ser retornados coo CaminoMaximoAdyacente,
 *   con GrillaAgrupada la grilla de elementos en forma matricial, computa y unifica en CaminoRetornar el CaminoMaximoAdyacente efectivo.
 *
 * El cómputo lo realiza teniendo en cuenta las potencias de dos en juego; si no hay camino que satisfaga la mayor, prueba con la menor potencia de dos siguiente.
 */
camino_retornar([], _, _, _, []).
camino_retornar([PotMayor | Potencias], CaminosCandidatos, GrillaAgrupada, CantColumnas, CaminoRetornar):-
    findall(C,
            (member(C, CaminosCandidatos),
             suma_camino_pot_dos(GrillaAgrupada, C, SumaC),
             SumaC =:= PotMayor,
             validar_resolucion_camino(GrillaAgrupada, CantColumnas, C)),
            CandidatosPotencia),
    length(CandidatosPotencia, T),
    ((T > 0, last(CandidatosPotencia, CaminoRetornar));
    camino_retornar(Potencias, CaminosCandidatos, GrillaAgrupada, CantColumnas, CaminoRetornar)).
    
/*
 * validar_resolucion_camino(+GrillaAgrupada, +CantColumnas, +Camino)
 * 
 * Determina si el camino Camino produce un resultado valido para la funcionalidad maximo_adyacente
 */
validar_resolucion_camino(GrillaAgrupada, CantColumnas, Camino):-
    aplanar(GrillaAgrupada, GrillaPlana),
    length(GrillaPlana, T),
    suma_camino_pot_dos(GrillaAgrupada, Camino, Suma),
    virtualizar_jugada(GrillaAgrupada, Camino, GrillaJugadaVirtual),
    burbujear_ceros(GrillaJugadaVirtual, GrillaBurbujeada),
    coordenada_final_resolucion(Camino, CoordenadaResultante),
	tiene_adyacente_valido(GrillaBurbujeada, CantColumnas, T, [CoordenadaResultante], Suma).
    
/*
 * coordenada_final_resolucion(+Camino, -CoordenadaFinal)
 * 
 * Computa y unifica en CoordenadaFinal la coordenada donde termina posicionada la celda que contiene la suma de Camino
 */
coordenada_final_resolucion(Camino, CoordenadaFinal):-
    last(Camino, Ultima),
    get_coordenadas(Ultima, F, C),
    findall(Coordenada,
            (member(Coordenada, Camino),
             get_coordenadas(Coordenada, FC, CC),
             C =:= CC, F < FC),
            CoordMismaColumna),
    length(CoordMismaColumna, CantGravedad),
    NuevaFila is F + CantGravedad,
    CoordenadaFinal = [NuevaFila, C].

/*
 * tiene_adyacente_valido(+GrillaAgrupada, +CantColumnas, +Tamanio, +Camino)
 * 
 * Dado un Camino y las dimensiones CantColumnas y Tamanio de GrillaAgrupada, la grilla de elementos de forma matricial,
 *   se determina si, adyacente al último elemento de Camino, hay un elemento que sea equivalente a la potencia de dos de la suma del camino.
 */
tiene_adyacente_valido(GrillaAgrupada, CantColumnas, Tamanio, Camino, SumaC):-
    last(Camino, [F, C]),
    coordenadas_adyacentes([F, C], CantColumnas, Tamanio, Ady),
    findall(Coord,
            (member(Coord, Ady),
             get_coordenadas(Coord, FC, CC),
             get_elemento(GrillaAgrupada, FC, [CC], E),
             E =:= SumaC),
            AdyValidas),
    length(AdyValidas, AdyTamanio),
    AdyTamanio > 0,
    !.

 /*
  * virtualizar_jugada(+GrillaAgrupada, +Camino, -GrillaVirtual)
  *
  * Computa y unifica en GrillaVirtual cómo quedaría GrillaAgrupada luego de jugar el camino Camino
  */   
virtualizar_jugada(GrillaAgrupada, Camino, GrillaVirtual):-
    suma_camino_pot_dos(GrillaAgrupada, Camino, Suma),
    last(Camino, Coord),
    set_suma_grilla(GrillaAgrupada, [Coord], Suma, GrillaSuma),
    borrar_ultimo(Camino, CaminoSinPos),
    set_ceros_grilla(GrillaSuma, CaminoSinPos, GrillaVirtual).