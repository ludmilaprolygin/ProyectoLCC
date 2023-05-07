:- module(proylcc, 
	[  
		join/4,               % Evolución de la combinación de celdas del camino
		suma_camino_pot_dos/3 % Computa el resultado de la suma de un camino
		%booster
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
	agrupar(Grid, NumOfColumns, GrillaAgrupada), %Cambiar nombre a agrupar/2
	suma_camino_pot_dos(GrillaAgrupada, Path, Suma),
	set_suma_grilla(GrillaAgrupada, Path, Suma, GrillaSuma),
	borrar_ultimo(Path, PathSinUltimo), %Se busca el Path sin el ultimo elemento porque sino setea un cero en el lugar que debe contener a la suma de elementos del camino
	set_ceros_grilla(GrillaSuma, PathSinUltimo, GrillaCeros),
	burbujear_ceros(GrillaCeros, GrillaBurbujeada),
	generar_rango(GrillaSuma, LimInferior, LimSuperior), %Pensar la posibilidad de meterlo adentro de reemplazar_ceros directamente
	reemplazar_ceros(GrillaBurbujeada, LimInferior, LimSuperior, GrillaCompleta),
	aplanar(GrillaSuma, GrillaSumaAplanada),
	aplanar(GrillaCeros, GrillaCerosAplanada),
	aplanar(GrillaBurbujeada, GrillaBurbujeadaAplanada),
	aplanar(GrillaCompleta, GrillaCompletaAplanada),
	RGrids = [GrillaSumaAplanada, GrillaCerosAplanada, GrillaBurbujeadaAplanada, GrillaCompletaAplanada]. %Grid no va

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
	mover_ceros_derecha(Lista, [], ListaAux), % Inicialmente el desplazamiento se da a la derecha por simplicidad
	pos_ceros(ListaAux, 0, PosCeros), % Se busca una lista que contenga todas los posiciones donde hay ceros en la lista Lista
	tamanio(PosCeros, CantCeros),     % para poder consultar la cantidad total de ceros.
	tamanio(ListaAux, CantTotal), 
	CantValores is CantTotal-CantCeros,
	dividir_lista(ListaAux, CantValores, Valores, Ceros), % Se parte la lista Lista en dos listas; la primera es de longitud CantValores (y consecuentemente contiene solo valores diferentes de cero), y la segunda el resto de los elementos
	append(Ceros, Valores, ListaBurbujeada). % Se pegan las listas Ceros y Valores para que queden los ceros al principio de la lista, tal cual se busca
	
/*
 * mover_ceros_derecha(+Lista, +Auxiliar, -ListaCerosDerecha)
 * 
 * Dada una lista Lista a la cual se desea agrupar los ceros en el extremo derecho y una lista Auxiliar que sirve de acumulador de elementos, se busca formar una lista consecuente ListaCerosDerecha con los ceros a su derecha, como su nombre lo indica
 */
mover_ceros_derecha([], Aux, Aux).
mover_ceros_derecha([0 | Resto], Aux, ListaCerosDerecha):- 
	mover_ceros_derecha(Resto, [0 | Aux], ListaCerosDerecha).
mover_ceros_derecha([X | Resto], Aux, [X | ListaCerosDerecha]):-
	X\=0,
	mover_ceros_derecha(Resto, Aux, ListaCerosDerecha).

/*
 * dividir_lista(+ListaDividir, +TamanioPrimera, -PrimeraSubLista, -SegundaSubLista)
 * 
 * Dada una lista ListaDividir se unifica en PrimeraSubLista una lista conteniendo los primeros TamanioPrimera elementos de ListaDividir y, el resto de los elementos, en SegundaSubLista
 */
dividir_lista([], _, [], []).
dividir_lista(Lista, Tamanio, [], Lista) :-
    Tamanio =< 0.
dividir_lista([X|Resto], Tamanio, [X|RestoPrimera], SegundaLista) :-
    Tamanio > 0,
    TamanioAux is Tamanio - 1,
    dividir_lista(Resto, TamanioAux, RestoPrimera, SegundaLista).


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
pos_ceros(Lista, Indice, Posiciones):-
	pos_elem(Lista, 0, Indice, Posiciones).

/*
 * pos_elem(+Lista, +Elem, +Indice, -Posiciones)
 * 
 * Computa en una lista las posiciones donde hay elementos Elem. 
 * El mapeo de los indices donde los hay corresponden a un orden de posiciones de coordenadas de matriz (ver camino_posiciones más arriba).
 * Indice determina el inicio del mapeo de posiciones.
 * Lista es una lista conteniendo los elementos de la grilla en forma aplanada.
 */
pos_elem([], _, _, []).
pos_elem([Elem | Lista], Elem, Indice, [Indice | PosCeros]):-
	IndiceAux is Indice + 1,
	pos_elem(Lista, Elem, IndiceAux, PosCeros).
pos_elem([X | Lista], Elem, Indice, PosCeros):-
    X \= Elem,
	IndiceAux is Indice + 1,
	pos_elem(Lista, Elem, IndiceAux, PosCeros).

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

/*
 * intercambiar(+Grilla, +F1, +C1, +F2, +C2, -GrillaResultante)
 * 
 * Dados los valores de dos coordenadas de una grilla, intercambia sus valores.
 */
intercambiar(Grilla, Fila1, Col1, Fila2, Col2, GrillaResultante) :-
	nth0(Fila1, Grilla, Lista1), % Obtener la fila 1
	nth0(Fila2, Grilla, Lista2), % Obtener la fila 2
	nth0(Col1, Lista1, Valor1), % Obtener el valor en la columna 1 de la fila 1
	nth0(Col2, Lista2, Valor2), % Obtener el valor en la columna 2 de la fila 2
	cambiar_en_pos(Lista1, Col1, Valor2, NuevaLista1), % Reemplazar el valor 1 por el valor 2 en la fila 1
	cambiar_en_pos(Lista2, Col2, Valor1, NuevaLista2), % Reemplazar el valor 2 por el valor 1 en la fila 2
	cambiar_en_pos(Grilla, Fila1, NuevaLista1, Temp), % Reemplazar la fila 1 con la nueva fila 1 en la matriz
	cambiar_en_pos(Temp, Fila2, NuevaLista2, GrillaResultante). % Reemplazar la fila 2 con la nueva fila 2 en la matriz resultante
	
/*
 * intercambiar_arriba(+Grilla, +Fila, +Columna, -GrillaResultante)
 * 
 * Dados los valores de una coordenada de una grilla Grilla, intercarmbia su valor con el elemento inmediatamente superior a él
 */
intercambiar_arriba(Grilla, 0, _, Grilla).
intercambiar_arriba(Grilla, Fila, Col, GrillaNueva):-
    Fila > 0,
    FilaArriba is Fila - 1,
	intercambiar(Grilla, Fila, Col, FilaArriba, Col, GrillaNueva).

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

booster(Grid, NumOfColumns, RGrids).

/*
booster(Grilla, CantColumnas, GrillaEfectoResultante):-
    tamanio(Grilla, CantElementos),
    agrupar(Grilla, CantColumnas, GrillaMatriz),
    buscar_caminos_boostear(Grilla, GrillaMatriz, 0, CantElementos, CantColumnas, Caminos),
    eliminar_listas_un_elemento(Caminos, CaminosFinales)
    %%%%% Agregar co
    */

/*
 * buscar_caminos_boostear(+Elementos, +Grilla, +Posicion, +CantElementos, +CantColumnas, -Caminos)
 * 
 * A partir de los elementos ingresados computa y unifica una lista Caminos que contiene una lista de caminos de vecinos de elementos iguales.
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