
/*comentar este codigo para probar del html directo*/
// const pl =require("tau-prolog");
// const promise = require("tau-prolog/modules/promises.js");

// const loader = require("tau-prolog/modules/lists.js");
// loader(pl);
// promise(pl);

// loader(promise);

/*comentar hasta aca */

var session = pl.create();

const program = `
  % cargar módulo lists
  :- use_module(library(lists)).
  :- use_module(library(promises)).
  :- use_module(library(dif)).
  
eliminar_elemento(_, [], []).
  eliminar_elemento(Elem, [Elem|T], Resultado) :-
      eliminar_elemento(Elem, T, Resultado).
  eliminar_elemento(Elem, [H|T], [H|Resultado]) :-
      Elem \\= H,
      eliminar_elemento(Elem, T, Resultado).

mano([Carta1|Color1], [Carta2|Color2], [Carta3|Color3], [Carta4|Color4],
     [Carta5|Color5], [Carta6|Color6], [Carta7|Color7], Mano) :-
    is_escalera([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7]),
    cinco_mismo_palo([Color1,Color2,Color3,Color4,Color5,Color6,Color7]),
    a_k_q_presentes([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7]),
    Mano = "1", !.

mano([Carta1|Color1], [Carta2|Color2], [Carta3|Color3], [Carta4|Color4],
     [Carta5|Color5], [Carta6|Color6], [Carta7|Color7], Mano) :-
    is_escalera([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7]),
    cinco_mismo_palo([Color1,Color2,Color3,Color4,Color5,Color6,Color7]),
    Mano = "2", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    contar_apariciones(_,[Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7],4),
    Mano = "3", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    tres_iguales([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7], NumeroRepetido),
    eliminar_elemento(NumeroRepetido,[Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7],Resultado),
    dos_iguales(Resultado, _),
    Mano = "4", !.

mano([_|Color1], [_|Color2], [_|Color3], [_|Color4],
     [_|Color5], [_|Color6], [_|Color7], Mano) :-
    cinco_mismo_palo([Color1,Color2,Color3,Color4,Color5,Color6,Color7]),
    Mano = "5", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    is_escalera([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7]),
    Mano = "6", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    tres_iguales([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7], _),
    Mano = "7", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    dos_iguales([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7], NumeroRepetido),
    eliminar_elemento(NumeroRepetido,[Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7],Resultado),
    dos_iguales(Resultado,_),
    Mano = "8", !.

mano([Carta1|_], [Carta2|_], [Carta3|_], [Carta4|_],
     [Carta5|_], [Carta6|_], [Carta7|_], Mano) :-
    dos_iguales([Carta1,Carta2,Carta3,Carta4,Carta5,Carta6,Carta7], _),
    Mano = "9", !.

mano([_|_], [_|_],[_|_], [_|_],
     [_|_], [_|_] , [_|_], Mano) :-
    Mano = "10", !.

mano([_], [_],[_], [_],
     [_], [_] , [_], Mano) :-
    Mano = "10", !.

mano(_, _, _, _,
     _, _, _, Mano) :-
    Mano = "10", !.

is_escalera(Numeros) :-
    sort(Numeros, NumerosOrdenados), %saca los repetidos y ordena de menor a mayor.
    length(NumerosOrdenados, Longitud),
    Longitud >= 5,
    consecutivos(NumerosOrdenados).

consecutivos([2, 3, 4 , 5, _, _, 14]) :- !.
consecutivos([2, 3, 4 , 5, _, 14]) :- !.
consecutivos([2, 3, 4 , 5, 14]) :- !.

consecutivos([A, B, C, D , E | _]) :- B is A + 1, C is A + 2, D is A + 3, E is A + 4, !.
consecutivos([_ , A, B, C, D , E | _]) :- B is A + 1, C is A + 2, D is A + 3, E is A + 4, !.
consecutivos([_ , _, A, B, C, D , E]) :- B is A + 1, C is A + 2, D is A + 3, E is A + 4, !.


cinco_mismo_palo(Palos) :-
    member(Color, Palos),
    contar_apariciones(Color, Palos, Conteo),
    Conteo >= 5, !.

contar_apariciones(_, [], 0).

contar_apariciones(Elemento, [Elemento|Resto], Conteo) :-
    contar_apariciones(Elemento, Resto, ConteoResto),
    Conteo is ConteoResto + 1.

contar_apariciones(Elemento, [_|Resto], Conteo) :- contar_apariciones(Elemento, Resto, Conteo).

a_k_q_presentes(Numeros) :- member(14, Numeros), member(13, Numeros), member(12, Numeros), !.

tres_iguales(Numeros,NumeroRepetido) :- contar_apariciones(NumeroRepetido,Numeros,X), X >= 3.

dos_iguales(Numeros, NumeroRepetido) :- contar_apariciones(NumeroRepetido,Numeros,X), X >= 2.

%se van a guardar los resultados de cada jugador en un array y se busca el que tenga mayor fuerza de mano (escalera_real = 1, escalera_color = 2, etc...)



% FUNCIONES COMUNES

extraer_numeros_cartas([], []).
extraer_numeros_cartas([[Valor, _Palo] | T], [Valor | ValoresRestantes]) :-
    extraer_numeros_cartas(T, ValoresRestantes).

extraer_palos_cartas([], []).
extraer_palos_cartas([[_Valor, Palo] | T], [Palo | PalosRestantes]) :-
    extraer_palos_cartas(T, PalosRestantes).

% Función que elimina duplicados de una lista
eliminar_duplicados([], []).
eliminar_duplicados([H | T], [H | Rest]) :-
    select(H, T, Resto),
    eliminar_duplicados(Resto, Rest).
eliminar_duplicados([H | T], Rest) :-
    \\+ select(H, T, _),
    eliminar_duplicados(T, Rest).

% Agrupar cartas
agrupar_cartas([], []).  % Caso base: si la lista está vacía, no hay grupos.
agrupar_cartas(Valores, Grupos) :-
    eliminar_duplicados(Valores, ValoresSinDuplicados),
    encontrar_cantidad(ValoresSinDuplicados, Valores, Grupos).


% Función que encuentra la cantidad de cada valor
encontrar_cantidad([], _, []).  % Caso base
encontrar_cantidad([Valor | Resto], Valores, [[Valor, Cantidad] | Grupos]) :-
    findall(_, member(Valor, Valores), Cartas),
    length(Cartas, Cantidad),
    encontrar_cantidad(Resto, Valores, Grupos).


comparar_kickers([], [], '0').
comparar_kickers([K1 | R1], [K2 | R2], Resultado) :-    
    (K1 > K2 -> Resultado = '1';     
     K2 > K1 -> Resultado = '2';     
     comparar_kickers(R1, R2, Resultado)).

seleccionar_trio([], 0).  % Caso base
seleccionar_trio([[Valor, 3] | _], Valor) :- !.  % Encontramos el trío
seleccionar_trio([_ | T], Trio) :-
    seleccionar_trio(T, Trio).  % Buscamos el trío en el resto de la lista

maximo([X], X).  % Si la lista tiene un solo elemento, ese es el máximo
maximo([X, Y | T], Max) :-
    X > Y,  % Si X es mayor que Y, el máximo está en la cabeza
    maximo([X | T], Max).  % Continuamos buscando en el resto de la lista
maximo([X, Y | T], Max) :-
    X =< Y,  % Si Y es mayor o igual a X, el máximo está en la cola
    maximo([Y | T], Max).  % Continuamos buscando en el resto de la lista


comparar_valor_escalera(Escalera1, Escalera2, Resultado) :-
    maximo(Escalera1, Max1),
    maximo(Escalera2, Max2),
    (Max1 > Max2 -> Resultado = '1';
     Max1 < Max2 -> Resultado = '2';
     Resultado = '0').

eliminar_duplicados_escalera([], []).
eliminar_duplicados_escalera([H | T], [H | Rest]) :-
    \\+ member(H, T), % Verifica si H no está en el resto de la lista
    eliminar_duplicados_escalera(T, Rest).
eliminar_duplicados_escalera([H | T], Rest) :-
    member(H, T), % Si H está en el resto, lo omite
    eliminar_duplicados_escalera(T, Rest).


% Comprobar si una lista tiene cinco valores consecutivos
es_escalera([V1, V2, V3, V4, V5]) :-
    V1 is V2 + 1, V2 is V3 + 1, V3 is V4 + 1, V4 is V5 + 1.

% Ordenar una lista de forma descendente
ordenar_descendente([], []).
ordenar_descendente([H | T], ListaOrdenada) :-
    ordenar_descendente(T, ListaParcial),
    insertar_descendente(H, ListaParcial, ListaOrdenada).

insertar_descendente(X, [], [X]).
insertar_descendente(X, [Y | T], [X, Y | T]) :-
    X >= Y.
insertar_descendente(X, [Y | T], [Y | R]) :-
    X < Y,
    insertar_descendente(X, T, R).


ajustar_as(Valores, ValoresAjustados) :-
    eliminar_primero_ajustar_escalera(14, Valores, ValoresSinAs),
    ValoresAjustados = [1 | ValoresSinAs].

eliminar_primero_ajustar_escalera(_, [], []).
eliminar_primero_ajustar_escalera(X, [X | T], T) :- !.
eliminar_primero_ajustar_escalera(X, [H | T], [H | R]) :-
    eliminar_primero_ajustar_escalera(X, T, R).


    
% ESCALERA COLOR

% Comprobar si todos los palos de la lista son iguales
todos_mismo_palo([Palo | Resto]) :-
    forall(member(Palo, Resto), true).


% Filtrar cartas por palo específico
filtrar_cartas_por_palo([], _, []).
filtrar_cartas_por_palo([[Valor, Palo] | T], PaloBuscado, [[Valor, Palo] | Rest]) :-
    Palo = PaloBuscado,
    filtrar_cartas_por_palo(T, PaloBuscado, Rest).
filtrar_cartas_por_palo([_ | T], PaloBuscado, Rest) :-
    filtrar_cartas_por_palo(T, PaloBuscado, Rest).


% Buscar una sublista de cinco valores consecutivos y del mismo palo
sublista_consecutiva_color([V1, V2, V3, V4, V5 | _], [V1, V2, V3, V4, V5]) :-
    es_escalera([V1, V2, V3, V4, V5]),
    extraer_palos_cartas([[V1, _], [V2, _], [V3, _], [V4, _], [V5, _]], Palos),
    todos_mismo_palo(Palos), !.
sublista_consecutiva_color([_ | Resto], EscaleraColor) :-
    sublista_consecutiva_color(Resto, EscaleraColor).

% Ajustar el valor del As para que actúe como 1 en escalera color
ajustar_as_para_escalera_color(Valores, ValoresAjustados) :-
    ( member(14, Valores),
      member(2, Valores),
      member(3, Valores),
      member(4, Valores),
      member(5, Valores)
    ) ->
    ajustar_as(Valores, ValoresAjustados);
    ValoresAjustados = Valores.



% Obtener la escalera de color más alta de una lista de valores
obtener_escalera_color(Mano, EscaleraColor) :-
    % Filtrar los duplicados y separar por palo
    eliminar_duplicados_escalera(Mano, SinDuplicados),
    extraer_palos_cartas(SinDuplicados, Palos),
    member(Palo, Palos), % Seleccionamos un palo posible
    filtrar_cartas_por_palo(SinDuplicados, Palo, CartasDePalo), 
    extraer_numeros_cartas(CartasDePalo, Valores),
    ajustar_as_para_escalera_color(Valores, ValoresAjustados),
    ordenar_descendente(ValoresAjustados, ValoresOrdenados),
    sublista_consecutiva_color(ValoresOrdenados, EscaleraColor), !.


comparar_escalera_color(ManoJ1, ManoJ2, Resultado) :-
    obtener_escalera_color(ManoJ1, EscaleraColor1),
    obtener_escalera_color(ManoJ2, EscaleraColor2),
    comparar_valor_escalera(EscaleraColor1, EscaleraColor2, Resultado), !. % Depuración




% POKER

% Agrupar cartas para póker (sin duplicados en los grupos)
agrupar_cartas_poker([], []).
agrupar_cartas_poker(Valores, Grupos) :-
    eliminar_duplicados(Valores, ValoresSinDuplicados),
    encontrar_cantidad(ValoresSinDuplicados, Valores, GruposSinDuplicados),
    eliminar_duplicados_grupos(GruposSinDuplicados, Grupos).  % Eliminar duplicados en los grupos

% Eliminar duplicados en la lista de grupos
eliminar_duplicados_grupos([], []).
eliminar_duplicados_grupos([G | Resto], [G | RestoSinDuplicados]) :-
    \\+ member(G, Resto),  % Si G no está en el resto, lo añadimos
    eliminar_duplicados_grupos(Resto, RestoSinDuplicados).
eliminar_duplicados_grupos([_ | Resto], RestoSinDuplicados) :-
    eliminar_duplicados_grupos(Resto, RestoSinDuplicados).  % Ignorar duplicados


 
% Encontrar el kicker más alto que no sea parte del póker
seleccionar_poker([], 0).  % Caso base
seleccionar_poker([[Valor, 4] | _], Valor) :- !.  % Encontramos el póker
seleccionar_poker([_ | T], Poker) :-
    seleccionar_poker(T, Poker).  % Buscamos el póker en el resto de la lista

% Seleccionar kicker del póker, procesando las cartas restantes
seleccionar_kicker_poker(Valores, Poker, Kicker) :-
    exclude(=(Poker), Valores, Restantes),  % Excluir todas las cartas que forman el póker
    maximo(Restantes, Kicker).  % El kicker es la carta más alta restante


obtener_poker(Valores, Poker, Kicker) :-
    agrupar_cartas_poker(Valores, Grupos),  % Agrupar cartas para póker
    seleccionar_poker(Grupos, Poker),  % Encontrar el póker
    seleccionar_kicker_poker(Valores, Poker, Kicker).  % Encontrar el kicker usando las cartas restantes



% Comparar dos manos con póker
comparar_poker(ManoJ1, ManoJ2, Resultado) :-
    extraer_numeros_cartas(ManoJ1, Valores1),
    extraer_numeros_cartas(ManoJ2, Valores2),
    obtener_poker(Valores1, Poker1, Kicker1),
    obtener_poker(Valores2, Poker2, Kicker2),
    comparar_valor_poker(Poker1, Poker2, Kicker1, Kicker2, Resultado), !.

% Comparar los valores de los póker y los kickers
comparar_valor_poker(Poker1, Poker2, Kicker1, Kicker2, Resultado) :-
    (Poker1 > Poker2 -> Resultado = '1';
     Poker2 > Poker1 -> Resultado = '2';
     Kicker1 > Kicker2 -> Resultado = '1';
     Kicker2 > Kicker1 -> Resultado = '2';
     Resultado = '0').



% FULLHOUSE

seleccionar_trio_fullhouse([[Valor, 3] | T], MejorTrio) :-
    seleccionar_trio_fullhouse(T, RestoTrio),
    MejorTrio is max(Valor, RestoTrio), !.
seleccionar_trio_fullhouse([_ | T], Trio) :-
    seleccionar_trio_fullhouse(T, Trio).
seleccionar_trio_fullhouse([], 0).  % Caso base: si no hay tríos, devolver 0.


seleccionar_pareja_fullhouse([], _, 0).  % Caso base
seleccionar_pareja_fullhouse([[Valor, Cantidad] | T], Trio, MejorPareja) :-
    Cantidad >= 2, Valor \\= Trio,  % Es una pareja válida y no es el trío
    seleccionar_pareja_fullhouse(T, Trio, ParejaResto),
    MejorPareja is max(Valor, ParejaResto).  % Seleccionar la pareja más alta
seleccionar_pareja_fullhouse([_ | T], Trio, Pareja) :-
    seleccionar_pareja_fullhouse(T, Trio, Pareja).  % Continuamos si no es válida


obtener_fullhouse(Valores, Trio, Pareja) :-
    agrupar_cartas(Valores, Grupos),
    seleccionar_trio_fullhouse(Grupos, Trio),
    seleccionar_pareja_fullhouse(Grupos, Trio, Pareja).

% Modificar seleccionar_fullhouse para usar las nuevas funciones
seleccionar_fullhouse(Grupos, Trio, Pareja) :-
    seleccionar_trio(Grupos, Trio),  % Usar la nueva función para el trío
    seleccionar_pareja_fullhouse(Grupos, Trio, Pareja).  % Usar la nueva función para la pareja

% Comparar fullhouse
comparar_fullhouse(ManoJ1, ManoJ2, Resultado) :-
    extraer_numeros_cartas(ManoJ1, Valores1),
    extraer_numeros_cartas(ManoJ2, Valores2),
    obtener_fullhouse(Valores1, Trio1, Pareja1),
    obtener_fullhouse(Valores2, Trio2, Pareja2),
    comparar_valor_fullhouse(Trio1, Trio2, Pareja1, Pareja2, Resultado), !.

% Comparar valores de Full House
comparar_valor_fullhouse(Trio1, Trio2, Pareja1, Pareja2, Resultado) :-
    (Trio1 > Trio2 -> Resultado = '1';
     Trio1 < Trio2 -> Resultado = '2';
     (Pareja1 > Pareja2 -> Resultado = '1';
      Pareja1 < Pareja2 -> Resultado = '2';
      Resultado = '0')).



% COLOR

% Seleccionar las cartas de un determinado palo
seleccionar_cartas_del_palo([], _, []).
seleccionar_cartas_del_palo([[Valor, Palo] | T], Palo, [Valor | CartasRestantes]) :-    
    seleccionar_cartas_del_palo(T, Palo, CartasRestantes).
seleccionar_cartas_del_palo([[_Valor, OtroPalo] | T], Palo, CartasRestantes) :-    
    Palo \\= OtroPalo,    
    seleccionar_cartas_del_palo(T, Palo, CartasRestantes).

% Encontrar el palo con más cartas (el color)
seleccionar_color(Palos, PaloDominante) :-
    eliminar_duplicados(Palos, PalosUnicos),
    encontrar_cantidad(PalosUnicos, Palos, GruposDePalos),
    seleccionar_palo_con_mas_cartas(GruposDePalos, PaloDominante).

seleccionar_palo_con_mas_cartas([[Palo, Cantidad] | Resto], MejorPalo) :-    
    seleccionar_palo_con_mas_cartas(Resto, Palo, Cantidad, MejorPalo).

seleccionar_palo_con_mas_cartas([], PaloActual, _, PaloActual).
seleccionar_palo_con_mas_cartas([[Palo, Cantidad] | Resto], PaloActual, CantidadActual, MejorPalo) :-    
    (Cantidad > CantidadActual -> seleccionar_palo_con_mas_cartas(Resto, Palo, Cantidad, MejorPalo);
     seleccionar_palo_con_mas_cartas(Resto, PaloActual, CantidadActual, MejorPalo)).

% Obtener el color (cartas del mismo palo) y las 5 cartas más altas
obtener_color(Mano, Palo, CartasColor) :-    
    extraer_palos_cartas(Mano, Palos),
    seleccionar_color(Palos, Palo),  % Encontrar el palo predominante
    seleccionar_cartas_del_palo(Mano, Palo, CartasDelColor),
    sort(CartasDelColor, CartasOrdenadas),
    reverse(CartasOrdenadas, [C1, C2, C3, C4, C5 | _]),  % Tomar las 5 cartas más altas
    CartasColor = [C1, C2, C3, C4, C5].

% Comparar manos con color
comparar_color(ManoJ1, ManoJ2, Resultado) :-    
    obtener_color(ManoJ1, _, CartasColor1),    
    obtener_color(ManoJ2, _, CartasColor2),    
    comparar_cartas_color(CartasColor1, CartasColor2, Resultado), !.

% Comparar las cartas de los colores
comparar_cartas_color([], [], '0').
comparar_cartas_color([C1 | R1], [C2 | R2], Resultado) :-    
    (C1 > C2 -> Resultado = '1';     
     C2 > C1 -> Resultado = '2';     
     comparar_cartas_color(R1, R2, Resultado)).



% ESCALERA

% Ajustar el valor del As para que actúe como 1
ajustar_as_para_escalera(Valores, ValoresAjustados) :-
    ( member(14, Valores),
      member(2, Valores),
      member(3, Valores),
      member(4, Valores),
      member(5, Valores)
    ) ->
    ajustar_as(Valores, ValoresAjustados);
    ValoresAjustados = Valores.


% Buscar una sublista de cinco valores consecutivos
sublista_consecutiva([V1, V2, V3, V4, V5 | _], [V1, V2, V3, V4, V5]) :-
    es_escalera([V1, V2, V3, V4, V5]), !.
sublista_consecutiva([_ | Resto], Escalera) :-
    sublista_consecutiva(Resto, Escalera).

% Obtener la escalera más alta de una lista de valores
obtener_escalera(Valores, Escalera) :-
    eliminar_duplicados_escalera(Valores, SinDuplicados),
    ajustar_as_para_escalera(SinDuplicados, ValoresAjustados),
    ordenar_descendente(ValoresAjustados, ValoresOrdenados),
    sublista_consecutiva(ValoresOrdenados, Escalera).


% Comparar escaleras entre dos manos
comparar_escalera(ManoJ1, ManoJ2, Resultado) :-
    extraer_numeros_cartas(ManoJ1, Valores1),
    extraer_numeros_cartas(ManoJ2, Valores2),
    obtener_escalera(Valores1, Escalera1),
    obtener_escalera(Valores2, Escalera2),
    comparar_valor_escalera(Escalera1, Escalera2, Resultado), !. 



% TRIO

obtener_trio(Valores, Trio, Kickers) :-    
    agrupar_cartas(Valores, Grupos),    
    seleccionar_trio(Grupos, Trio),  % Encontrar el trío
    seleccionar_kickers_trio(Valores, Trio, KickersSinOrdenar),
    sort(KickersSinOrdenar, KickersOrdenados), 
    reverse(KickersOrdenados, [Kicker1, Kicker2 | _]),  % Seleccionar los dos kickers más altos
    Kickers = [Kicker1, Kicker2].

% Seleccionar los kickers (cartas que no son parte del trío)
seleccionar_kickers_trio([], _, []).
seleccionar_kickers_trio([Valor | T], Trio, [Valor | KickerRest]) :-    
    Valor \\= Trio, !, 
    seleccionar_kickers_trio(T, Trio, KickerRest).
seleccionar_kickers_trio([_ | T], Trio, Kickers) :-    
    seleccionar_kickers_trio(T, Trio, Kickers).

% Comparar manos con trío
comparar_trio(ManoJ1, ManoJ2, Resultado) :-    
    extraer_numeros_cartas(ManoJ1, Valores1),    
    extraer_numeros_cartas(ManoJ2, Valores2),    
    obtener_trio(Valores1, Trio1, Kickers1),    
    obtener_trio(Valores2, Trio2, Kickers2),    
    comparar_valor_trio(Trio1, Trio2, Kickers1, Kickers2, Resultado), !.

% Comparar tríos y luego los kickers si los tríos son iguales
comparar_valor_trio(Trio1, Trio2, Kickers1, Kickers2, Resultado) :-    
    (Trio1 > Trio2 -> Resultado = '1';     
     Trio2 > Trio1 -> Resultado = '2';     
     comparar_kickers(Kickers1, Kickers2, Resultado)).


% DOBLES PAREJAS 

seleccionar_dobles_parejas([], 0, 0).  % Caso base
seleccionar_dobles_parejas([[Valor, 2], [Valor2, 2] | _], Valor, Valor2) :- 
    Valor > Valor2, !.
seleccionar_dobles_parejas([[Valor2, 2], [Valor, 2] | _], Valor, Valor2) :- 
    Valor2 > Valor, !.
seleccionar_dobles_parejas([[Valor, 2] | T], Valor, Pareja2) :-    
    seleccionar_dobles_parejas(T, Pareja2, _).  % Buscamos la segunda pareja en el resto de la lista
seleccionar_dobles_parejas([_ | T], Pareja1, Pareja2) :-    
    seleccionar_dobles_parejas(T, Pareja1, Pareja2).  % Buscar ambas parejas en el resto de la lista

% Obtener las dobles parejas y la carta kicker restante
obtener_dobles_parejas(Valores, Pareja1, Pareja2, Kicker) :-    
    agrupar_cartas(Valores, Grupos),    
    seleccionar_dobles_parejas(Grupos, Pareja1, Pareja2),  % Encontrar las dobles parejas
    seleccionar_kickers_dobles_parejas(Valores, Pareja1, Pareja2, KickersSinOrdenar),  % Encontrar el kicker restante
    sort(KickersSinOrdenar, KickersOrdenados), 
    reverse(KickersOrdenados, [Kicker | _]).  % Obtener el kicker más alto

% Seleccionar los kickers (cartas que no son parte de las parejas)
seleccionar_kickers_dobles_parejas([], _, _, []).
seleccionar_kickers_dobles_parejas([Valor | T], Pareja1, Pareja2, [Valor | KickerRest]) :-    
    Valor \\= Pareja1, Valor \\= Pareja2, !, 
    seleccionar_kickers_dobles_parejas(T, Pareja1, Pareja2, KickerRest).
seleccionar_kickers_dobles_parejas([_ | T], Pareja1, Pareja2, Kickers) :-    
    seleccionar_kickers_dobles_parejas(T, Pareja1, Pareja2, Kickers).

% Comparar dobles parejas
comparar_dobles_parejas(ManoJ1, ManoJ2, Resultado) :-    
    extraer_numeros_cartas(ManoJ1, Valores1),    
    extraer_numeros_cartas(ManoJ2, Valores2),    
    obtener_dobles_parejas(Valores1, Pareja1_1, Pareja2_1, Kicker1),    
    obtener_dobles_parejas(Valores2, Pareja1_2, Pareja2_2, Kicker2),    
    comparar_valor_dobles_parejas(Pareja1_1, Pareja2_1, Pareja1_2, Pareja2_2, Kicker1, Kicker2, Resultado), !.


% Comparar las parejas en orden (pareja más alta, luego segunda pareja, luego kicker)
comparar_valor_dobles_parejas(Pareja1_1, Pareja2_1, Pareja1_2, Pareja2_2, Kicker1, Kicker2, Resultado) :-    
    (Pareja1_1 > Pareja1_2 -> Resultado = '1';     
     Pareja1_2 > Pareja1_1 -> Resultado = '2';     
     Pareja2_1 > Pareja2_2 -> Resultado = '1';     
     Pareja2_2 > Pareja2_1 -> Resultado = '2';     
     Kicker1 > Kicker2 -> Resultado = '0';
     Kicker1 < Kicker2 -> Resultado = '0';
     Resultado = '0').



% PAREJA

seleccionar_pareja([], 0).  % Caso base
seleccionar_pareja([[Valor, 2] | _], Valor) :- !.  % Encontramos la pareja
seleccionar_pareja([_ | T], Pareja) :-    
    seleccionar_pareja(T, Pareja).  % Buscamos la pareja en el resto de la lista

obtener_pareja(Valores, Pareja, Kickers) :-
    agrupar_cartas(Valores, Grupos),
    seleccionar_pareja(Grupos, Pareja),
    seleccionar_kickers_pareja(Valores, Pareja, KickersSinOrdenar),
    sort(KickersSinOrdenar, KickersOrdenados),
    reverse(KickersOrdenados, KickersOrdenadosDesc),
    tomar_n_primeros(KickersOrdenadosDesc, 3, Kickers).

% Predicado para tomar los primeros N elementos de una lista
tomar_n_primeros(_, 0, []) :- !.
tomar_n_primeros([], _, []) :- !.
tomar_n_primeros([H | T], N, [H | R]) :-
    N > 0,
    N1 is N - 1,
    tomar_n_primeros(T, N1, R).


seleccionar_kickers_pareja([], _, []).
seleccionar_kickers_pareja([Valor | T], Pareja, [Valor | KickerRest]) :-
    Valor \\= Pareja, !,
    seleccionar_kickers_pareja(T, Pareja, KickerRest).
seleccionar_kickers_pareja([_ | T], Pareja, Kickers) :-
    seleccionar_kickers_pareja(T, Pareja, Kickers).

comparar_pareja(ManoJ1, ManoJ2, Resultado) :-
    extraer_numeros_cartas(ManoJ1, Valores1),
    extraer_numeros_cartas(ManoJ2, Valores2),
    obtener_pareja(Valores1, Pareja1, Kickers1),
    obtener_pareja(Valores2, Pareja2, Kickers2),
    comparar_valor_pareja(Pareja1, Pareja2, Kickers1, Kickers2, Resultado), !.

comparar_valor_pareja(Pareja1, Pareja2, Kickers1, Kickers2, Resultado) :-
    (Pareja1 > Pareja2 -> Resultado = '1';
     Pareja2 > Pareja1 -> Resultado = '2';
     comparar_kickers(Kickers1, Kickers2, Resultado)).



% CARTA ALTA

obtener_cinco_mayores(Valores, CincoMayores) :-    
    sort(Valores, ValoresOrdenados),   
    reverse(ValoresOrdenados, ValoresOrdenadosReversos),    
    sublist(ValoresOrdenadosReversos, CincoMayores, 5).

sublist(Lista, Sublista, N) :-    
    length(Sublista, N),    
    append(Sublista, _, Lista).

comparar_carta_alta(ManoJ1, ManoJ2, Resultado) :-    
    extraer_numeros_cartas(ManoJ1, Valores1),    
    extraer_numeros_cartas(ManoJ2, Valores2),    
    obtener_cinco_mayores(Valores1, CincoMayores1),    
    obtener_cinco_mayores(Valores2, CincoMayores2),    
    comparar_cinco_mayores(CincoMayores1, CincoMayores2, Resultado), !.

comparar_cinco_mayores([], [], '0').
comparar_cinco_mayores([C1 | R1], [C2 | R2], Resultado) :-    
    (C1 > C2 -> Resultado = '1';     
     C2 > C1 -> Resultado = '2';     
     comparar_cinco_mayores(R1, R2, Resultado)).

`;

var listadoCartasOriginalObject = [
  {value: 'Sin seleccionar', text:'Sin seleccionar', selected: true},
  {value:'2 C',text: '2 ♥', selected: false },
  {value:'3 C',text: '3 ♥', selected: false },
  {value:'4 C',text: '4 ♥', selected: false },
  {value:'5 C',text: '5 ♥', selected: false },
  {value:'6 C',text: '6 ♥', selected: false },
  {value:'7 C',text: '7 ♥', selected: false },
  {value:'8 C',text: '8 ♥', selected: false },
  {value:'9 C',text: '9 ♥', selected: false },
  {value:'10 C',text: '10 ♥', selected: false },
  {value:'11 C',text: 'J ♥', selected: false },
  {value:'12 C',text: 'Q ♥', selected: false },
  {value:'13 C',text: 'K ♥', selected: false },
  {value:'14 C',text: 'A ♥', selected: false },
  {value:'2 D',text: '2 ♦', selected: false },
  {value:'3 D',text: '3 ♦', selected: false },
  {value:'4 D',text: '4 ♦', selected: false },
  {value:'5 D',text: '5 ♦', selected: false },
  {value:'6 D',text: '6 ♦', selected: false },
  {value:'7 D',text: '7 ♦', selected: false },
  {value:'8 D',text: '8 ♦', selected: false },
  {value:'9 D',text: '9 ♦', selected: false },
  {value:'10 D',text: '10 ♦', selected: false },
  {value:'11 D',text: 'J ♦', selected: false },
  {value:'12 D',text: 'Q ♦', selected: false },
  {value:'13 D',text: 'K ♦', selected: false },
  {value:'14 D',text: 'A ♦', selected: false },
  {value:'2 T',text: '2 ♣', selected: false },
  {value:'3 T',text: '3 ♣', selected: false },
  {value:'4 T',text: '4 ♣', selected: false },
  {value:'5 T',text: '5 ♣', selected: false },
  {value:'6 T',text: '6 ♣', selected: false },
  {value:'7 T',text: '7 ♣', selected: false },
  {value:'8 T',text: '8 ♣', selected: false },
  {value:'9 T',text: '9 ♣', selected: false },
  {value:'10 T',text: '10 ♣', selected: false },
  {value:'11 T',text: 'J ♣', selected: false },
  {value:'12 T',text: 'Q ♣', selected: false },
  {value:'13 T',text: 'K ♣', selected: false },
  {value:'14 T',text: 'A ♣', selected: false },
  {value:'2 P',text: '2 ♠', selected: false },
  {value:'3 P',text: '3 ♠', selected: false },
  {value:'4 P',text: '4 ♠', selected: false },
  {value:'5 P',text: '5 ♠', selected: false },
  {value:'6 P',text: '6 ♠', selected: false },
  {value:'7 P',text: '7 ♠', selected: false },
  {value:'8 P',text: '8 ♠', selected: false },
  {value:'9 P',text: '9 ♠', selected: false },
  {value:'10 P',text: '10 ♠', selected: false },
  {value:'11 P',text: 'J ♠', selected: false },
  {value:'12 P',text: 'Q ♠', selected: false },
  {value:'13 P',text: 'K ♠', selected: false },
  {value:'14 P',text: 'A ♠', selected: false }
];
  
var listadoCartasFiltradoObject = [...listadoCartasOriginalObject];

/*variables seteadas por el usuario */

var valorRandomCarta1Mesa;
var valorRandomCarta2Mesa;
var valorRandomCarta3Mesa;
var valorRandomCarta4Mesa;
var valorRandomCarta5Mesa;

var listadoCartasMesa = [];

class ManoJugador{
  nombre = null; //nombre del jugador ej J1, J2, etc...
  carta1 = null;
  carta2 = null;
  resultadoMano = null; //aca guarda el numero 

  constructor(nombre,carta1, carta2, resultadoMano){
    this.nombre = nombre;
    this.carta1 = carta1;
    this.carta2 = carta2;
    this.resultadoMano = resultadoMano;
  }

  getJsonManoJugador(){
    return {nombre: this.nombre, carta1: this.carta1, carta2: this.carta2, resultadoMano: this.resultadoMano};
  }

  getManoJugador(){
    let numeroCarta1 = extraerNumero(this.carta1);
    let colorCarta1 = extraerColor(this.carta1);

    let numeroCarta2 = extraerNumero(this.carta2);
    let colorCarta2 = extraerColor(this.carta2);

    return `[${numeroCarta1}|${colorCarta1}], [${numeroCarta2}|${colorCarta2}]`;

  }

  getManosCartasDeLaMesa(){
    let manosMesa = '';
    let numeroCarta = null;
    let colorCarta = null;

    for (let index = 0; index < listadoCartasMesa.length; index++) {
      let carta = listadoCartasMesa[index].value;
     
      numeroCarta = extraerNumero(carta);
      colorCarta = extraerColor(carta);

      if(index == 4){
        manosMesa += `[${numeroCarta}|${colorCarta}]`;
      }else{
        manosMesa += `[${numeroCarta}|${colorCarta}],`;
      }     
    }
    console.log(' manos mesa ', manosMesa);
    return manosMesa;
  }

  setCarta1(carta1){
    this.carta1 = carta1;
  }

  setCarta2(carta2){
    this.carta2 = carta2;
  }

  setResultadoMano(resultadoMano){
    this.resultadoMano = resultadoMano;
  }

}

//se guarda en cada posicion el valor de la carta de la mesa
let cartasMesa = []; 

//es un array de arrays, en donde en cada posicion del array interno guardo dos valores, carta 1 y carta 2 de cada jugador. Ej [[2,3],[3,4],[4,5]] el index 0 corresponde al jugador1, el index 1 al jugador2, etc...
//es un array de tipo ManoJugador
var valoresCartasManoJugadores = []; 

var cantJugadores = 1;

var listadoCartasCarta1Mesa = listadoCartasOriginalObject.filter(item => item.value !== "Sin seleccionar");

function extraerNumero(carta) {
  let valoresCarta = carta.split(' ');

  let numeroCarta = valoresCarta[0];
  return numeroCarta.toLowerCase();
}

function extraerColor(carta) {
  
  let valoresCarta = carta.split(' ');

  let colorCarta = valoresCarta[1];
  return colorCarta.toLowerCase();
}

function getNombreMano(resultadoMano){
  let nombreMano = null;
  switch (resultadoMano){
    case 1:
      nombreMano = 'Escalera Real';
      break;
    case 2:
      nombreMano = 'Escalera Color';
      break;
    case 3:
      nombreMano = 'Poker';
      break;
    case 4:
      nombreMano = 'Full House';
      break;  
    case 5:
      nombreMano = 'Color';
      break;  
    case 6:
      nombreMano = 'Escalera';
      break;  
    case 7:
      nombreMano = 'Trio';
      break;
    case 8:
      nombreMano = 'Doble Pareja';
      break;  
    case 9:
      nombreMano = 'Pareja';
      break;  
    case 10:
      nombreMano = 'Carta Alta';
      break;  
  }

  return nombreMano;
}

async function consultarManosJugadores(){

  console.log('consultarManosJugadores valoresCartasManoJugadores ', valoresCartasManoJugadores);

  for (let index = 0; index < valoresCartasManoJugadores.length; index++) {

    let numeroCarta1 = extraerNumero(valoresCartasManoJugadores[index].carta1);
    let colorCarta1 = extraerColor(valoresCartasManoJugadores[index].carta1);

    let numeroCarta2 = extraerNumero(valoresCartasManoJugadores[index].carta2);
    let colorCarta2 = extraerColor(valoresCartasManoJugadores[index].carta2);
    
    let resultadoMano = await evaluarMano(numeroCarta1, colorCarta1, numeroCarta2, colorCarta2);

    // let nombreJugador = valoresCartasManoJugadores[index].nombre;

    valoresCartasManoJugadores[index].resultadoMano = parseInt(resultadoMano);

    // valoresCartasManoJugadores[index].nombreResultadoMano = valoresCartasManoJugadores[index].getNombreMano();

    let manoJugadorHTML = document.getElementById(`manoJugador${index+1}`);

    if(manoJugadorHTML){
      manoJugadorHTML.textContent = `Mano del Jugador: ${this.getNombreMano(parseInt(resultadoMano))}`;
    }

  }
  
}

function evaluarCartasRepetidas(){
  let carta1Anterior;
  let carta2Anterior;
  let cartasRepetidas = false;

  let cartasJugadores = valoresCartasManoJugadores.map(valoresCartasManoJugadores => valoresCartasManoJugadores.carta1).concat(valoresCartasManoJugadores.map(valoresCartasManoJugadores => valoresCartasManoJugadores.carta2));

  // console.log('evaluarCartasRepetidas cartasJugadores ',  cartasJugadores);
  // console.log('evaluarCartasRepetidas listadoCartasMesa ',  listadoCartasMesa);

  for (let index = 0; index < valoresCartasManoJugadores.length; index++) {
    let carta1 = valoresCartasManoJugadores[index].carta1;
    let carta2 = valoresCartasManoJugadores[index].carta2;
    
    // console.log('evaluarCartasRepetidas carta1' , carta1,' carta2 ', carta2);

    //pregunta si carta 1 y carta 2 son iguales
    //si carta 1 y carta 2 esta mas de una vez en las cartas de los jugadores
    //si carta 1 y carta 2 estan en las cartas mesa
    if(carta1 == carta2 || cartasJugadores.filter(carta => carta == carta1).length > 1 || cartasJugadores.filter(carta => carta == carta2).length > 1 || listadoCartasMesa.filter(cartaMesa => cartaMesa.value == carta1).length > 0 || listadoCartasMesa.filter(cartaMesa => cartaMesa.value == carta2).length > 0){
      cartasRepetidas = true;
      break;
    }else{
      carta1Anterior = carta1;
      carta2Anterior = carta2;
    }

  }

  return cartasRepetidas;
}

function habilitarDeshabilitarCartas(deshabilitar){
  for (let index = 1; index < 6; index++) {
    let selectCartasMesaHTML = document.getElementById(`carta${index}-mesa`);
    
    if(selectCartasMesaHTML){
      if(deshabilitar==true){
        selectCartasMesaHTML.setAttribute("disabled", deshabilitar);
      }else{
        selectCartasMesaHTML.removeAttribute("disabled");
      } 
    }
  }

  for (let j = 1; j <= cantJugadores; j++) {
    for(let i = 1; i <= 2; i++){
      let selectCartasJugadoresHTML = document.getElementById(`carta${i}-jugador${j}`);
      if(selectCartasJugadoresHTML){
        if(deshabilitar==true){
          selectCartasJugadoresHTML.setAttribute("disabled", deshabilitar);
        }else{
          selectCartasJugadoresHTML.removeAttribute("disabled");
        } 
      }
    }
  }

}

function habilitarSeleccionDeCartas(){
  //deshabilitar el boton de evaluar
  let botonEvaluarManoGanadora = document.getElementById("boton-evaluar-mano");
  botonEvaluarManoGanadora.hidden = true;

  //habilitar el boton de cartas aleatorias
  let botonCartasMesaRandom = document.getElementById("boton-cartas-random-mesa");
  botonCartasMesaRandom.removeAttribute("disabled");

  let botonCartasMesaRandomTodos = document.getElementById("boton-cartas-random-todos");
  botonCartasMesaRandomTodos.removeAttribute("disabled");

  let jugadorGanadorDocumento = document.getElementById('jugador-ganador');
  if(jugadorGanadorDocumento){
    jugadorGanadorDocumento.hidden = true;

  }

  //habilitar las cartas de las manos y mesa para que no se puedan cambiar
  habilitarDeshabilitarCartas(false);

}

//se fija si estan todas las cartas selecionadas
function evaluarTodasLasCartasDeLaMesaSeleccionadas(){
  let cartasSinSeleccionarDeLaMesa = false;
  for (let index = 1; index < 6; index++) {
    let selectCartasMesaHTML = document.getElementById(`carta${index}-mesa`);
    if(selectCartasMesaHTML){
      for(var i, j = 0; i = selectCartasMesaHTML.options[j]; j++) {
        if(i.selected == true && i.value == 'Sin seleccionar') {
          return true;
        }
      }
    }
  }

  return cartasSinSeleccionarDeLaMesa;

}

async function obtenerManosDeJugadores() {

  if(evaluarTodasLasCartasDeLaMesaSeleccionadas() == true){
    alert('Falta selecionar cartas de la Mesa, no es posible evaluar la mano');
    return;
  }

  if(evaluarCartasRepetidas() == true){
    console.log('Cartas repetidas');
    alert('Existen cartas repetidas, no es posible evaluar la mano');
    return;
  }

  //habilitar el boton de evaluar
  const botonEvaluarManoGanadora = document.getElementById("boton-evaluar-mano");
  botonEvaluarManoGanadora.hidden = false;

  await consultarManosJugadores();

  //deshabilitar el boton de cartas aleatorias
  const botonCartasMesaRandom = document.getElementById("boton-cartas-random-mesa");
  botonCartasMesaRandom.setAttribute("disabled", "true");

  let botonCartasMesaRandomTodos = document.getElementById("boton-cartas-random-todos");
  botonCartasMesaRandomTodos.setAttribute("disabled", "true");

  //bloquear las cartas de las manos y mesa para que no se puedan cambiar
  habilitarDeshabilitarCartas(true);
}

let jugadorGanador = new ManoJugador();

async function evaluarGanador(){

  console.log('Jugador ganador valoresCartasManoJugadores: ', valoresCartasManoJugadores);
  let jugadorGanadorDocumento = document.getElementById('jugador-ganador');
  if(jugadorGanadorDocumento){
    jugadorGanadorDocumento.hidden = false;
    jugadorGanadorDocumento.textContent = '';
  }

  let jugadoresGanadores = [];

  let ganadorJugador = valoresCartasManoJugadores[0];

  jugadoresGanadores.push(ganadorJugador);

  for (let index = 1; index < valoresCartasManoJugadores.length; index++) {
    if(valoresCartasManoJugadores[index].resultadoMano < ganadorJugador.resultadoMano){
      jugadoresGanadores = [];
      ganadorJugador = valoresCartasManoJugadores[index];
      jugadoresGanadores.push(ganadorJugador);
    }else if(valoresCartasManoJugadores[index].resultadoMano == ganadorJugador.resultadoMano){
      jugadoresGanadores.push(valoresCartasManoJugadores[index]);
    }
    // resultadoJugadorGanador = valoresCartasManoJugadores[index].resultadoMano;
  }
  //Aca ya tengo si hay uno o mas ganadores

  if(jugadoresGanadores.length >= 2){

    let ganadoresAux = [];

    //Mas de un ganador (llamar a desempate):
    console.log("Los ganadores EMPATADOS son:\n");
    jugadoresGanadores.forEach( (jugador) => console.log(jugador.nombre) );

    let ganador = await desempatar(jugadoresGanadores);


    switch(ganador){
      case '0' :
        ganadoresAux.push(jugadoresGanadores[0]);
        ganadoresAux.push(jugadoresGanadores[1]);
        ganadorJugador = jugadoresGanadores[0];
        break;
      case '1' :
        ganadoresAux.push(jugadoresGanadores[0]);
        ganadorJugador = jugadoresGanadores[0];
        break;
      case '2' :
        ganadoresAux.push(jugadoresGanadores[1]);
        ganadorJugador = jugadoresGanadores[1];
        break;
    }
      
    
  
    for (let index = 2; index < jugadoresGanadores.length; index++) {
      ganador = await desempatar([ganadorJugador, jugadoresGanadores[index]]);

      switch(ganador){
        case '0' :
          ganadoresAux.push(jugadoresGanadores[index]);
          break;
        case '1' :
          //Nada
          break;
        case '2' :
          ganadoresAux = [];
          ganadoresAux.push(jugadoresGanadores[index]);
          ganadorJugador = jugadoresGanadores[index];
          break;
      }

    }
  
    if(ganadoresAux.length === 1){
      console.log("Ganador:" + ganadorJugador.nombre);
      jugadorGanadorDocumento.textContent = `El Ganador es:  ${ganadorJugador.nombre}`;
    }else{
      jugadorGanadorDocumento.textContent = `Empate entre los siguientes jugadores: `;
      console.log("Empate entre los siguientes jugadores:");
      ganadoresAux.forEach( (jugador) => {
        console.log(jugador.nombre) 
        jugadorGanadorDocumento.textContent = jugadorGanadorDocumento.textContent + jugador.nombre + ', ';
      }
        
      );
    }

    

  }else{
    console.log("Ganador:" + ganadorJugador.nombre);

    if(jugadorGanadorDocumento){
      jugadorGanadorDocumento.textContent = `El Ganador es:  ${ganadorJugador.nombre}`;
    }

  }


  //   // for (let index = 1; index < cantJugadores; index++) {
  //   //   let nombreJugador1 = jugadorGanador.nombre;
  //   //   let manoJugador1 = jugadorGanador.resultadoMano;

  //   //   let nombreJugador2 = valoresCartasManoJugadores[index].nombre;
  //   //   let manoJugador2 = valoresCartasManoJugadores[index].resultadoMano;

  //   //   let nombreJugadorGanador = await evaluarManoGanadora(nombreJugador1, manoJugador1, nombreJugador2, manoJugador2);
  //   //   // console.log('Jugador ganador nombreJugadorGanador: ', nombreJugadorGanador, ' Entre Jugadores :', nombreJugador1, ' y ', nombreJugador2);

  //   //   jugadorGanador = valoresCartasManoJugadores.filter(jugador => jugador.nombre == nombreJugadorGanador)[0];
  //   //   console.log('Jugador ganador: ', jugadorGanador);
  //   // }
  //  console.log('zzz jugadoresGanadores ', jugadoresGanadores);
  //}

  //if(jugadorGanadorDocumento){
  //  jugadorGanadorDocumento.hidden = false;
  //  jugadorGanadorDocumento.textContent = `Jugador ganador: ${jugadorGanador.nombre}`;
  // }
  
}
//devuelve '1' si gano el primer jugador o '2' si gano el segundo y '0' en caso de empate 
async function desempatar(manos){
  let mano = manos[0].resultadoMano

  let ganador = '';


  let query = null;

  const manoJ1 = '[' + manos[0].getManoJugador() + ', ' + manos[0].getManosCartasDeLaMesa() + ']';
  const manoJ2 = '[' + manos[1].getManoJugador() + ', ' + manos[1].getManosCartasDeLaMesa() + ']';

  switch(mano){
    case 1:
      return '0';
    case 2:
      query = 'comparar_escalera_color(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 3:
      query = 'comparar_poker(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 4:
      query = 'comparar_fullhouse(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 5:
      query = 'comparar_color(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 6:
      query = 'comparar_escalera(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 7:
      query = 'comparar_trio(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 8:
      query = 'comparar_dobles_parejas(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 9:
      query = 'comparar_pareja(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
      break;
    case 10:
      query = 'comparar_carta_alta(' + manoJ1.replaceAll('|', ',') + ',' + manoJ2.replaceAll('|', ',') + ', Resultado).';
    break;
  }



  console.log("query " + query);

  if(query){

    const answer =  await session.promiseQuery(query);
    const answers =  await session.promiseAnswers(answer, 100000);
    let nextAnswer =  await answers.next();

    while (!nextAnswer.done) {
      const answer = nextAnswer.value;

      // console.log('resuesta de la consulta evaluarMano : ', answer);
      if (answer.links?.hasOwnProperty('Resultado')) {
        const manoTerm = answer.links['Resultado'];

        const manoValue = manoTerm.toString();

        let resultado = '';
        for (let index = 0; index < manoValue.length; index++) {
          const element = manoValue[index];
          if(element != ',' && element != ']' && element != '[') {
            resultado = resultado + element;
          }
        }

        ganador = resultado;

      }
      nextAnswer = await answers.next();
    };
  }else{
    console.error('No se armo el query para desempatar.')
  }
  
  console.log("Resultado del desempate " + ganador);


  return ganador;
}

async function evaluarMano(numeroCarta1, colorCarta1, numeroCarta2, colorCarta2) {

    try {

        let numeroCarta1Mesa;
        let colorCarta1Mesa;

        let numeroCarta2Mesa;
        let colorCarta2Mesa;

        let numeroCarta3Mesa;
        let colorCarta3Mesa;

        let numeroCarta4Mesa;
        let colorCarta4Mesa;

        let numeroCarta5Mesa;
        let colorCarta5Mesa;

        console.log('evaluarMano listadoCartasMesa', listadoCartasMesa)

        for (let index = 0; index < listadoCartasMesa.length; index++) {
          let carta = listadoCartasMesa[index].value;
          switch(index){
            case 0:
              numeroCarta1Mesa = extraerNumero(carta);
              colorCarta1Mesa = extraerColor(carta);
              break;
            case 1:
              numeroCarta2Mesa = extraerNumero(carta);
              colorCarta2Mesa = extraerColor(carta);
              break;
            case 2:
              numeroCarta3Mesa = extraerNumero(carta);
              colorCarta3Mesa = extraerColor(carta);
              break;
            case 3:
              numeroCarta4Mesa = extraerNumero(carta);
              colorCarta4Mesa = extraerColor(carta);
              break;
            case 4:
              numeroCarta5Mesa = extraerNumero(carta);
              colorCarta5Mesa = extraerColor(carta);
              break;
          }
          
        }

        await session.promiseConsult(program);
        // var query = "mano([3|c4], [10|c2], [12|c5], [13|c1], [10|c1], [2|c6], [10|c7], Mano).";
        var query = `mano([${numeroCarta1}|${colorCarta1}], [${numeroCarta2}|${colorCarta2}],
         [${numeroCarta1Mesa}|${colorCarta1Mesa}], [${numeroCarta2Mesa}|${colorCarta2Mesa}], [${numeroCarta3Mesa}|${colorCarta3Mesa}], 
         [${numeroCarta4Mesa}|${colorCarta4Mesa}], [${numeroCarta5Mesa}|${colorCarta5Mesa}], Mano).`;

         console.log('evaluarMano query ', query)

        const answer =  await session.promiseQuery(query);
        const answers =  await session.promiseAnswers(answer, 100000);
        let nextAnswer =  await answers.next();
        console.log("evaluarMano nextAnswer ", nextAnswer);

        while (!nextAnswer.done) {
          const answer = nextAnswer.value;

          // console.log('resuesta de la consulta evaluarMano : ', answer);
          if (answer.links?.hasOwnProperty('Mano')) {
            const manoTerm = answer.links['Mano'];

            const manoValue = manoTerm.toString();

            console.log('evaluarMano manoValue : ', manoValue);

            let resultado = '';
            for (let index = 0; index < manoValue.length; index++) {
              const element = manoValue[index];
              if(element != ',' && element != ']' && element != '[') {
                resultado = resultado + element;
              }
            }
              console.log("evaluarMano resultado: ", resultado);

            //await evaluarGanador();

            return resultado;

          }
          nextAnswer = await answers.next();
        }


    } catch (err) {
        console.error(err);
        console.log('Error en evaluarMano: ', err);
        
    }

};

// async function evaluarManoGanadora(nombreJugador1, manoJugador1, nombreJugador2, manoJugador2) {
//   try {
//       await session.promiseConsult(program);

//       // // Consulta para capturar el ganador
//       var query = `evaluar_mano_ganadora(${nombreJugador1}, ${manoJugador1}, ${nombreJugador2}, ${manoJugador2}, G).`;

//       console.log('evaluarManoGanadora query: ', query);

//       const answer = await session.promiseQuery(query);

//       // console.log('evaluarManoGanadora answer: ', answer);

//       const answers = await session.promiseAnswers(answer, 100000);

//       // console.log("evaluarManoGanadora answers ", answers);

//       let nextAnswer = await answers.next();

//       // console.log("evaluarManoGanadora nextAnswer ", nextAnswer);

      
//       while (!nextAnswer.done) {
//         const answer = nextAnswer.value;

//         // Capturando el ganador
//         if (answer.links?.hasOwnProperty('G')) {
//           const ganador = answer.links['G'];
//           console.log("evaluarManoGanadora ganador ", ganador);
          
//           // console.log("evaluarManoGanadora ganador g ", ganador);
//           for (let key in answer.links) {
//             if (answer.links.hasOwnProperty(key) && answer.links[key].id ==ganador.id ) {
//               return key;
//             }
//           }
            
//         }
//         nextAnswer = await answers.next();
//       }

//   } catch (err) {
//       console.error(err);
//       console.log('Error en evaluarManoGanadora: ', err);
//   }
// };

function setearCartasRandomTodos(){

  let listadoPosibleDeCartas = listadoCartasOriginalObject.filter(item => item.value !='Sin seleccionar');

  valoresCartasManoJugadores = [];

  for (let j = 1; j <= cantJugadores; j++) {

    for(let i = 1; i <= 2; i++){

      for (let index = 0; index < valoresCartasManoJugadores.length; index++) {
        let manoCarta = valoresCartasManoJugadores[index];
                
        // Encontrar el índice del valor
        let indiceCarta1 = listadoPosibleDeCartas.map(carta => carta.value).indexOf(manoCarta.carta1);

        if (indiceCarta1 !== -1) {
          // Eliminar el elemento en ese índice
          listadoPosibleDeCartas.splice(indiceCarta1, 1);
        }

        let indiceCarta2 = listadoPosibleDeCartas.map(carta => carta.value).indexOf(manoCarta.carta2);

        if (indiceCarta2 !== -1) {
          // Eliminar el elemento en ese índice
          listadoPosibleDeCartas.splice(indiceCarta2, 1);
        }
        
      }
      
      let valorRandomCarta1 = getValorRandomInListado(listadoPosibleDeCartas);
      selectDefaultOption(valorRandomCarta1.value,`carta${i}-jugador${j}`);

      onChangeCarta(i,j,valorRandomCarta1.value)

    }
  }

  setearCartasMesaRandom();
  
};

function onChangeCarta(numeroCarta, numeroJugador, valorCarta){ //se ejecuta cuando un jugador cambia de carta en su mano

  let manoJugador = valoresCartasManoJugadores.filter(mano => mano.nombre == 'J'+numeroJugador.toString())[0];

  let cartaAnterior = null;

  if(manoJugador){
    if(numeroCarta ==1){
      cartaAnterior = manoJugador.carta1;
      manoJugador.carta1 = valorCarta;
    }else{
      cartaAnterior = manoJugador.carta2;
      manoJugador.carta2 = valorCarta;
    }
  }else{

    manoJugador = new ManoJugador();
    manoJugador.nombre = 'J'+numeroJugador.toString();

    if(numeroCarta ==1){
      cartaAnterior = manoJugador.carta1;
      manoJugador.carta1 = valorCarta;
    }else{
      cartaAnterior = manoJugador.carta2;
      manoJugador.carta2 = valorCarta;
    }
    valoresCartasManoJugadores.push(manoJugador);

    // valoresCartasManoJugadores.push(manoJugador.getJsonManoJugador());

  }

  const cartasMesaHTML = document.getElementById('cartas-mesa');

  let todosLosJugadoresConCartasSeleccionadas = jugadoresConCartasSeleccionadas();

  if(todosLosJugadoresConCartasSeleccionadas == true){
    // setearCartasMesa();
    console.log('carta nueva en la mesa cartaAnterior ', cartaAnterior)
    if(cartaAnterior && cartaAnterior != 'Sin seleccionar') agregarCartaEnLaMesa(cartaAnterior);

    //deshabilitar el boton de evaluar mano ganadora
    const botonEvaluarManoGanadora = document.getElementById("boton-evaluar-mano");
    botonEvaluarManoGanadora.hidden = true;
  }
  cartasMesaHTML.hidden = !todosLosJugadoresConCartasSeleccionadas;  // Si todos los jugadores tienen dos cartas devuelve false y muestra las cartas de la mesa 
  
  const botonObtenerManosJugadores = document.getElementById("boton-obtener-mano");
  botonObtenerManosJugadores.hidden = !todosLosJugadoresConCartasSeleccionadas;
};

function jugadoresConCartasSeleccionadas(){ // Si todos los jugadores tienen dos cartas devuelve false y muestra las caratas de la mesa 
  let jugadoresConCartas = valoresCartasManoJugadores.filter(mano => mano.carta1 != 'Sin seleccionar' && mano.carta1 != undefined && mano.carta2 != 'Sin seleccionar' && mano.carta2 != undefined);

  if(jugadoresConCartas.length == cantJugadores && valoresCartasManoJugadores.length != 0){
    return true;
  }else{
    return false;
  }
};

function setearCantidadJugadores(cantidadJugadores) {

  //esto lo uso para definir la cantidad de columnas segun la cantidad de jugadores
  let colTitulo;
  switch (cantidadJugadores) {
    case '1':
      colTitulo = 12;
      break;
    case '2':
      colTitulo = 6;
      break;
    case '3':
      colTitulo = 4;
      break;
    case '4':
      colTitulo = 3;
      break;
    default:
      colTitulo = 3;
      break;
  }

  cantJugadores = parseInt(cantidadJugadores);

  const container = document.getElementById('seleccion-cartas');
  container.innerHTML = ''; //vacio el contenedor, para volverlo a cargar

  // Generar las cartas con un bucle
  for (let j = 1; j <= cantidadJugadores; j++) {

    // Título
    const titulo = document.createElement('div');
    titulo.className = `col-${colTitulo} p-0 text-center`;
    titulo.innerHTML = `<h5>Cartas Jugador ${j}:</h5>`;
    container.appendChild(titulo);

    // Título con el resultado de la mano
    const tituloResultado = document.createElement('span');
    tituloResultado.className = `p-0 text-center`;
    tituloResultado.textContent = ``;
    tituloResultado.id=`manoJugador${j}`
    titulo.appendChild(tituloResultado);

    // Crear la fila segundaria
    const rowSegundaria = document.createElement('div');
    rowSegundaria.className = 'row p-1 m-1 border border-1 rounded';
    titulo.appendChild(rowSegundaria);

    for(let i = 1; i <= 2; i++){
      
      const col = document.createElement('div');
      col.className = 'col-6 input-group';

      const inputGroup = document.createElement('div');
      inputGroup.className = 'input-group-prepend ml-1';

      const span = document.createElement('span');
      span.className = 'input-group-text';
      // span.id = `basic-addon${i}`;
      span.textContent = `Carta ${i}`;

      inputGroup.appendChild(span);
      col.appendChild(inputGroup);

      const select = document.createElement('select');
      select.id = `carta${i}-jugador${j}`;

      //cargo las cartas a los select
      for (let index = 0; index < listadoCartasOriginalObject?.length; index++) {
        let carta = listadoCartasOriginalObject[index];
        select.add(new Option(carta.text, carta.value, carta.selected)) 
      }

      select.className = 'form-control selectpicker validation-field';
      select.setAttribute('onchange', `onChangeCarta(${i}, ${j}, this.value)`);

      col.appendChild(select);
      rowSegundaria.appendChild(col);
    }
  }

  container.hidden  = false;

  // 
  let botonCartasMesaRandomTodos = document.getElementById('boton-cartas-random-todos');
  botonCartasMesaRandomTodos.hidden = false;
};

function setearCartasMesa(){
  
  cargarListadosCartas("carta1-mesa", listadoCartasOriginalObject);
  cargarListadosCartas("carta2-mesa", listadoCartasOriginalObject);
  cargarListadosCartas("carta3-mesa", listadoCartasOriginalObject);
  cargarListadosCartas("carta4-mesa", listadoCartasOriginalObject);
  cargarListadosCartas("carta5-mesa", listadoCartasOriginalObject);
};

function agregarCartaEnLaMesa(carta){
  
  agregarCartaListadosCartas("carta1-mesa", carta);
  agregarCartaListadosCartas("carta2-mesa", carta);
  agregarCartaListadosCartas("carta3-mesa", carta);
  agregarCartaListadosCartas("carta4-mesa", carta);
  agregarCartaListadosCartas("carta5-mesa", carta);
};


function agregarCartaListadosCartas(idElementHTML, valorCarta){
  let carta = listadoCartasOriginalObject.filter(item => valorCarta !='Sin seleccionar' && item.value == valorCarta)[0];
  console.log('agregarCartaListadosCartas - carta nueva en la mesa ', carta);

  let optionsSelectCartas = document.getElementById(idElementHTML);
  optionsSelectCartas.add(new Option(carta.text, carta.value, carta.selected)) 
};

function onChangeCartaMesa(index, valorCarta){
  let carta = listadoCartasOriginalObject.filter(item => item.value === valorCarta)[0];
  listadoCartasMesa[index]= carta;
};

function setearCartasMesaRandom(){

  listadoCartasMesa = [];

  let listadoCartasCarta1Mesa = listadoCartasOriginalObject.filter(item => item.value !='Sin seleccionar');

  for (let index = 0; index < valoresCartasManoJugadores.length; index++) {
    let manoCarta = valoresCartasManoJugadores[index];
    // Encontrar el índice del valor
    let indiceCarta1 = listadoCartasCarta1Mesa.map(carta => carta.value).indexOf(manoCarta.carta1);

    if (indiceCarta1 !== -1) {
      // Eliminar el elemento en ese índice
      listadoCartasCarta1Mesa.splice(indiceCarta1, 1);
    }

    let indiceCarta2 = listadoCartasCarta1Mesa.map(carta => carta.value).indexOf(manoCarta.carta2);

    if (indiceCarta2 !== -1) {
      // Eliminar el elemento en ese índice
      listadoCartasCarta1Mesa.splice(indiceCarta2, 1);
    }
    // listadoCartasCarta1Mesa = listadoCartasOriginalObject.filter(item => item.value !== manoCarta.carta1);
    // listadoCartasCarta1Mesa = listadoCartasCarta1Mesa.filter(item => item.value !== manoCarta.carta2);

  }

  console.log('listadoCartasCarta1Mesa ', listadoCartasCarta1Mesa);

  cargarListadosCartas("carta1-mesa", listadoCartasCarta1Mesa);
  valorRandomCarta1Mesa = getValorRandomInListado(listadoCartasCarta1Mesa);
  selectDefaultOption(valorRandomCarta1Mesa.value,"carta1-mesa");

  listadoCartasMesa.push(valorRandomCarta1Mesa);

  let listadoCartasCarta2Mesa = listadoCartasCarta1Mesa.filter(item => item.value !== valorRandomCarta1Mesa.value);

  console.log('listadoCartasCarta2Mesa ', listadoCartasCarta2Mesa);


  cargarListadosCartas("carta2-mesa", listadoCartasCarta2Mesa);
  valorRandomCarta2Mesa = getValorRandomInListado(listadoCartasCarta2Mesa);
  selectDefaultOption(valorRandomCarta2Mesa.value,"carta2-mesa");

  listadoCartasMesa.push(valorRandomCarta2Mesa);


  let listadoCartasCarta3Mesa = listadoCartasCarta2Mesa.filter(item => item.value !== valorRandomCarta2Mesa.value);

  console.log('listadoCartasCarta3Mesa ', listadoCartasCarta3Mesa);

  cargarListadosCartas("carta3-mesa", listadoCartasCarta3Mesa);
  valorRandomCarta3Mesa = getValorRandomInListado(listadoCartasCarta3Mesa);
  selectDefaultOption(valorRandomCarta3Mesa.value,"carta3-mesa");

  listadoCartasMesa.push(valorRandomCarta3Mesa);


  let listadoCartasCarta4Mesa = listadoCartasCarta3Mesa.filter(item => item.value !== valorRandomCarta3Mesa.value);
  console.log('listadoCartasCarta4Mesa ', listadoCartasCarta4Mesa);

  cargarListadosCartas("carta4-mesa", listadoCartasCarta4Mesa);
  valorRandomCarta4Mesa = getValorRandomInListado(listadoCartasCarta4Mesa);
  selectDefaultOption(valorRandomCarta4Mesa.value,"carta4-mesa");
  
  listadoCartasMesa.push(valorRandomCarta4Mesa);

  let listadoCartasCarta5Mesa = listadoCartasCarta4Mesa.filter(item => item.value !== valorRandomCarta4Mesa.value);

  console.log('listadoCartasCarta5Mesa ', listadoCartasCarta5Mesa);

  cargarListadosCartas("carta5-mesa", listadoCartasCarta5Mesa);
  valorRandomCarta5Mesa = getValorRandomInListado(listadoCartasCarta5Mesa);
  selectDefaultOption(valorRandomCarta5Mesa.value,"carta5-mesa");

  listadoCartasMesa.push(valorRandomCarta5Mesa);

  

  const botonObtenerManosJugadores = document.getElementById("boton-obtener-mano");
  botonObtenerManosJugadores.hidden = false;
};

function getValorRandomInListado(listadoCartas){
  return listadoCartas[Math.floor(Math.random() * listadoCartas.length)];
};

function cargarListadosCartas(idElementHTML, listadoCartas){
  let optionsSelectCartas = document.getElementById(idElementHTML);

  optionsSelectCartas.innerHTML = ''; //vacio todas las opciones y las vuelvo a cargar

  for (let index = 0; index < listadoCartas.length; index++) {
    let carta = listadoCartas[index];
    optionsSelectCartas.add(new Option(carta.text, carta.value, carta.selected)) 
  }
};

//se utiliza en los select para poner una opcion por default cuando se recarga el html 
function selectDefaultOption(valueDefault, idElementHTML){
  // console.log('selectDefaultOption valueDefault ', valueDefault)
  let mySelect = document.getElementById(idElementHTML);
  for(var i, j = 0; i = mySelect.options[j]; j++) {
    if(i.value == valueDefault) {
      mySelect.options[j].selected = true;
      // i.selected == true;
      // mySelect.selectedIndex = j;
      break;
    }
  }
};