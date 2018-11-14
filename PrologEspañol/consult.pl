%--------------------------------------------------------------------------------------------------
%Operations for consulting
%--------------------------------------------------------------------------------------------------
%Verify if a class exists
verificarClase(_,[],'no lo se').
verificarClase(Clase,[class(not(Clase),_,_,_,_)|_],no).
verificarClase(Clase,[class(Clase,_,_,_,_)|_],yes).
verificarClase(Clase,[_|T],Respuesta):-
	verificarClase(Clase,T,Respuesta).

%Verify if an object exists
verificarObjeto(_,[],'no lo se').
verificarObjeto(Objeto,[class(_,_,_,_,O)|_],no):-
	verificarPertenenciaALista([id=>not(Objeto),_,_],O).
verificarObjeto(Objeto,[class(_,_,_,_,O)|_],yes):-
	verificarPertenenciaALista([id=>Objeto,_,_],O).
verificarObjeto(Objeto,[_|T],Respuesta):-
	verificarObjeto(Objeto,T,Respuesta).

%Consult the mother of a class
obtenerAntecesorDeClase(_,[],'no lo se').
obtenerAntecesorDeClase(Clase,[class(Clase,Madre,_,_,_)|_],Madre).
obtenerAntecesorDeClase(Clase,[_|T],Madre):-
	obtenerAntecesorDeClase(Clase,T,Madre).

%Consult the ancestors of a class
obtenerAntecesores(Clase,KB,AntecesoresDeClase):-
	verificarClase(Clase,KB,yes),
	listaAntecesores(Clase,KB,AntecesoresDeClase).
obtenerAntecesores(Clase,KB,'no lo se'):-
	verificarClase(Clase,KB,'no lo se').
listaAntecesores(top,_,[]).
listaAntecesores(Clase,KB,Antecesores):-
	obtenerAntecesorDeClase(Clase,KB,Madre),
	append([Madre],Anteantecesores,Antecesores),
	listaAntecesores(Madre,KB,Anteantecesores).

%Consult the properties of a class
obtenerPropiedadesDeClase(top,KB,Propiedades):-
	propiedadesExclusivasDeClase(top,KB,Propiedades).
obtenerPropiedadesDeClase(Clase,KB,ListaPropiedades):-
	verificarClase(Clase,KB,yes),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerPropiedadesDeAntecesores([Clase|Antecesores],KB,PEntrada),
	obtenerPreferencia(PEntrada,PSalida),
	eliminarPropiedadesRepetidas(PSalida,ListaPropiedades).
obtenerPropiedadesDeClase(Clase,KB,'no lo se'):-
	verificarClase(Clase,KB,'no lo se').
obtenerTodasLasPropiedadesDeClase(top,KB,Propiedades):-
        propiedadesExclusivasDeClase(top,KB,Propiedades).
obtenerTodasLasPropiedadesDeClase(Clase,KB,ListaPropiedades):-
        verificarClase(Clase,KB,yes),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerPropiedadesDeAntecesores([Clase|Antecesores],KB,PEntrada),
	obtenerPreferencia(PEntrada,ListaPropiedades).
propiedadesExclusivasDeClase(_,[],[]).
propiedadesExclusivasDeClase(Clase,[class(Clase,_,Propiedades,_,_)|_],Propiedades).
propiedadesExclusivasDeClase(Clase,[_|T],Propiedades):-
	propiedadesExclusivasDeClase(Clase,T,Propiedades).
obtenerPropiedadesDeAntecesores([],_,[]).
obtenerPropiedadesDeAntecesores([Antecesor|T],KB,ListaFinal):-
	obtenerPropiedadesDeAntecesores(T,KB,ListaResultante),
	propiedadesExclusivasDeClase(Antecesor,KB,Propiedades),
	append(Propiedades,['?'],NuevasPropiedades),
	append(NuevasPropiedades,ListaResultante,ListaFinal).
eliminarPropiedadesRepetidas([],[]).
eliminarPropiedadesRepetidas([P=>V|T],[P=>V|ListaResultante]):-
	eliminarAtomosPorPropiedadUnitaria(P,T,L1),
	eliminarAtomo(not(P=>V),L1,L2),
	eliminarPropiedadesRepetidas(L2,ListaResultante),!.
eliminarPropiedadesRepetidas([not(P=>V)|T],[not(P=>V)|ListaResultante]):-
	eliminarAtomosPorPropiedadNegadaUnitaria(P,T,L1),
	eliminarAtomo(P=>V,L1,L2),
	eliminarPropiedadesRepetidas(L2,ListaResultante),!.
eliminarPropiedadesRepetidas([not(H)|T],[not(H)|ListaResultante]):-
	eliminarAtomo(not(H),T,L1),
	eliminarAtomo(H,L1,L2),
	eliminarPropiedadesRepetidas(L2,ListaResultante),!.
eliminarPropiedadesRepetidas([H|T],[H|ListaResultante]):-
	eliminarAtomo(H,T,L1),
	eliminarAtomo(not(H),L1,L2),
	eliminarPropiedadesRepetidas(L2,ListaResultante),!.

%Unir Lista
concatenarLista([],L,L).
concatenarLista([H|T],L,[H|M]):-
	concatenarLista(T,L,M).

%Parte de lista
extraerLista(E,[E|_]).
extraerLista(E,[_|T]):-
	extraerLista(E,T).

%%Algoritmo de ordenamiento
ordenar(L, S):-
	permutacion(L, S),
	ordered(S).
permutacion([], []).
permutacion(L, [H|R]):-
	uno(L, H, L1),
	permutacion(L1, R).
uno([H|T], H, T).
uno([X|R], H, [X|T]):-
	uno(R, H, T).
ordered([]).
ordered([_]).
ordered([X,Y|T]):-
	X=[_,ValX],
	Y=[_,ValY],
	ValX=<ValY,
	ordered([Y|T]).

%%preordenar
preordenar([],_,[]).
preordenar(['?'|Preferencia],Entrada,PreferenciaFinal):-
	ordenar(Entrada,Salida),
	preordenar(Preferencia,[],PreferenciaSalida),
	concatenarLista(Salida,PreferenciaSalida,PreferenciaFinal).
preordenar([H|Preferencia],Entrada,PreferenciaSalida):-
	preordenar(Preferencia,[H|Entrada],PreferenciaSalida).

%%%Prefer handler
obtenerPreferencia(Propiedad,PropiedadResultante):-
	%print(Propiedad),
	extraerPreferencia(Propiedad,PropiedadA,Preferencia),
	eliminarPropiedadesRepetidas(PropiedadA,PropiedadB),
	preordenar(Preferencia,[],PreferenciaSalida),
	manejadorDePreferencias(PreferenciaSalida,PropiedadB,PropiedadResultante).
extraerPreferencia([],[],[]).
extraerPreferencia([[H,Peso]|T],ListaPropiedades,[[H,Peso]|TP]):-
	Peso\=0,
	extraerPreferencia(T,ListaPropiedades,TP).
extraerPreferencia([[H,0]|T],[H|ListaPropiedades],TP):-
	extraerPreferencia(T,ListaPropiedades,TP).
extraerPreferencia([_|T],ListaPropiedades,['?'|TP]):-
	extraerPreferencia(T,ListaPropiedades,TP).
manejadorDePreferencias([],PropiedadResultante,PropiedadResultante).

%Caso 1.1 preferencia x,y => x,y
manejadorDePreferencias([[(Preferencia=>'-')=>>(Conclusion=>'-'),_]|T],Propiedad,PropiedadResultante):-
	eliminarPropiedadesRepetidas(Propiedad,ListaPropiedadesC),
	extraerLista((Preferencia=>Valor),ListaPropiedadesC),
	concatenarLista(Propiedad,[Conclusion=>Valor],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%Caso 1.2 preferencia x,y => x,val
manejadorDePreferencias([[(Preferencia=>'-')=>>(Conclusion=>ValE),_]|T],Propiedad,PropiedadResultante):-
	extraerLista((Preferencia=>_),Propiedad),
	concatenarLista(Propiedad,[Conclusion=>ValE],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).
%caso 2 preferencia x,val=>x,valE
manejadorDePreferencias([[(Preferencia=>Valor)=>>(Conclusion=>ValE),_]|T],Propiedad,PropiedadResultante):-
	eliminarPropiedadesRepetidas(Propiedad,ListaPropiedadesC),
	extraerLista((Preferencia=>Valor),ListaPropiedadesC),
	concatenarLista(Propiedad,[Conclusion=>ValE],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 3.1 preferencia x => x , x,val=>x, x=>x,val
manejadorDePreferencias([[Preferencia=>>Conclusion,_]|T],Propiedad,PropiedadResultante):-
	extraerLista(Preferencia,Propiedad),
	concatenarLista(Propiedad,[Conclusion],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 3.2 preferencia x,y=>x
manejadorDePreferencias([[(Preferencia=>'-')=>>Conclusion,_]|T],Propiedad,PropiedadResultante):-
	extraerLista((Preferencia=>_),Propiedad),
	concatenarLista(Propiedad,[Conclusion],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 3.3 preferencia '-'=>x,val
manejadorDePreferencias([['-'=>>(Conclusion=>Valor),_]|T],Propiedad,PropiedadResultante):-
	concatenarLista(Propiedad,[Conclusion=>Valor],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 3.4 preferencia '-'=>x
manejadorDePreferencias([['-'=>>Conclusion,_]|T],Propiedad,PropiedadResultante):-
	concatenarLista(Propiedad,[Conclusion],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 4.1 antecedentes de preferencia caso 1 x,y => x,y
manejadorDePreferencias([[(Preferencia=>'-')=>>(Conclusion=>'-'),_]|T],Propiedad,PropiedadResultante):-
	extraerLista([_=>>(Preferencia=>_),_],T),
	manejadorDePreferencias(T,Propiedad,ListaPropiedadesA),
	eliminarPropiedadesRepetidas(ListaPropiedadesA,ListaPropiedadesB),
	extraerLista((Preferencia=>Valor),ListaPropiedadesB),
	concatenarLista(Propiedad,[Conclusion=>Valor],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 4.2 antecedentes de preferencia caso 2 x,val=>x,valE
manejadorDePreferencias([[(Preferencia=>Valor)=>>(Conclusion=>ValE),_]|T],Propiedad,PropiedadResultante):-
	extraerLista([_=>>(Preferencia=>Valor),_],T),
	manejadorDePreferencias(T,Propiedad,ListaPropiedadesA),
	eliminarPropiedadesRepetidas(ListaPropiedadesA,ListaPropiedadesB),
	extraerLista((Preferencia=>Valor),ListaPropiedadesB),
	concatenarLista(Propiedad,[Conclusion=>ValE],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 4.3 antecedentes de preferencia caso x => x
manejadorDePreferencias([[Preferencia=>>Conclusion,_]|T],Propiedad,PropiedadResultante):-
	extraerLista([_=>>Preferencia,_],T),
	manejadorDePreferencias(T,Propiedad,ListaPropiedadesA),
	extraerLista(Preferencia,ListaPropiedadesA),
	concatenarLista(Propiedad,[Conclusion],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 5.1 lista caso =>x,y
manejadorDePreferencias([[PrefL=>>(Conclusion=>'-'),_]|T],Propiedad,PropiedadResultante):-
	manejadorDeListasDePreferencias(PrefL,T,Propiedad,Valor),
	concatenarLista(Propiedad,[Conclusion=>Valor],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).
%caso 5.2 lista caso =>x,val
manejadorDePreferencias([[PrefL=>>(Conclusion=>Valor),_]|T],Propiedad,PropiedadResultante):-
	manejadorDeListasDePreferencias(PrefL,T,Propiedad,Valor),
	concatenarLista(Propiedad,[Conclusion=>Valor],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso 5.3 lista caso =>x
manejadorDePreferencias([[PrefL=>>Conclusion,_]|T],Propiedad,PropiedadResultante):-
	manejadorDeListasDePreferencias(PrefL,T,Propiedad),
	concatenarLista(Propiedad,[Conclusion],NP),
	manejadorDePreferencias(T,NP,PropiedadResultante).

%caso default, si no la encuentra.
manejadorDePreferencias([_|T],Propiedad,PropiedadResultante):-
	manejadorDePreferencias(T,Propiedad,PropiedadResultante).

%%manejo de lista
manejadorDeListasDePreferencias([],_,_).

%caso x
manejadorDeListasDePreferencias([Preferencia|T],ListaPreferencias,Propiedad):-
	extraerLista(Preferencia,Propiedad),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad).

%caso antecedentes x
manejadorDeListasDePreferencias([Preferencia|T],ListaPreferencias,Propiedad):-
	extraerLista([_=>>Preferencia,_],ListaPreferencias),
	manejadorDePreferencias(ListaPreferencias,Propiedad,ListaPropiedadesA),
	extraerLista(Preferencia,ListaPropiedadesA),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad).
manejadorDeListasDePreferencias([],_,_,_).

%caso x,y
manejadorDeListasDePreferencias([(Preferencia=>'-')|T],ListaPreferencias,Propiedad,Valor):-
	eliminarPropiedadesRepetidas(Propiedad,ListaPropiedadesC),
	extraerLista((Preferencia=>Valor),ListaPropiedadesC),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad,Valor),!.

%caso x,val
manejadorDeListasDePreferencias([(Preferencia=>Valor)|T],ListaPreferencias,Propiedad,Valor):-
	eliminarPropiedadesRepetidas(Propiedad,ListaPropiedadesC),
	extraerLista((Preferencia=>Valor),ListaPropiedadesC),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad,Valor).

%caso antecedentes x,y
manejadorDeListasDePreferencias([(Preferencia=>'-')|T],ListaPreferencias,Propiedad,Valor):-
	extraerLista([_=>>(Preferencia=>_),_],ListaPreferencias),
	manejadorDePreferencias(ListaPreferencias,Propiedad,ListaPropiedadesA),
	eliminarPropiedadesRepetidas(ListaPropiedadesA,ListaPropiedadesB),
	extraerLista((Preferencia=>Valor),ListaPropiedadesB),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad,Valor).

%caso antecedentes x,val
manejadorDeListasDePreferencias([(Preferencia=>Valor)|T],ListaPreferencias,Propiedad,Valor):-
	extraerLista([_=>>(Preferencia=>Valor),_],ListaPreferencias),
	manejadorDePreferencias(ListaPreferencias,Propiedad,ListaPropiedadesA),
	eliminarPropiedadesRepetidas(ListaPropiedadesA,ListaPropiedadesB),
	extraerLista((Preferencia=>Valor),ListaPropiedadesB),
	manejadorDeListasDePreferencias(T,ListaPreferencias,Propiedad,Valor).

%Verify if a class has a specific property
verificarPropiedadClase(Clase,Propiedad,KB,Respuesta):-
	obtenerPropiedadesDeClase(Clase,KB,Propiedades),
	manejadorNoMonotonico(Propiedad,Propiedades,Respuesta).
manejadorNoMonotonico(_,[], 'no lo se').
manejadorNoMonotonico(Atomo, Lista, yes):- verificarPertenenciaALista(Atomo,Lista).
manejadorNoMonotonico(not(Atomo), Lista, no):- verificarPertenenciaALista(Atomo,Lista).
manejadorNoMonotonico(Atomo, Lista, no):- verificarPertenenciaALista(not(Atomo),Lista).
manejadorNoMonotonico(_, _, 'no lo se').

%Return the value of a class property
obtenerValorDePropiedadDeClase(Clase,Propiedad,KB,Valor):-
	obtenerPropiedadesDeClase(Clase,KB,PropiedadesDeClase),
	obtenerValor(Propiedad,PropiedadesDeClase,Valor).

%Return list of values of a class property
obtenerListaPropiedadDeClase(Clase,Propiedad,KB,Values):-
        obtenerTodasLasPropiedadesDeClase(Clase,KB,PropiedadesDeClase),
	obtenerValorLista(Propiedad,PropiedadesDeClase,Values).
obtenerListaPropiedadDeClase(_,_,_,'no lo se').

obtenerValor(_,[],'no lo se').
obtenerValor(Entrada,[Entrada=>Valor|_],Valor).
obtenerValor(Entrada,[not(Entrada)|_],no).
obtenerValor(Entrada,[Entrada|_],yes).
obtenerValor(Entrada,[_|T],Valor):-
	obtenerValor(Entrada,T,Valor).
obtenerValorLista(_,[],[]).
obtenerValorLista(Entrada,[Entrada=>Valor|T],[Valor|TV]):-
	obtenerValorLista(Entrada,T,TV).
obtenerValorLista(Entrada,[_|T],Valor):-
	obtenerValorLista(Entrada,T,Valor).
obtenerCuantificador(_,[],'no lo se').
obtenerCuantificador(Preferencia,[[Preferencia,Cuantificador]|_],Cuantificador).
obtenerCuantificador(Preferencia,[_|PT],Cuantificador):-
	obtenerCuantificador(Preferencia,PT,Cuantificador).

%Shows the class of an object
obtenerHerencia(_,[],'no lo se'):-!.
obtenerHerencia(Objeto,[class(C,_,_,_,O)|_],C):-
	verificarPertenenciaALista([id=>Objeto,_,_],O).
obtenerHerencia(Objeto,[_|T],Clase):-
	obtenerHerencia(Objeto,T,Clase).

%Lista all the properties of an object
obtenerPropiedadesObjeto(Objeto,KB,ListaPropiedades):-
	verificarObjeto(Objeto,KB,yes),
	obtenerPropiedadesExclusivasDeObjeto(Objeto,KB,PropiedadesDeObjeto),
	obtenerHerencia(Objeto,KB,Clase),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerPropiedadesDeAntecesores([Clase|Antecesores],KB,PropiedadesDeClase),
	append(PropiedadesDeObjeto,['?'],PropiedadesDeObjetoA),
	append(PropiedadesDeObjetoA,PropiedadesDeClase,PEntrada),
	obtenerPreferencia(PEntrada,PSalida),
	eliminarPropiedadesRepetidas(PSalida,ListaPropiedades).
obtenerPropiedadesObjeto(_,_,'no lo se').
obtenerTodasLasPropiedadesDeObjeto(Objeto,KB,ListaPropiedades):-
	verificarObjeto(Objeto,KB,yes),
	obtenerPropiedadesExclusivasDeObjeto(Objeto,KB,PropiedadesDeObjeto),
	obtenerHerencia(Objeto,KB,Clase),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerPropiedadesDeAntecesores([Clase|Antecesores],KB,PropiedadesDeClase),
	append(PropiedadesDeObjeto,['?'],PropiedadesDeObjetoA),
	append(PropiedadesDeObjetoA,PropiedadesDeClase,PEntrada),
	obtenerPreferencia(PEntrada,ListaPropiedades).

	%eliminarPropiedadesRepetidas(PSalida,ListaPropiedades).
obtenerTodasLasPropiedadesDeObjeto(_,_,'no lo se').
obtenerPreferenciasDePropiedadesDeObjeto(Objeto,KB,ListaPreferenciasObjeto):-
	verificarObjeto(Objeto,KB,yes),
	obtenerPropiedadesExclusivasDeObjeto(Objeto,KB,PropiedadesDeObjeto),
	obtenerHerencia(Objeto,KB,Clase),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerPropiedadesDeAntecesores([Clase|Antecesores],KB,PropiedadesDeClase),
	append(PropiedadesDeObjeto,['?'],PropiedadesDeObjetoA),
	append(PropiedadesDeObjetoA,PropiedadesDeClase,PEntrada),
	extraerPreferencia(PEntrada,_,Preferencia),
	preordenar(Preferencia,[],ListaPreferenciasObjeto).
obtenerPreferenciasDePropiedadesDeObjeto(_,_,[]).
obtenerPropiedadesExclusivasDeObjeto(_,[],[]).
obtenerPropiedadesExclusivasDeObjeto(Objeto,[class(_,_,_,_,O)|_],Propiedades):-
	verificarPertenenciaALista([id=>Objeto,Propiedades,_],O).
obtenerPropiedadesExclusivasDeObjeto(Objeto,[_|T],Propiedades):-
	obtenerPropiedadesExclusivasDeObjeto(Objeto,T,Propiedades).

%Return the value of an object property
obtenerValorPropiedad(Objeto,Propiedad,KB,Valor):-
	verificarObjeto(Objeto,KB,yes),
	obtenerPropiedadesObjeto(Objeto,KB,Propiedades),
	obtenerValor(Propiedad,Propiedades,Valor).
obtenerValorPropiedad(_,_,_,'no lo se').

%Return list of values of an object property
obtenerListaDePropiedadesDeObjeto(Objeto,Propiedad,KB,Values):-
	verificarObjeto(Objeto,KB,yes),
	obtenerTodasLasPropiedadesDeObjeto(Objeto,KB,Propiedades),
	obtenerValorLista(Propiedad,Propiedades,Values).
obtenerListaDePropiedadesDeObjeto(_,_,_,[]).

%Return the weight of an object property preference
obtenerValorDePreferenciaDePropiedades(Objeto,Preferencia,KB,Valor):-
	verificarObjeto(Objeto,KB,yes),
	obtenerPreferenciasDePropiedadesDeObjeto(Objeto,KB,Preferences),
	obtenerCuantificador(Preferencia,Preferences,Valor).
obtenerValorDePreferenciaDePropiedades(_,_,_,'no lo se').

%Consult the relations of a class
obtenerRelacionesDeClase(top,KB,Relaciones):-
	obtenerRelacionesExclusivasDeClase(top,KB,Relaciones).
obtenerRelacionesDeClase(Clase,KB,Relaciones):-
	verificarClase(Clase,KB,yes),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerRelacionesDeAntecesores([Clase|Antecesores],KB,PEntrada),
	obtenerPreferencia(PEntrada,PSalida),
	eliminarPropiedadesRepetidas(PSalida,Relaciones).
obtenerRelacionesDeClase(_,_,'no lo se').
obtenerRelacionesExclusivasDeClase(_,[],[]).
obtenerRelacionesExclusivasDeClase(Clase,[class(Clase,_,_,Relaciones,_)|_],Relaciones).
obtenerRelacionesExclusivasDeClase(Clase,[_|T],Relaciones):-
	obtenerRelacionesExclusivasDeClase(Clase,T,Relaciones).
obtenerRelacionesDeAntecesores([],_,[]).
obtenerRelacionesDeAntecesores([Antecesor|T],KB,ListaFinal):-
	obtenerRelacionesDeAntecesores(T,KB,ListaResultante),
	obtenerRelacionesExclusivasDeClase(Antecesor,KB,Relaciones),
	append(Relaciones,['?'],RelacionesResultantes),
	append(RelacionesResultantes,ListaResultante,ListaFinal).

%Return the value of a class relation
obtenerValorDeRelacionDeClase(Clase,Relacion,KB,Valor):-
	verificarClase(Clase,KB,yes),
	obtenerRelacionesDeClase(Clase,KB,Relaciones),
	buscarValorDeRelacionDeClase(Relacion,Relaciones,Valor).
obtenerValorDeRelacionDeClase(_,_,_,'no lo se').
buscarValorDeRelacionDeClase(not(Relacion),Relaciones,Valor):-
	encontrarRelacionesNegadas(Relacion,Relaciones,Valor).
buscarValorDeRelacionDeClase(Relacion,Relaciones,Valor):-
	encontrarRelaciones(Relacion,Relaciones,Valor).
encontrarRelacionesNegadas(_,[],'no lo se').
encontrarRelacionesNegadas(Entrada,[not(Entrada=>Valor)|_],Valor).
encontrarRelacionesNegadas(Entrada,[_|T],Valor):-
	encontrarRelacionesNegadas(Entrada,T,Valor).
encontrarRelaciones(_,[],'no lo se').
encontrarRelaciones(Entrada,[Entrada=>Valor|_],Valor).
encontrarRelaciones(Entrada,[_|T],Valor):-
	encontrarRelaciones(Entrada,T,Valor).

%Lista all the relations of an object
obtenerRelacionesObjeto(Objeto,KB,RelacionesDeObjetoResultantes):-
	verificarObjeto(Objeto,KB,yes),
	obtenerRelacionesExclusivasDeObjeto(Objeto,KB,RelacionesDeObjeto),
	obtenerHerencia(Objeto,KB,Clase),
	listaAntecesores(Clase,KB,Antecesores),
	obtenerRelacionesDeAntecesores([Clase|Antecesores],KB,RelacionesDeClase),
	append(RelacionesDeObjeto,['?'],RelacionesDeObjetoB),
	append(RelacionesDeObjetoB,RelacionesDeClase,PEntrada),
	obtenerPreferencia(PEntrada,PSalida),
	eliminarPropiedadesRepetidas(PSalida,RelacionesDeObjetoResultantes).
obtenerRelacionesObjeto(_,_,'no lo se').
obtenerRelacionesExclusivasDeObjeto(_,[],[]).
obtenerRelacionesExclusivasDeObjeto(Objeto,[class(_,_,_,_,O)|_],Relaciones):-
	verificarPertenenciaALista([id=>Objeto,_,Relaciones],O).
obtenerRelacionesExclusivasDeObjeto(Objeto,[_|T],Relaciones):-
	obtenerRelacionesExclusivasDeObjeto(Objeto,T,Relaciones).

%Return the value of an object relation
obteneValorRelacion(Objeto,Relacion,KB,Valor):-
	verificarObjeto(Objeto,KB,yes),
	obtenerRelacionesObjeto(Objeto,KB,Relaciones),
	buscarValorDeRelacionDeClase(Relacion,Relaciones,Valor).
obteneValorRelacion(_,_,_,'no lo se').

% Return the son classes of a class
obtenerDescendientesDeClase(Clase,KB,Respuesta):-
	verificarClase(Clase,KB,yes),
	descendientesDeClase(Clase,KB,Respuesta).
obtenerDescendientesDeClase(_,_,'no lo se').
descendientesDeClase(_,[],[]).
descendientesDeClase(Clase,[class(Descendiente,Clase,_,_,_)|T],Descendientes):-
	descendientesDeClase(Clase,T,DescendientesDeLaMismaClase),
	append([Descendiente],DescendientesDeLaMismaClase,Descendientes).
descendientesDeClase(Clase,[_|T],Descendientes):-
	descendientesDeClase(Clase,T,Descendientes).

% Return the sons of a list of classes of a class
obtenerDescendientesDeListaDeClases([],_,[]).
obtenerDescendientesDeListaDeClases([Descendiente|T],KB,SubDescendientes):-
	descendientesDeClase(Descendiente,KB,Descendientes),
	obtenerDescendientesDeListaDeClases(T,KB,SubDescendientesVecinos),
	append(Descendientes,SubDescendientesVecinos,SubDescendientes).

% Return all the descendant classes of a class
obtenerClasesDescendientesDeClase(Clase,KB,DescendientesDeClase):-
	verificarClase(Clase,KB,yes),
	descendientesDeClase(Clase,KB,Descendientes),
	obtenerTodosDescendientesDeClase(Descendientes,KB,DescendientesDeClase).
obtenerClasesDescendientesDeClase(_,_,'no lo se').
obtenerTodosDescendientesDeClase([],_,[]).
obtenerTodosDescendientesDeClase(ListaClases,KB,DescendientesDeClase):-
	obtenerDescendientesDeListaDeClases(ListaClases,KB,Descendientes),
	obtenerTodosDescendientesDeClase(Descendientes,KB,ResiduoDescendientes),
	append(ListaClases,ResiduoDescendientes,DescendientesDeClase).

% Return the names of the objects listed only in a specific class
obtenerObjetosDeLaClase(_,[],'no lo se').
obtenerObjetosDeLaClase(Clase,[class(Clase,_,_,_,O)|_],Objetos):-
	obtenerNombresDeObjetos(O,Objetos).
obtenerObjetosDeLaClase(Clase,[_|T],Objetos):-
	obtenerObjetosDeLaClase(Clase,T,Objetos).
obtenerNombresDeObjetos([],[]).
obtenerNombresDeObjetos([[id=>Name,_,_]|T],Objetos):-
	obtenerNombresDeObjetos(T,Rest),
	append([Name],Rest,Objetos).

% Return all the objects of a class
obtenerObjetosDeUnaClase(Clase,KB,Objetos):-
	verificarClase(Clase,KB,yes),
	obtenerObjetosDeLaClase(Clase,KB,ObjectsInClass),
	obtenerClasesDescendientesDeClase(Clase,KB,Descendientes),
	obtenerObjetosDeClasesDescendientes(Descendientes,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objetos).
obtenerObjetosDeUnaClase(_,_,'no lo se').
obtenerObjetosDeClasesDescendientes([],_,[]).
obtenerObjetosDeClasesDescendientes([Clase|T],KB,ListaDeObjetos):-
	obtenerObjetosDeLaClase(Clase,KB,Objetos),
	obtenerObjetosDeClasesDescendientes(T,KB,Rest),
	append(Objetos,Rest,ListaDeObjetos).
