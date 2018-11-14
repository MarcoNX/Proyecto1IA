%----------------------------------------
% Lista management
%----------------------------------------

%Change all ocurrences of an element X in a list for the value Y
%remplazarAtomo(X,Y,ListaAnterior,ListaNueva).
remplazarAtomo(_,_,[],[]).
remplazarAtomo(X,Y,[X|T],[Y|N]):-
	remplazarAtomo(X,Y,T,N).
remplazarAtomo(X,Y,[H|T],[H|N]):-
	remplazarAtomo(X,Y,T,N).
	
%Delete all ocurrences of an element X in a list
%eliminarAtomo(X,ListaAnterior,ListaNueva).
eliminarAtomo(_,[],[]).
eliminarAtomo(X,[X|T],N):-
	eliminarAtomo(X,T,N).
eliminarAtomo(X,[H|T],[H|N]):-
	eliminarAtomo(X,T,N),
	X\=H.
	
%Verify if an element X is in a list
%verificarPertenenciaALista(X,Lista)
verificarPertenenciaALista(X,[X|_]).
verificarPertenenciaALista(X,[_|T]):-
	verificarPertenenciaALista(X,T).
	
%Convert in a single list a list of lists
%Example ([[a],[b,c],[],[d]],[a,b,c,d]).
unificarListas([],[]).
unificarListas([H|T],X):-
	append(H,ListasResultantes,X),
	unificarListas(T,ListasResultantes).
	
%Delete all elements with a specific property in a property-value list
%eliminarAtomosPorPropiedad(P,ListaAnterior,ListaNueva).
eliminarAtomosPorPropiedad(_,[],[]).
eliminarAtomosPorPropiedad(X,[[X=>_,_]|T],N):-
	eliminarAtomosPorPropiedad(X,T,N).
eliminarAtomosPorPropiedad(X,[H|T],[H|N]):-
	eliminarAtomosPorPropiedad(X,T,N).
	
%%Single without weights
eliminarAtomosPorPropiedadUnitaria(_,[],[]).
eliminarAtomosPorPropiedadUnitaria(X,[X=>_|T],N):-
	eliminarAtomosPorPropiedadUnitaria(X,T,N).
eliminarAtomosPorPropiedadUnitaria(X,[H|T],[H|N]):-
	eliminarAtomosPorPropiedadUnitaria(X,T,N).
	
%Delete all elements with a specific negated property in a property-value list
%eliminarAtomosPorPropiedadNegada(P,ListaAnterior,ListaNueva).
eliminarAtomosPorPropiedadNegada(_,[],[]).
eliminarAtomosPorPropiedadNegada(X,[[not(X=>_),_]|T],N):-
	eliminarAtomosPorPropiedadNegada(X,T,N).
eliminarAtomosPorPropiedadNegada(X,[H|T],[H|N]):-
	eliminarAtomosPorPropiedadNegada(X,T,N).
	
%Single version
eliminarAtomosPorPropiedadNegadaUnitaria(_,[],[]).
eliminarAtomosPorPropiedadNegadaUnitaria(X,[not(X=>_)|T],N):-
	eliminarAtomosPorPropiedadNegadaUnitaria(X,T,N).
eliminarAtomosPorPropiedadNegadaUnitaria(X,[H|T],[H|N]):-
	eliminarAtomosPorPropiedadNegadaUnitaria(X,T,N).
