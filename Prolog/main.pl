
% Inteligencia Artificial Semestre 1 2019-1

% Ing. Jessica Sarahi Mendez Rincon
% Ing. Juan Daniel Lawrence Pedroza
% Ing. Marco Tulio Sanchez Rodriguez
% Ing. Nahet Cortez Fuerte
% Ing. Rodrigo Terpán Arenas

% Profesor: Dr. Luis Pineda
% Ayudante: Mtro.Ivan
% Ayudante: Dr. Arturo

%Proposito General: Procesar información jerárquica de Individuos, clases y Propiedades a partir de una Base de datos.
%Se desarrollará de forma modular, de acuerdo a los criterios impuestos en el requerimiento del proyecto.
%Estructura de Conocimiento
%nombre,padre, propiedades,relaciones,Objetos

%--------------------------------------------------
% Variables de entorno
%--------------------------------------------------

setEnv:- %Inicializa variables de entorno
	setenv('ProyectoIA','C:/Prolog/KB/BaseConocimientosIA.txt').%Constante de ubicación del KB en el disco duro

updEnv(KB):- %Actualiza KB en memoria y guarda en disco duro
	save_kb(KB).

getEnv(KB):- %Trae KB de la memoria
	open_kb(KB).

%--------------------------------------------------
% Abrir y cerrar Archivo KB
%--------------------------------------------------

open_kb(KB):-
	getenv('ProyectoIA',KBPATH),
	open(KBPATH,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(KB):-
	getenv('ProyectoIA',KBPATH),
	open(KBPATH,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).
	checkCharAndReadRest(-1,[],_) :- !.
	checkCharAndReadRest(end_of_file,[],_) :- !. %Final de la cadena

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
	get0(InStream,NextChar),
	checkCharAndReadRest(NextChar,Chars,InStream).
	atom_to_term(ATOM, TERM) :-
		atom(ATOM),
		atom_to_chars(ATOM,STR),
		atom_to_chars('.',PTO),
		append(STR,PTO,STR_PTO),
		read_from_chars(STR_PTO,TERM).

:- op(800,xfx,'=>').
:- op(850,xfx,'=>>').

%------------------------------
%Consultas
%-----------------------------

%Extension de una propiedad

consulta_extension_propiedad(Propiedad):-
	getEnv(KB),
	property_extension(Propiedad,KB,X),
	checa_lista_vacia(X).

%Extension de una clase
consulta_extension_clase(Clase):-
	getEnv(KB),
	class_extension(Clase,KB,X),
	checa_objeto_unknown(X).

%Extension de una relacion
consulta_extension_relaciones(Relacion):-
		getEnv(KB),
		relation_extension(Relacion,KB,X),
		checa_lista_vacia(X).
%Todas las clases de un objeto
todas_clases(Objeto):-
		getEnv(KB),
		classes_of_individual(Objeto,KB,X),
		checa_objeto_unknown(X).

%Todas las propiedades de un objeto
todas_propiedades(Objeto):-
		getEnv(KB),
		properties_of_individual(Objeto,KB,X),
		checa_objeto_unknown(X).


%Todas las relaciones de un objeto
todas_relaciones(Objeto):-
		getEnv(KB),
		relations_of_individual(Objeto,KB,X),
		checa_objeto_unknown(X).

