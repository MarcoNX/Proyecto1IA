% Inteligencia Artificial Semestre 1 2019-1


% Ing. Jessica Sarahi M�ndez Rincon
% Ing. Juan Daniel Lawrence Pedroza
% Ing. Marco Tulio S�nchez Rodr�guez
% Ing. Nahet Cortez Fuerte
% Ing. Rodrigo Terp�n Arenas

% Profesor: Dr. Arturo
% Ayudante: Mtro.Iv�n



%Proposito General: Procesar informaci�n jer�rquica de Individuos, clases y Propiedades a partir de una Base de datos.
%Se desarrollar� de forma modular, de acuerdo a los criterios impuestos en el requerimiento del proyecto.
%Estructura de Conocimiento
%nombre,padre, propiedades,relaciones,Objetos


%------------------------------------------------------------------------
% Lectura y Guardado de Archivos
%------------------------------------------------------------------------

%---------- Variable de entorno ---------------------

setEnv:- %Inicializa variables de entorno
	setenv('ProyectoIA','C:/Users/Documents/GitHub/Proyecto1IA/BaseConocimientosIA.txt').%Constante de ubicaci�n del KB en el disco duro

updEnv(KB):- %Actualiza KB en memoria y guarda en disco duro
	save_kb(KB).

getEnv(KB):- %Trae KB de la memoria
	open_kb(KB).

% ------------- Abrir y cerrar Archivo KB-----------------------------

open_kb(KB):-
	getenv('ProyectoIA',KBPATH),
	open(KBPATH,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term_conversion(X,KB).

save_kb(KB):-
	getenv('ProyectoIA',KBPATH),
	open(KBPATH,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.
checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

atom_to_term_conversion(ATOM, TERM) :-
	 atom(ATOM),
	 atom_to_chars(ATOM,STR),
	 atom_to_chars('.',PTO),
	 append(STR,PTO,STR_PTO),
	 read_from_chars(STR_PTO,TERM).


% ===>Lee el caracter y conserva el resto de la cadena
	checkCharAndReadRest(-1,[],_) :- !.
	checkCharAndReadRest(end_of_file,[],_) :- !. %Final de la cadena
	checkCharAndReadRest(Char,[Char|Chars],InStream) :-
				get0(InStream,NextChar),
				checkCharAndReadRest(NextChar,Chars,InStream).

%===> Compilar una cadena de caracteres de un �tomo como un t�rmino Prolog
	atom_to_term(ATOM, TERM) :-
		atom(ATOM),
		atom_to_chars(ATOM,STR),
		atom_to_chars('.',PTO),
		append(STR,PTO,STR_PTO),
		read_from_chars(STR_PTO,TERM).

:- op(800,xfx,'=>').
:- op(15,xfx,'=>>').




%----------------------------------------
% Administration of lists
%----------------------------------------




%------------------------------------------------------FUNCIONES B�SICAS--------------------------------------------------------

 %Verify if an element X is in a list
%isElement(X,List)
%Example (n,[b,a,n,a,n,a])

isElement(X,[X|_]).
isElement(X,[_|T]):-
	isElement(X,T).

%Verifica si un objeto existe

there_is_object(_,[],unknown):-
write('don\'t know'),
nl.

there_is_object(Object,[class(_,_,_,_,O)|_],no):-
	isElement([id=>not(Object),_,_],O).

there_is_object(Object,[class(_,_,_,_,O)|_],yes):-
	isElement([id=>Object,_,_],O).

there_is_object(Object,[_|T],Answer):-
	there_is_object(Object,T,Answer).


%Change all ocurrences of an element X in a list for the value Y
%changeElement(X,Y,InputList,OutputList).
%Example (p,b,[p,a,p,a,y,a],[p,b,p,b,y,b])

changeElement(_,_,[],[]).

changeElement(X,Y,[X|T],[Y|N]):-
	changeElement(X,Y,T,N).

changeElement(X,Y,[H|T],[H|N]):-
	changeElement(X,Y,T,N).



%--------------------------------------------------------------------------------------------------
% Modulos para Consulta
%--------------------------------------------------------------------------------------------------


%------------------------------------------------------CLASES--------------------------------------------------------

%Consulta la clase madre de la clase

mother_of_a_class(_,[],unknown).
mother_of_a_class(Class,[class(Class,Mother,_,_,_)|_],Mother).
mother_of_a_class(Class,[_|T],Mother):-
	mother_of_a_class(Class,T,Mother).

%Verifica si la clase existe (De otra forma, no debería de pasar nada).

there_is_class(_,[],unknown).
there_is_class(Class,[class(not(Class),_,_,_,_)|_],no).
there_is_class(Class,[class(Class,_,_,_,_)|_],yes).
there_is_class(Class,[_|T],Answer):-
	there_is_class(Class,T,Answer).

 %Shows the class of an object

class_of_an_object(_,[],unknown):-!.



class_of_an_object(Object,[class(C,_,_,_,O)|_],C):-
isElement([id=>Object,_,_],O).

class_of_an_object(Object,[_|T],Class):-
	class_of_an_object(Object,T,Class).


%Consult the ancestors of a class

class_ancestors(Class,KB,ClassAncestors):-
	there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,ClassAncestors).

%class_ancestors(Class,KB,unknown):-	there_is_class(Class,KB,unknown).

list_of_ancestors(top,_,[]).

list_of_ancestors(Class,KB,Ancestors):-
	mother_of_a_class(Class,KB,Mother),
	append([Mother],GrandParents,Ancestors),
	list_of_ancestors(Mother,KB,GrandParents).

%Classes of individual

classes_of_individual(Object,KB,Classes):-
	(there_is_object(Object,KB,yes),
	class_of_an_object(Object,KB,X),
	class_ancestors(X,KB,Y),
	append([X],Y,Classes);
  there_is_object(Object,KB,no),
	class_of_an_object(Object,KB,X),
	class_ancestors(X,KB,Y),
	append([X],Y,Classes);
  there_is_object(Object,KB,unknown)).

%classes_of_individual(_,_,unknown).

%------------------------------------------------------PROPIEDADES--------------------------------------------------------



% Properties of individual

properties_of_individual(Object,KB,Properties):-
	object_properties(Object,KB,Properties).



%List all the properties of an object

object_properties(Object,KB,AllProperties):-
	there_is_object(Object,KB,yes),
	properties_only_in_the_object(Object,KB,ObjectProperties),
	class_of_an_object(Object,KB,Class),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_properties([Class|Ancestors],KB,ClassProperties),
	append(ObjectProperties,['?'],ObjectProperties2),
	append(ObjectProperties2,ClassProperties,Temp),
	prefer(Temp,TempPref),
	delete_repeated_properties(TempPref,AllProperties).

%object_properties(_,_,unknown).



properties_only_in_the_object(_,[],[]).

properties_only_in_the_object(Object,[class(_,_,_,_,O)|_],Properties):-
	isElement([id=>Object,Properties,_],O).

properties_only_in_the_object(Object,[_|T],Properties):-
	properties_only_in_the_object(Object,T,Properties).




concat_ancestors_properties([],_,[]).

concat_ancestors_properties([Ancestor|T],KB,TFinal):-
	concat_ancestors_properties(T,KB,NewT),
	properties_only_in_the_class(Ancestor,KB,Properties),
	append(Properties,['?'],NewProperties),
	append(NewProperties,NewT,TFinal).






properties_only_in_the_class(_,[],[]).

properties_only_in_the_class(Class,[class(Class,_,Properties,_,_)|_],Properties).

properties_only_in_the_class(Class,[_|T],Properties):-
	properties_only_in_the_class(Class,T,Properties).





%%%Prefer handler

prefer(Prop,NewProp):-
	%print(Prop),
	prefer_extract(Prop,PropE,Pref),
	delete_repeated_properties(PropE,PropEE),
	%nl,write('aqui'),nl,write(PropE),
	preordenar(Pref,[],PrefO),
	prefer_handler(PrefO,PropEE,NewProp).



prefer_extract([],[],[]).
prefer_extract([[H,Peso]|T],TProp,[[H,Peso]|TP]):-
	Peso\=0,
	prefer_extract(T,TProp,TP).
prefer_extract([[H,0]|T],[H|TProp],TP):-
	prefer_extract(T,TProp,TP).
prefer_extract([_|T],TProp,['?'|TP]):-
	prefer_extract(T,TProp,TP).

prefer_handler([],NewProp,NewProp).
%Caso 1.1 preferencia x,y => x,y
prefer_handler([[(Pref=>'-')=>>(El=>'-'),_]|T],Prop,NewProp):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%Caso 1.2 preferencia x,y => x,val
prefer_handler([[(Pref=>'-')=>>(El=>ValE),_]|T],Prop,NewProp):-
	parte_de((Pref=>_),Prop),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 2 preferencia x,val=>x,valE
prefer_handler([[(Pref=>Val)=>>(El=>ValE),_]|T],Prop,NewProp):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.1 preferencia x => x , x,val=>x, x=>x,val
prefer_handler([[Pref=>>El,_]|T],Prop,NewProp):-
	parte_de(Pref,Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.2 preferencia x,y=>x
prefer_handler([[(Pref=>'-')=>>El,_]|T],Prop,NewProp):-
	parte_de((Pref=>_),Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.3 preferencia '-'=>x,val
prefer_handler([['-'=>>(El=>Val),_]|T],Prop,NewProp):-
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.4 preferencia '-'=>x
prefer_handler([['-'=>>El,_]|T],Prop,NewProp):-
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.1 antecedentes de preferencia caso 1 x,y => x,y
prefer_handler([[(Pref=>'-')=>>(El=>'-'),_]|T],Prop,NewProp):-
	parte_de([_=>>(Pref=>_),_],T),
	prefer_handler(T,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.2 antecedentes de preferencia caso 2 x,val=>x,valE
prefer_handler([[(Pref=>Val)=>>(El=>ValE),_]|T],Prop,NewProp):-
	parte_de([_=>>(Pref=>Val),_],T),
	prefer_handler(T,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.3 antecedentes de preferencia caso x => x
prefer_handler([[Pref=>>El,_]|T],Prop,NewProp):-
	parte_de([_=>>Pref,_],T),
	prefer_handler(T,Prop,PropA),
	parte_de(Pref,PropA),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.1 lista caso =>x,y
prefer_handler([[PrefL=>>(El=>'-'),_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop,Val),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.2 lista caso =>x,val
prefer_handler([[PrefL=>>(El=>Val),_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop,Val),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.3 lista caso =>x
prefer_handler([[PrefL=>>El,_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso default, si no la encuentra.
prefer_handler([_|T],Prop,NewProp):-
	prefer_handler(T,Prop,NewProp).

%%manejo de lista
prefer_handlerL([],_,_).
%caso x
prefer_handlerL([Pref|T],LPref,Prop):-
	parte_de(Pref,Prop),
	prefer_handlerL(T,LPref,Prop).
%caso antecedentes x
prefer_handlerL([Pref|T],LPref,Prop):-
	parte_de([_=>>Pref,_],LPref),
	prefer_handler(LPref,Prop,PropA),
	parte_de(Pref,PropA),
	prefer_handlerL(T,LPref,Prop).
prefer_handlerL([],_,_,_).
%caso x,y
prefer_handlerL([(Pref=>'-')|T],LPref,Prop,Val):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	prefer_handlerL(T,LPref,Prop,Val),!.
%caso x,val
prefer_handlerL([(Pref=>Val)|T],LPref,Prop,Val):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	prefer_handlerL(T,LPref,Prop,Val).
%caso antecedentes x,y
prefer_handlerL([(Pref=>'-')|T],LPref,Prop,Val):-
	parte_de([_=>>(Pref=>_),_],LPref),
	prefer_handler(LPref,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	prefer_handlerL(T,LPref,Prop,Val).
%caso antecedentes x,val
prefer_handlerL([(Pref=>Val)|T],LPref,Prop,Val):-
	parte_de([_=>>(Pref=>Val),_],LPref),
	prefer_handler(LPref,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	prefer_handlerL(T,LPref,Prop,Val).





%preordenar
preordenar([],_,[]).
preordenar(['?'|Pref],Aux,PrefF):-
	ordenar(Aux,AuxO),
	preordenar(Pref,[],PrefO),
	unir_lista(AuxO,PrefO,PrefF).
preordenar([H|Pref],Aux,PrefO):-
	preordenar(Pref,[H|Aux],PrefO).


delete_repeated_properties([],[]).

delete_repeated_properties([P=>V|T],[P=>V|NewT]):-
	deleteAllElementsWithSamePropertySingle(P,T,L1),

	deleteElement(not(P=>V),L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([not(P=>V)|T],[not(P=>V)|NewT]):-
	deleteAllElementsWithSameNegatedPropertySingle(P,T,L1),
	deleteElement(P=>V,L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([not(H)|T],[not(H)|NewT]):-
	deleteElement(not(H),T,L1),
	deleteElement(H,L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([H|T],[H|NewT]):-
	deleteElement(H,T,L1),
	deleteElement(not(H),L1,L2),
	delete_repeated_properties(L2,NewT),!.



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


%Unir Lista
unir_lista([],L,L).
unir_lista([H|T],L,[H|M]):-
	unir_lista(T,L,M).

%Parte de lista
parte_de(E,[E|_]).
parte_de(E,[_|T]):-
	parte_de(E,T).

%%Single without weights
deleteAllElementsWithSamePropertySingle(_,[],[]).

deleteAllElementsWithSamePropertySingle(X,[X=>_|T],N):-
	deleteAllElementsWithSamePropertySingle(X,T,N).

deleteAllElementsWithSamePropertySingle(X,[H|T],[H|N]):-
	deleteAllElementsWithSamePropertySingle(X,T,N).


%Single version

deleteAllElementsWithSameNegatedPropertySingle(_,[],[]).

deleteAllElementsWithSameNegatedPropertySingle(X,[not(X=>_)|T],N):-
	deleteAllElementsWithSameNegatedPropertySingle(X,T,N).

deleteAllElementsWithSameNegatedPropertySingle(X,[H|T],[H|N]):-
	deleteAllElementsWithSameNegatedPropertySingle(X,T,N).


%Delete all ocurrences of an element X in a list
%deleteElement(X,InputList,OutputList).
%Example (a,[p,a,p,a,y,a],[p,p,y])

deleteElement(_,[],[]).

deleteElement(X,[X|T],N):-
	deleteElement(X,T,N).

deleteElement(X,[H|T],[H|N]):-
	deleteElement(X,T,N),
	X\=H.


%------------------------------------------------------CONSULTA RELACIONES--------------------------------------------------------




% Relations of individual

relations_of_individual(Object,KB,ExpandedRelations):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	expand_classes_to_objects(Relations,ExpandedRelations,KB).

%relations_of_individual(_,_,unknown).

%List all the relations of an object

object_relations(Object,KB,AllRelations):-
	there_is_object(Object,KB,yes),
	relations_only_in_the_object(Object,KB,ObjectRelations),
	class_of_an_object(Object,KB,Class),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_relations([Class|Ancestors],KB,ClassRelations),
	append(ObjectRelations,['?'],ObjectRelations2),
	append(ObjectRelations2,ClassRelations,Temp),
	prefer(Temp,TempPref),
	delete_repeated_properties(TempPref,AllRelations).

%object_relations(_,_,unknown).






relations_only_in_the_object(_,[],[]).

relations_only_in_the_object(Object,[class(_,_,_,_,O)|_],Relations):-
	isElement([id=>Object,_,Relations],O).

relations_only_in_the_object(Object,[_|T],Relations):-
	relations_only_in_the_object(Object,T,Relations).





concat_ancestors_relations([],_,[]).

concat_ancestors_relations([Ancestor|T],KB,TFinal):-
	concat_ancestors_relations(T,KB,NewT),
	relations_only_in_the_class(Ancestor,KB,Relations),
	append(Relations,['?'],NewRelations),
	append(NewRelations,NewT,TFinal).



expand_classes_to_objects([],[],_).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>Objects)|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>Objects|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>[Y])|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>[Y]|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).






relations_only_in_the_class(_,[],[]).

relations_only_in_the_class(Class,[class(Class,_,_,Relations,_)|_],Relations).

relations_only_in_the_class(Class,[_|T],Relations):-
	relations_only_in_the_class(Class,T,Relations).



% Return all the objects of a class

objects_of_a_class(Class,KB,Objects):-
	there_is_class(Class,KB,yes),
	objects_only_in_the_class(Class,KB,ObjectsInClass),
	descendants_of_a_class(Class,KB,Sons),
	objects_of_all_descendants_classes(Sons,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objects).
%Excepcion
objects_of_a_class(_,_, unknown).
%objects_of_a_class(_,_,prolog:no_encontrado).
objects_of_all_descendants_classes([],_,[]).

objects_of_all_descendants_classes([Class|T],KB,AllObjects):-
	objects_only_in_the_class(Class,KB,Objects),
	objects_of_all_descendants_classes(T,KB,Rest),
	append(Objects,Rest,AllObjects).


% Return the names of the objects listed only in a specific class




%Excepcion
objects_only_in_the_class(_,[],unknown).
%objects_only_in_the_class(_,[],prolog:no_encontrado).

objects_only_in_the_class(Class,[class(Class,_,_,_,O)|_],Objects):-
	extract_objects_names(O,Objects).

objects_only_in_the_class(Class,[_|T],Objects):-
	objects_only_in_the_class(Class,T,Objects).

extract_objects_names([],[]).

extract_objects_names([[id=>Name,_,_]|T],Objects):-
	extract_objects_names(T,Rest),
	append([Name],Rest,Objects).





% Return all the descendant classes of a class

descendants_of_a_class(Class,KB,Descendants):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Sons),
	all_descendants_of_a_class(Sons,KB,Descendants).

%descendants_of_a_class(_,_,unknown).

all_descendants_of_a_class([],_,[]).

all_descendants_of_a_class(Classes,KB,Descendants):-
	sons_of_a_list_of_classes(Classes,KB,Sons),
	all_descendants_of_a_class(Sons,KB,RestOfDescendants),
	append(Classes,RestOfDescendants,Descendants).

	% Return the sons of a list of classes of a class

sons_of_a_list_of_classes([],_,[]).

sons_of_a_list_of_classes([Son|T],KB,Grandsons):-
	sons_of_a_class(Son,KB,Sons),
	sons_of_a_list_of_classes(T,KB,Cousins),
	append(Sons,Cousins,Grandsons).




	sons_of_a_class(_,[],[]).

sons_of_a_class(Class,[class(Son,Class,_,_,_)|T],Sons):-
	sons_of_a_class(Class,T,Brothers),
	append([Son],Brothers,Sons).

sons_of_a_class(Class,[_|T],Sons):-
	sons_of_a_class(Class,T,Sons).

%------------------------------------------------------EXTENSIONES DE CLASES--------------------------------------------------------


%Class extension

class_extension(Class,KB,Objects):-
    nl,write('Clase:'),write(Class),nl,
	objects_of_a_class(Class,KB,Objects).



%------------------------------------------------------EXTENSIONES DE PROPIEDADES-------------------------------------------------

% Property extension

property_extension(Property,KB,Result):-
    nl,write('Propiedad:'),write(Property),nl,
	objects_of_a_class(top,KB,AllObjects),
	filter_objects_with_property(KB,Property,AllObjects,Objects),
	eliminate_null_property(Objects,Result).

filter_objects_with_property(_,_,[],[]).

filter_objects_with_property(KB,Property,[H|T],[H:Value|NewT]):-
	object_property_value(H,Property,KB,Value),
	filter_objects_with_property(KB,Property,T,NewT).

eliminate_null_property([],[]).

eliminate_null_property([_:unknown|T],NewT):-
	eliminate_null_property(T,NewT).

eliminate_null_property([X:Y|T],[X:Y|NewT]):-
	eliminate_null_property(T,NewT).


%Return the value of an object property

object_property_value(Object,Property,KB,Value):-
	there_is_object(Object,KB,yes),
	object_properties(Object,KB,Properties),
	find_value(Property,Properties,Value).

object_property_value(_,_,_,unknown).



find_value(_,[],unknown).

find_value(Attribute,[Attribute=>Value|_],Value).

find_value(Attribute,[not(Attribute)|_],no).

find_value(Attribute,[Attribute|_],yes).

find_value(Attribute,[_|T],Value):-
	find_value(Attribute,T,Value).


%------------------------------------------------------EXTENSIONES DE RELACIONES-------------------------------------------------
% Relation extension

relation_extension(Relation,KB,FinalResult):-
    nl,write('Relaci�n:'),write(Relation),nl,
	objects_of_a_class(top,KB,AllObjects),
	filter_objects_with_relation(KB,Relation,AllObjects,Objects),
	eliminate_null_property(Objects,Result),
	expanding_classes_into_objects(Result,FinalResult,KB).

filter_objects_with_relation(_,_,[],[]).

filter_objects_with_relation(KB,Relation,[H|T],[H:Value|NewT]):-
	object_relation_value(H,Relation,KB,Value),
	filter_objects_with_relation(KB,Relation,T,NewT).

expanding_classes_into_objects([],[],_).

expanding_classes_into_objects([X:Y|T],[X:Objects|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expanding_classes_into_objects(T,NewT,KB).

expanding_classes_into_objects([X:Y|T],[X:[Y]|NewT],KB):-
	expanding_classes_into_objects(T,NewT,KB).

%Return the value of an object relation

object_relation_value(Object,Relation,KB,Value):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	find_value_relation(Relation,Relations,Value).

object_relation_value(_,_,_,unknown).

find_value_relation(not(Relation),Relations,Value):-
	find_value_negative_relation(Relation,Relations,Value).

find_value_relation(Relation,Relations,Value):-
	find_value_positive_relation(Relation,Relations,Value).


find_value_negative_relation(_,[],unknown).

find_value_negative_relation(Attribute,[not(Attribute=>Value)|_],Value).

find_value_negative_relation(Attribute,[_|T],Value):-
	find_value_negative_relation(Attribute,T,Value).


find_value_positive_relation(_,[],unknown).

find_value_positive_relation(Attribute,[Attribute=>Value|_],Value).

find_value_positive_relation(Attribute,[_|T],Value):-
	find_value_positive_relation(Attribute,T,Value).




%--------------------------------------------------------------------------------------------------
% Modulos para Inserci�n
%--------------------------------------------------------------------------------------------------



%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------AGREGAR CLASES----------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------AGREGAR PROPIEDADES DE CLASES--------------------------------------------------------
%Add new class property

agregarPropiedad(Clase,Propiedad,Valor):-
	getEnv(KB),
	add_class_property(Clase,Propiedad,Valor,KB,NKB),
	updEnv(NKB).

add_class_property(Class,NewProperty,Value,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	append_property(Props,NewProperty,Value,NewProps).

append_property(Props,NewProperty,yes,NewProps):-
	append(Props,[[NewProperty,0]],NewProps).

append_property(Props,NewProperty,no,NewProps):-
	append(Props,[[not(NewProperty),0]],NewProps).

append_property(Props,NewProperty,Value,NewProps):-
	append(Props,[[NewProperty=>Value,0]],NewProps).


%-============AGREGAR PREFERENCIAS EN LAS PROPIEDADES DE CLASES

%%Add new class property preference
add_class_property_preference(Class,NewPreference,Weight,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	append_preference(Props,NewPreference,Weight,NewProps).

append_preference(Props,NewPreference,Weight,NewProps):-
	append(Props,[[NewPreference,Weight]],NewProps).





%------------------------------------------------------AGREGAR RELACIONES DE CLASES--------------------------------------------------------


%Add new class relation

add_class_relation(Class,NewRelation,OtherClass,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	append_relation(Rels,NewRelation,OtherClass,NewRels).

append_relation(Rels,not(NewRelation),OtherClass,NewRels):-
	append(Rels,[[not(NewRelation=>OtherClass),0]],NewRels).

append_relation(Rels,NewRelation,OtherClass,NewRels):-
	append(Rels,[[NewRelation=>OtherClass,0]],NewRels).



%-============AGREGAR PREFERENCIAS EN LAS RELACIONES DE CLASES
 %Revisar por que se repite con la add_class_property_preference
%%Add new class relation preference
add_class_relation_preference(Class,NewPreference,Weight,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	append_preference(Rels,NewPreference,Weight,NewRels).



%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------AGREGAR OBJETOS--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------
%Add new class
crearClase(Nombre,Madre):-
	getEnv(KB),
	add_class(Nombre,Madre,KB,NKB),
	updEnv(NKB).

add_class(NewClass,Mother,OriginalKB,NewKB) :-
	append(OriginalKB,[class(NewClass,Mother,[],[],[])],NewKB).

%Add new class relation
agregarRelacion(ClaseA,Relacion,ClaseB):-
	getEnv(KB),
	add_class_relation(ClaseA,Relacion,ClaseB,KB,NKB),
	updEnv(NKB).


%Add new object
crearObjeto(Nombre,Clase):-
	getEnv(KB),
	add_object(Nombre,Clase,KB,NKB),
	updEnv(NKB).

add_object(NewObject,Class,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	append(Objects,[[id=>NewObject,[],[]]],NewObjects).


%Add new object property
agregarPropiedadObjeto(Nombre,Propiedad,Valor):-
	getEnv(KB),
	add_object_property(Nombre,Propiedad,Valor,KB,NKB),
	updEnv(NKB).

add_object_property(Object,NewProperty,Value,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	append_property(Properties,NewProperty,Value,NewProperties).


%Add new object relation
agregarRelacionObjeto(ObjetoA,Relacion,ObjetoB):-
	getEnv(KB),
	add_object_relation(ObjetoA,Relacion,ObjetoB,KB,NKB),
	updEnv(NKB).

add_object_relation(Object,NewRelation,OtherObject,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	append_relation(Relations,NewRelation,OtherObject,NewRelations).


%--------------------------------------------------------------------------------------------------
% Modulos para Eliminar
%--------------------------------------------------------------------------------------------------

%Delete all elements with a specific property in a property-value list
%deleteAllElementsWithSameProperty(P,InputList,OutputList).
%Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

deleteAllElementsWithSameProperty(_,[],[]).

deleteAllElementsWithSameProperty(X,[[X=>_,_]|T],N):-
	deleteAllElementsWithSameProperty(X,T,N).

deleteAllElementsWithSameProperty(X,[H|T],[H|N]):-
	deleteAllElementsWithSameProperty(X,T,N).



%Delete all elements with a specific negated property in a property-value list
%deleteAllElementsWithSameNegatedProperty(P,InputList,OutputList).
%Example (p2,[p1=>v1,not(p2=>v2),not(p3=>v3),p2=>v4,p4=>v4],[p1=>v1,not(p3=>v3),p2=>v4,p4=>v4])

deleteAllElementsWithSameNegatedProperty(_,[],[]).

deleteAllElementsWithSameNegatedProperty(X,[[not(X=>_),_]|T],N):-
	deleteAllElementsWithSameNegatedProperty(X,T,N).

deleteAllElementsWithSameNegatedProperty(X,[H|T],[H|N]):-
	deleteAllElementsWithSameNegatedProperty(X,T,N).

%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------ELIMINAR CLASES----------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

% Remove a class

rm_class(Class,OriginalKB,NewKB) :-
	deleteElement(class(Class,Mother,_,_,_),OriginalKB,TemporalKB),
	changeMother(Class,Mother,TemporalKB,TemporalKB2),
	delete_relations_with_object(Class,TemporalKB2,NewKB).

changeMother(_,_,[],[]).

changeMother(OldMother,NewMother,[class(C,OldMother,P,R,O)|T],[class(C,NewMother,P,R,O)|N]):-
	changeMother(OldMother,NewMother,T,N).

changeMother(OldMother,NewMother,[H|T],[H|N]):-
	changeMother(OldMother,NewMother,T,N).



%------------------------------------------------------ELIMINAR PROPIEDADES DE CLASES--------------------------------------------------------

%Remove a class property

rm_class_property(Class,Property,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameProperty(Property,Props,Aux),
	deleteElement([not(Property),_],Aux,Aux2),
	deleteElement([Property,_],Aux2,NewProps).

%-============ELIMINAR PREFERENCIAS EN LAS PROPIEDADES DE CLASES

%Remove a class property preference

rm_class_property_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Props,NewProps).

%------------------------------------------------------ELIMINAR RELACIONES DE CLASES--------------------------------------------------------
%Remove a class relation

rm_class_relation(Class,Relation,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameProperty(Relation,Rels,NewRels).

%Revisar en que casos se aplica
rm_class_relation_negative(Class,not(Relation),OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameNegatedProperty(Relation,Rels,NewRels).

%-============ELIMINAR PREFERENCIAS EN LAS RELACIONES DE CLASES

%Remove a class relation preference
rm_class_relation_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Rels,NewRels).


%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------ELIMINAR OBJETOS--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

%Remove an object

rm_object(Object,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	isElement([id=>Object|Properties],Objects),
	deleteElement([id=>Object|Properties],Objects,NewObjects),
	delete_relations_with_object(Object,TemporalKB,NewKB).

delete_relations_with_object(_,[],[]).

delete_relations_with_object(Object,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,O,NewO),
	delete_relations_with_object(Object,T,NewT).

del_relations(_,[],[]).

del_relations(Object,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,T,NewT).

cancel_relation(_,[],[]).

cancel_relation(Object,[[_=>Object,_]|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[[not(_=>Object),_]|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[H|T],[H|NewT]):-
	cancel_relation(Object,T,NewT).



%------------------------------------------------------ELIMINAR PROPIEDADES DE OBJETOS--------------------------------------------------------
%Remove an object property

rm_object_property(Object,Property,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	deleteAllElementsWithSameProperty(Property,Properties,Aux),
	deleteElement([not(Property),_],Aux,Aux2),
	deleteElement([Property,_],Aux2,NewProperties).


%-============ELIMINAR PREFERENCIAS EN LAS PROPIEDADES DE OBJETOS

%Remove an object property preference
rm_object_property_preference(Object,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	deleteElement([Preference,_],Properties,NewProperties).

%------------------------------------------------------ELIMINAR RELACIONES DE OBJETOS--------------------------------------------------------

%Remove an object relation
rm_object_relation(Object,Relation,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteAllElementsWithSameProperty(Relation,Relations,NewRelations).


%Revisar en que casos cae
rm_object_relation_negative(Object,not(Relation),OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteAllElementsWithSameNegatedProperty(Relation,Relations,NewRelations).


%-============ELIMINAR PREFERENCIAS EN LAS RELACIONES DE OBJETOS

%Remove an object relation preference
rm_object_relation_preference(Object,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteElement([Preference,_],Relations,NewRelations).



%--------------------------------------------------------------------------------------------------
% Modulos para Modificar
%--------------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------MODIFICAR CLASES--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------


%Change the name of a class

change_class_name(Class,NewName,KB,NewKB):-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(NewName,Mother,Props,Rels,Objects),KB,TemporalKB),
	changeMother(Class,NewName,TemporalKB,TemporalKB2),
	change_relations_with_object(Class,NewName,TemporalKB2,NewKB).



%------------------------------------------------------MODIFICAR PROPIEDADES DE CLASES--------------------------------------------------------

change_value_class_property(Class,Property,NewValue,KB,NewKB):-
	rm_class_property(Class,Property,KB,TemporalKB),
	add_class_property(Class,Property,NewValue,TemporalKB,NewKB).



%-============MODIFICAR PREFERENCIAS EN LAS PROPIEDADES DE CLASES

change_weight_class_property_preference(Class,Preference,NewWeight,KB,NewKB):-
	rm_class_property_preference(Class,Preference,KB,TemporalKB),
	add_class_property_preference(Class,Preference,NewWeight,TemporalKB,NewKB).


%------------------------------------------------------MODIFICAR RELACIONES DE CLASES--------------------------------------------------------

change_value_class_relation(Class,Relation,NewClassRelated,KB,NewKB):-
	rm_class_relation(Class,Relation,KB,TemporalKB),
	add_class_relation(Class,Relation,NewClassRelated,TemporalKB,NewKB).


%-============MODIFICAR PREFERENCIAS EN LAS REALCIONES DE CLASES


change_weight_class_relation_preference(Class,Preference,NewWeight,KB,NewKB):-
	rm_class_relation_preference(Class,Preference,KB,TemporalKB),
	add_class_relation_preference(Class,Preference,NewWeight,TemporalKB,NewKB).


%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------MODIFICAR OBJETOS--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

%Change the name of an object

change_object_name(Object,NewName,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	isElement([id=>Object|Properties],Objects),
	changeElement([id=>Object|Properties],[id=>NewName|Properties],Objects,NewObjects),
	change_relations_with_object(Object,NewName,TemporalKB,NewKB).

change_relations_with_object(_,_,[],[]).

change_relations_with_object(Object,NewName,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	change_relations(Object,NewName,O,NewO),
	change_relation(Object,NewName,R,NewR),
	change_relations_with_object(Object,NewName,T,NewT).

 change_relations(_,_,[],[]).
change_relations(Object,NewName,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	change_relation(Object,NewName,R,NewR),
	change_relations(Object,NewName,T,NewT).


change_relation(_,_,[],[]).
change_relation(OldName,NewName,[[R=>OldName,Weight]|T],[[R=>NewName,Weight]|NewT]):-
	change_relation(OldName,NewName,T,NewT).

change_relation(OldName,NewName,[[not(R=>OldName),Weight]|T],[[not(R=>NewName),Weight]|NewT]):-
	change_relation(OldName,NewName,T,NewT).

change_relation(OldName,NewName,[H|T],[H|NewT]):-
	change_relation(OldName,NewName,T,NewT).




%------------------------------------------------------MODIFICAR PROPIEDADES DE OBJETOS--------------------------------------------------------

change_value_object_property(Object,Property,NewValue,KB,NewKB):-
	rm_object_property(Object,Property,KB,TemporalKB),
	add_object_property(Object,Property,NewValue,TemporalKB,NewKB).

%-============MODIFICAR PREFERENCIAS EN LAS PROPIEDADES DE OBJETOS

change_weight_object_property_preference(Object,Preference,NewWeight,KB,NewKB):-
	rm_object_property_preference(Object,Preference,KB,TemporalKB),
	add_object_property_preference(Object,Preference,NewWeight,TemporalKB,NewKB).



%------------------------------------------------------MODIFICAR RELACIONES DE OBJETOS--------------------------------------------------------

change_value_object_relation(Object,Relation,NewObjectRelated,KB,NewKB):-
	rm_object_relation(Object,Relation,KB,TemporalKB),
	add_object_relation(Object,Relation,NewObjectRelated,TemporalKB,NewKB).

%-============MODIFICAR PREFERENCIAS EN LAS RELACIONES DE OBJETOS


%%Preferences change weights

change_weight_object_relation_preference(Object,Preference,NewWeight,KB,NewKB):-
	rm_object_relation_preference(Object,Preference,KB,TemporalKB),
	add_object_relation_preference(Object,Preference,NewWeight,TemporalKB,NewKB).


%------------------------------------------------------------------------
% Ejecuci�n de las Consulta todas las propiedades
%------------------------------------------------------------------------

 %________________Consultas
todas_extension_clases(Clase):-
 		getEnv(KB),
 		class_extension(Clase,KB,X),
 		write(X).

todas_extension_propiedades(Propiedad):-
 		getEnv(KB),
 		property_extension(Propiedad,KB,X),
 		write(X).

todas_extension_relaciones(Relacion):-
 		getEnv(KB),
 		relation_extension(Relacion,KB,X),
 		write(X).

todas_clases(Objeto):-
		getEnv(KB),
		classes_of_individual(Objeto,KB,X),
		write(X).

todas_propiedades(Objeto):-
		getEnv(KB),
		properties_of_individual(Objeto,KB,X),
		write(X).

todas_relaciones(Objeto):-
		getEnv(KB),
		relations_of_individual(Objeto,KB,X),
		write(X).



%---------------------Insertar

%Clases

agregar_clases:-
		getEnv(KB),
		add_class(extraterrestres,animales,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(extraterrestres,KB2,X),
		write(X).

agregar_clases_propiedades:-
		getEnv(KB),
		add_class_property(extraterrestres,propiedad_vivo_extraterrestre,si,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_vivo_extraterrestre,KB2,X),
		write(X).

agregar_clases_propiedades_preferencias:-
		getEnv(KB),
		add_class_property_preference(extraterrestres,preferencia1,2,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(extraterrestres,KB2,X),
		write(X).

agregar_clases_relaciones:-
		getEnv(KB),
		add_class_relation(extraterrestres,relacion_mental,telequinesis,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).


agregar_clases_relaciones_preferencias:-
		getEnv(KB),
		add_class_relation_preference(extraterrestres,relacion_mental,1,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).



%Objetos
agregar_objetos:-
		getEnv(KB),
		add_object(spock,extraterrestres,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(extraterrestres,KB2,X),
		write(X).

agregar_objetos_propiedades:-
		getEnv(KB),
		add_object_property(spock,propiedad_orejas,picudas,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_orejas,KB2,X),
		write(X).

agregar_objetos_propiedades_preferencias:-
		getEnv(KB),
		add_object_property_preference(spock,propiedad_orejas_preferencia,1,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(extraterrestres,KB2,X),
		write(X).

agregar_objetos_relaciones:-
		getEnv(KB),
		add_object_relation(spock,relacion_paterna,sarek,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna,KB2,X),
		write(X).


agregar_objetos_relaciones_preferencias:-
		getEnv(KB),
		add_object_relation_preference(spock,relacion_paterna_preferencia,1,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna_preferencia,KB2,X),
		write(X).



%---------------------Modificar

%Clases
modificar_clases:-
		getEnv(KB),
		change_class_name(extraterrestres,vulcanos,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(vulcanos,KB2,X),
		write(X).

modificar_clases_propiedades:-
		getEnv(KB),
		change_value_class_property(vulcanos,propiedad_vivo_extraterrestre,no,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_vivo_extraterrestre,KB2,X),
		write(X).


modificar_clases_propiedades2:-
		getEnv(KB),
		change_value_class_property(vulcanos,propiedad_vivo_extraterrestre,si,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_vivo_extraterrestre,KB2,X),
		write(X).


modificar_clases_propiedades_preferencias:-
		getEnv(KB),
		change_weight_class_property_preference(vulcanos,preferencia1,1,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(preferencia1,KB2,X),
		write(X).

modificar_clases_relaciones:-
		getEnv(KB),
		change_value_class_relation(vulcanos,relacion_mental,telepatia,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).

modificar_clases_relaciones_preferencias:-
		getEnv(KB),
		change_weight_class_relation_preference(vulcanos,relacion_mental,2,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).

%Objetos

modificar_objetos:-
		getEnv(KB),
		change_object_name(spock,capitan_spok,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(vulcanos,KB2,X),
		write(X).

modificar_objetos_propiedades:-
		getEnv(KB),
		change_value_object_property(capitan_spok,propiedad_orejas,si,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_orejas,KB2,X),
		write(X).


modificar_objetos_propiedades2:-
		getEnv(KB),
		change_value_object_property(capitan_spok,propiedad_orejas,raras,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_orejas,KB2,X),
		write(X).


modificar_objetos_propiedades_preferencias:-
		getEnv(KB),
		change_weight_object_property_preference(capitan_spok,propiedad_orejas_preferencia,2,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_orejas_preferencia,KB2,X),
		write(X).

modificar_objetos_relaciones:-
		getEnv(KB),
		change_value_object_relation(capitan_spok,relacion_paterna,sarek_spok,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna,KB2,X),
		write(X).

modificar_objetos_relaciones_preferencias:-
		getEnv(KB),
		change_weight_object_relation_preference(capitan_spok,relacion_paterna_preferencia,2,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna_preferencia,KB2,X),
		write(X).



%---------------------Borrar


%Objetos


borrar_objetos_relaciones_preferencias:-
		getEnv(KB),
		rm_object_relation_preference(capitan_spok,relacion_paterna_preferencia,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna_preferencia,KB2,X),
		write(X).


borrar_objetos_relaciones :-
		getEnv(KB),
		rm_object_relation(capitan_spok,relacion_paterna,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_paterna,KB2,X),
		write(X).


borrar_objetos_propiedades_preferencias:-
		getEnv(KB),
		rm_object_property_preference(capitan_spok,preferencia1,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(preferencia1,KB2,X),
		write(X).


borrar_objetos_propiedades:-
		getEnv(KB),
		rm_object_property(capitan_spok,propiedad_orejas,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_orejas,KB2,X),
		write(X).

borrar_objetos:-
		getEnv(KB),
		rm_object(capitan_spok,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(capitan_spok,KB2,X),
		write(X).



%Clases



borrar_clases_relaciones_preferencias:-
		getEnv(KB),
		rm_class_relation_preference(vulcanos,relacion_mental,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).

borrar_clases_relaciones :-
		getEnv(KB),
		rm_class_relation(vulcanos,relacion_mental,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		relation_extension(relacion_mental,KB2,X),
		write(X).


borrar_clases_propiedades_preferencias:-
		getEnv(KB),
		rm_class_property_preference(vulcanos,propiedad_vivo_extraterrestre,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_vivo_extraterrestre,KB2,X),
		write(X).


borrar_clases_propiedades:-
		getEnv(KB),
		rm_class_property(vulcanos,propiedad_vivo_extraterrestre,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		property_extension(propiedad_vivo_extraterrestre,KB2,X),
		write(X).


borrar_clases:-
		getEnv(KB),
		rm_class(vulcanos,KB,KB2),
		updEnv(KB2),
		write('Predicado exitoso.'),nl,
		class_extension(vulcanos,KB2,X),
		write(X).
