
%---------------------------------------------OPERACIONES PARA LOS PSEUDÓNIMOS-------------------------------------
%Abrir la base de datos de pseudónimos:
open_libreria_apodos(NL):-
open('C:/Prolog/KB/LibP.txt',read,Stream),
readclauses(Stream,X),
close(Stream),
atom_to_term(X,NL).

%Guardar lbrería apodos:
save_libreria_apodos(NL):-
open('C:/Prolog/KB/LibP.txt',write,Stream),
writeq(Stream,NL),
close(Stream).

%%%%%%Crear una lista de apodos%%%%%%%%%%%
crear_apodos(Nombre,Apodos):-
  open_libreria_apodos(L),
  agrega_apodos(Nombre,Apodos,L,R),
  save_libreria_apodos(R).
agrega_apodos(Nombre,Apodos,OriginalLib,NewLib):-
  append(Nombre,Apodos,NewList),
  append(OriginalLib,[NewList],NewLib).

%%%%%Cambiar el primer elemento de una lista de apodos%%%%%%%%
cambiar_nombre(Nombre,NuevoNombre):-
  open_libreria_apodos(L),
  changeElement([Nombre|Apodos],[NuevoNombre|Apodos],L,R),
  save_libreria_apodos(R).

%%%%%Cambiar un apodo%%%%%%%%
cambiar_apodo(Apodo,NuevoApodo):-
  open_libreria_apodos(L),
  changeElement([Nombre|Apodos],[Nombre|NueApodos],L,R),
  isElement(Apodo,Apodos),
  changeElement(Apodo,NuevoApodo,Apodos,NueApodos),
  save_libreria_apodos(R).

%%%%Verifica si un apodo o nombre existe en la lista%%%%%
verifica_apodo(Apodo,R):-
    open_libreria_apodos(L),
    revisa_apodo(Apodo,L,R).

  revisa_apodo(_,[],no).
  revisa_apodo(Apodo,[[Apodo|_]|_],yes).
  revisa_apodo(Apodo,[[_|Apodos]|_],R):-
    isElement(Apodo,Apodos),
    R=yes.
  revisa_apodo(Apodo,[[_|_]|Resto],R):-
    revisa_apodo(Apodo,Resto,R).

%%%%%Revisa si alguno de los elementos de una lista pertenece a la librería.
existeNomAp([H|T],R):-
  (
  verifica_apodo(H,yes),
  R=yes,!;
  verifica_apodo(H,no),
  existeNomAp(T,R)
  ).
existeNomAp([],no).

listaAApodos([],[]).
listaAApodos([H|T],LRes):-
  regresaLista(H,Hlist),
  append([Hlist],Tlist,LRes),
  listaAApodos(T,Tlist).

listaAApodosProp([],[]).
listaAApodosProp([H:Prop|T],LRes):-
  regresaLista(H,Hlist),
  append([Hlist:Prop],Tlist,LRes),
  listaAApodosProp(T,Tlist).

listaAApodosRel([],[]).
listaAApodosRel([H:H2|T],LRes):-
  regresaLista(H,Hlist),
  listaAApodos(H2,H2list),
  append([Hlist:H2list],Tlist,LRes),
  listaAApodosRel(T,Tlist).





%%%%%Regresa la lista con sólo el nombre%%%%%
regresaLista(Nombre,Lista):-
  open_libreria_apodos(L),
  isElement([Nombre|Apodos],L),
  Lista=[Nombre|Apodos].
regresaLista([],[]).

/*
%%%%%Empuja un apodo al principio de la lista (a la posición del nombre).
apodo_bubble(Apodo):-
  open_libreria_apodos(L),
  corona_apodo(Apodo,L,R),
  save_libreria_apodos(R).

corona_apodo(Apodo,[Lista|Otros],[NewLib|Otros]):-
  isElement(Apodo,Lista),
  deleteElement(Apodo,[Lista],Aux),
  append(Apodo,Aux,NewLib).
*/
/*
%%%%%Eliminar un elemento de la lista de apodos%%%%%%%
eliminar_un_apodo(Nombre,Apodo):-
  open_libreria_apodos(L),

  save_libreria_apodos(R).
*/
%%%%%Eliminar todos los elementos de la lista de apodos referentes a un nombre%%%%%%%%
eliminar_todos_apodos(Nombre):-
    open_libreria_apodos(L),
    purga_apodos(Nombre,L,R),
    save_libreria_apodos(R).
purga_apodos(Nombre,OriginalLib,NewLib):-
  deleteElement([Nombre|_],OriginalLib,NewLib).

%%%%%Verificar un apodo y regresar el nombre%%%%%%%%
relacion_apodo(Apodo,Nombre):-
  open_libreria_apodos(L),
  regresa_nombre(Apodo,L,Nombre).

regresa_nombre(_,[],unknown).
regresa_nombre(Apodo,[[Apodo|_]|_],Apodo).
regresa_nombre(Apodo,[[Nombre|Apodos]|_],Nombre):-
  isElement(Apodo,Apodos).
regresa_nombre(Apodo,[_|T],Nombre):-
	regresa_nombre(Apodo,T,Nombre).
