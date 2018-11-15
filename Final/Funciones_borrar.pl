
%--------------------------------------------------------------------------------------------------
%                                                    MÓDULOS PARA ELIMINAR
%--------------------------------------------------------------------------------------------------

%Pirmero necesitamos funciones que eliminen propiedades dentro de las listas de propiedades y relaciones dentro de la lista de relaciones.
%borraproprel(E,ListaEntrada,ListaSalida).
% Ejemplo (p2,[[p1=>v1,w1],[p2=>v2,w2],[p3=>v3,w3],[p2=>v4,w4],[p4=>v4,w5]],[[p1=>v1,w1],[p3=>v3,w3],[p4=>v4,w4]])
%esto
borraPropRelflecha(_,[],[]).

borraPropRelflecha(X,[[X=>_,_]|T],N):-
	borraPropRelflecha(X,T,N).

borraPropRelflecha(X,[H|T],[H|N]):-
	borraPropRelflecha(X,T,N).
% La propiedades y relaciones también pueden ser de la forma [prop,peso], sin tener operador =>.  En este caso, es necesaria otra función que borre esto.

borraPropRel(_,[],[]).

borraPropRel(X,[[X,_]|T],N):-
  borraPropRel(X,T,N).

borraPropRel(X,[H|T],[H|N]):-
  borraPropRel(X,T,N).

  %También necesitamos que se borren propiedades y relaciones negadas.
  %borraNotPropRel(P,InputList,OutputList).
  %Ejemplo (p2,[[p1=>v1,w1],[not(p2=>v2),w2],[not(p3=>v3),w3],[p2=>v4,w4],[p4=>v4,w5]],[[p1=>v1,w1],[p3=>v3,w3],[p2=>v4,w4],[p4=>v4,w5]])

  borraNotPropRelflecha(_,[],[]).

  borraNotPropRelflecha(X,[[not(X=>_),_]|T],N):-
	borraNotPropRelflecha(X,T,N).

  borraNotPropRelflecha(X,[H|T],[H|N]):-
	borraNotPropRelflecha(X,T,N).
%Y una operación que borre propiedades y relaciones negadas con forma [p,peso] sin operador flecha.
borraNotPropRel(_,[],[]).

borraNotPropRel(X,[[not(X),_]|T],N):-
	borraNotPropRel(X,T,N).

borraNotPropRel(X,[H|T],[H|N]):-
	borraNotPropRel(X,T,N).

changeMother(_,_,[],[]).
changeMother(OldMother,NewMother,[class(C,OldMother,P,R,O)|T],[class(C,NewMother,P,R,O)|N]):-
	changeMother(OldMother,NewMother,T,N).
changeMother(OldMother,NewMother,[H|T],[H|N]):-
	changeMother(OldMother,NewMother,T,N).

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



%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------ELIMINAR CLASES----------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

/*
Para eliminar una clase se tienen que seguir los siguientes pasos:
1.- Encontrar la clase u objeto a eliminar. (listo)
2.- Si es una clase, los objetos de esta clase se pasan a la clase madre. (listo)
3.- Se elimina el objeto o la clase. (listo)
4.- Si es una clase, a las clases derivadas de la clase a borrar se les cambia la clase madre para evitar errores. (listo)
5.- Eliminar las propiedades y relaciones de la KB referentes a la claso u objeto. (listo)
6.- En caso de que no exista la clase o el objeto, se escribe 'don't know' (listo)
*/

rm_class(Clase,OriginalKB,NewKB) :-
  (
  %Si existe la clase a eliminar:
  there_is_class(Clase,OriginalKB,yes),
  mother_of_a_class(Clase,OriginalKB,Madre),
  heredaInd(Clase,Madre,OriginalKB,TemporalKB),
	deleteElement(class(Clase,Madre,_,_,_),TemporalKB,TemporalKB2),
	cambiaMadre(Clase,Madre,TemporalKB2,TemporalKB3),
	borra_relaciones_referentes(Clase,TemporalKB3,NewKB);
  %Si no existe la clase a eliminar:
  there_is_class(Clase,OriginalKB,unknown),
  NewKB=unknown
).

%función para cambiar la madre de una clase.
cambiaMadre(_,_,[],[]).

cambiaMadre(ViejaMadre,NuevaMadre,[class(C,ViejaMadre,P,R,O)|T],[class(C,NuevaMadre,P,R,O)|N]):-
	cambiaMadre(ViejaMadre,NuevaMadre,T,N).

cambiaMadre(ViejaMadre,NuevaMadre,[H|T],[H|N]):-
	cambiaMadre(ViejaMadre,NuevaMadre,T,N).

%Aislar los objetos de una clase.
aislar_objetos(_,[],unknown).

aislar_objetos(Clase,[class(Clase,_,_,_,Objetos)|_],Objetos).

aislar_objetos(Clase,[_|T],Objetos):-
	aislar_objetos(Clase,T,Objetos).

%función para heredar los individuos de una clase a su clase madre.
heredaInd(ClasOrig,MadOrig,OriginalKB,NewKB):-
  aislar_objetos(ClasOrig,OriginalKB,ObjClase),
  aislar_objetos(MadOrig,OriginalKB,ObjMadre),
  append(ObjMadre,ObjClase,NueObj),
  changeElement(class(MadOrig,Madre,Props,Rels,ObjMadre),class(MadOrig,Madre,Props,Rels,NueObj),OriginalKB,NewKB).


%------------------------------------------------------ELIMINAR PROPIEDADES DE CLASES--------------------------------------------------------

%Quitar la propiedad de una clase
rm_class_property(Class,Property,OriginalKB,NewKB) :-
  properties_only_in_the_class(Class,OriginalKB,Props),
  borraPropRelflecha(Property,Props,Aux),
  borraPropRel(Property,Aux,Aux2),
  borraNotPropRelflecha(Property,Aux2,Aux3),
  borraNotPropRel(Property,Aux3,NewProps),
  changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB).

%=============Predicados para borrar preferencias:

borraPrefflecha(_,[],[]).

borraPrefflecha(X,[[_=>>X=>_,_]|T],N):-
	borraPrefflecha(X,T,N).

borraPrefflecha(X,[H|T],[H|N]):-
	borraPrefflecha(X,T,N).
% La propiedades y relaciones también pueden ser de la forma [prop,peso], sin tener operador =>.  En este caso, es necesaria otra función que borre esto.

borraPref(_,[],[]).

borraPref(X,[[_=>>X,_]|T],N):-
  borraPref(X,T,N).

borraPref(X,[H|T],[H|N]):-
  borraPref(X,T,N).

  %También necesitamos que se borren propiedades y relaciones negadas.
  %borraNotPropRel(P,InputList,OutputList).
  %Ejemplo (p2,[[p1=>v1,w1],[not(p2=>v2),w2],[not(p3=>v3),w3],[p2=>v4,w4],[p4=>v4,w5]],[[p1=>v1,w1],[p3=>v3,w3],[p2=>v4,w4],[p4=>v4,w5]])

  borraNotPrefflecha(_,[],[]).

  borraNotPrefflecha(X,[[_=>>not(X=>_),_]|T],N):-
	borraNotPrefflecha(X,T,N).

  borraNotPrefflecha(X,[H|T],[H|N]):-
	borraNotPrefflecha(X,T,N).
%Y una operación que borre propiedades y relaciones negadas con forma [p,peso] sin operador flecha.
borraNotPref(_,[],[]).

borraNotPref(X,[[_=>>not(X),_]|T],N):-
	borraNotPref(X,T,N).

borraNotPref(X,[H|T],[H|N]):-
	borraNotPref(X,T,N).

%-============Elimina preferencias en propiedades
rm_class_property_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Props,NewProps).

%------------------------------------------------------ELIMINAR RELACIONES DE CLASES--------------------------------------------------------
%Quita una relación de una clase

rm_class_relation(Class,Relacion,OriginalKB,NewKB) :-
  relations_only_in_the_class(Class,OriginalKB,Relaciones),
  borraPropRelflecha(Relacion,Relaciones,Aux),
  borraPropRel(Relacion,Aux,Aux2),
  borraNotPropRelflecha(Relacion,Aux2,Aux3),
  borraNotPropRel(Relacion,Aux3,NewRels),
	changeElement(class(Class,Mother,Props,Relaciones,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB).

%-============ELIMINAR PESOS EN LAS RELACIONES DE CLASES (NOTA: Cambiar por remoción de inferencias de relaciones)

%Quita preferencias en las relaciones de clase.

rm_class_relation_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Rels,NewRels).

%-----------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------ELIMINAR OBJETOS--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

%Quitar un objeto o individuo.
/*
Para poder hacer esto hay que considerar los siguientes pasos:
1.- Verificar que el objeto exista.
2.- Eliminar el objeto
3.- Como el objeto no afecta de manera directa a clases cuando se remueve, no es necesario renombrar clases o reacomodarlas.
4.- Sin embargo, es importante remover cualquier propiedad/relación que hage referencia al objeto (nota: sólo relaciones porque sólo estas hacen preferencia
    a objetos o clases externas).
*/

rm_object(Objeto,OriginalKB,NewKB) :-
(
  there_is_object(Objeto,OriginalKB,yes),
  class_of_an_object(Objeto,OriginalKB,Clase),
  aislar_objetos(Clase,OriginalKB,ObjClase),
	isElement([id=>Objeto|Properties],ObjClase),
	deleteElement([id=>Objeto|Properties],ObjClase,NueObjClase),
  changeElement(class(Class,Mother,Props,Rels,ObjClase),class(Class,Mother,Props,Rels,NueObjClase),OriginalKB,TemporalKB),
  borra_relaciones_referentes(Objeto,TemporalKB,NewKB);
  there_is_object(Objeto,OriginalKB,no),
  class_of_an_object(Objeto,OriginalKB,Clase),
  aislar_objetos(Clase,OriginalKB,ObjClase),
	isElement([id=>Objeto|Properties],ObjClase),
	deleteElement([id=>Objeto|Properties],ObjClase,NueObjClase),
  changeElement(class(Class,Mother,Props,Rels,ObjClase),class(Class,Mother,Props,Rels,NueObjClase),OriginalKB,TemporalKB),
  borra_relaciones_referentes(Objeto,TemporalKB,NewKB);
  there_is_object(Objeto,OriginalKB,unknown),
  NewKB=unknown
).
borra_relaciones_referentes(_,[],[]).

borra_relaciones_referentes(Objeto,[class(Clase,Madre,Propiedades,Relaciones,Objetos)|T],[class(Clase,Madre,Propiedades,NueRelaciones,NueObjetos)|NueT]):-
	borra_relaciones_clase(Objeto,Relaciones,NueRelaciones),
	borra_relaciones_obj(Objeto,Objetos,NueObjetos),
	borra_relaciones_referentes(Objeto,T,NueT).

borra_relaciones_obj(_,[],[]).

borra_relaciones_obj(Objeto,[[id=>Nombre,Propiedades,Relaciones]|T],[[id=>Nombre,Propiedades,NueRelaciones]|NueT]):-
	borra_relaciones_clase(Objeto,Relaciones,NueRelaciones),
	borra_relaciones_obj(Objeto,T,NueT).

borra_relaciones_clase(_,[],[]).

borra_relaciones_clase(Objeto,[[_=>Objeto,_]|T],NueT):-
	borra_relaciones_clase(Objeto,T,NueT).

borra_relaciones_clase(Objeto,[[not(_=>Objeto),_]|T],NueT):-
	borra_relaciones_clase(Objeto,T,NueT).

borra_relaciones_clase(Objeto,[H|T],[H|NueT]):-
	borra_relaciones_clase(Objeto,T,NueT).

%------------------------------------------------------ELIMINAR PROPIEDADES DE OBJETOS--------------------------------------------------------
%Remove an object property

rm_object_property(Objeto,Propiedad,OriginalKB,NewKB) :-
  class_of_an_object(Objeto,OriginalKB,Clase),
  changeElement(class(Clase,Madre,Props,Rels,ObjetosCla),class(Clase,Madre,Props,Rels,NueObjetos),OriginalKB,NewKB),
  isElement([id=>Objeto,PropObj,RelsObj],ObjetosCla),
  changeElement([id=>Objeto,PropObj,RelsObj],[id=>Objeto,NewProps,RelsObj],ObjetosCla,NueObjetos),
  borraPropRelflecha(Propiedad,PropObj,Aux),
  borraPropRel(Propiedad,Aux,Aux2),
  borraNotPropRelflecha(Propiedad,Aux2,Aux3),
  borraNotPropRel(Propiedad,Aux3,NewProps).

%---------------------------------------------Eliminar preferencias en las propiedades de objetos-------------------------------------------

rm_object_property_preference(Objeto,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Objeto,Properties,Relations],Objects),
	changeElement([id=>Objeto,Properties,Relations],[id=>Objeto,NewProperties,Relations],Objects,NewObjects),
	deleteElement([Preference,_],Properties,NewProperties).

%------------------------------------------------------ELIMINAR RELACIONES DE OBJETOS--------------------------------------------------------

rm_object_relation(Objeto,Relacion,OriginalKB,NewKB) :-
  class_of_an_object(Objeto,OriginalKB,Clase),
  changeElement(class(Clase,Madre,Props,Rels,ObjetosCla),class(Clase,Madre,Props,Rels,NueObjetos),OriginalKB,NewKB),
  isElement([id=>Objeto,PropObj,RelsObj],ObjetosCla),
  changeElement([id=>Objeto,PropObj,RelsObj],[id=>Objeto,PropObj,NewRels],ObjetosCla,NueObjetos),
  borraPropRelflecha(Relacion,RelsObj,Aux),
  borraPropRel(Relacion,Aux,Aux2),
  borraNotPropRelflecha(Relacion,Aux2,Aux3),
  borraNotPropRel(Relacion,Aux3,NewRels).

%-============Eliminar preferencias en las relaciones de objetos.

rm_object_relation_preference(Objeto,Preferencia,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Objeto,Properties,Relations],Objects),
	changeElement([id=>Objeto,Properties,Relations],[id=>Objeto,Properties,NewRelations],Objects,NewObjects),
	deleteElement([Preferencia,_],Relations,NewRelations).
