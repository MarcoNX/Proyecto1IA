%--------------------------------------------------------------------------------------------------
%Operations for adding classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------
%Add new class
add_class(NewClass,Madre,OriginalKB,NewKB) :-
	append(OriginalKB,[class(NewClass,Madre,[],[],[])],NewKB).

%Add new class property
add_class_property(Clase,NewProperty,Valor,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,NewProps,Rels,Objetos),OriginalKB,NewKB),
	append_property(Props,NewProperty,Valor,NewProps).
append_property(Props,NewProperty,yes,NewProps):-
	append(Props,[[NewProperty,0]],NewProps).
append_property(Props,NewProperty,no,NewProps):-
	append(Props,[[not(NewProperty),0]],NewProps).
append_property(Props,NewProperty,Valor,NewProps):-
	append(Props,[[NewProperty=>Valor,0]],NewProps).

%%Add new class property preference
add_class_property_preference(Clase,NewPreference,Weight,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,NewProps,Rels,Objetos),OriginalKB,NewKB),
	append_preference(Props,NewPreference,Weight,NewProps).
append_preference(Props,NewPreference,Weight,NewProps):-
	append(Props,[[NewPreference,Weight]],NewProps).

%Add new class relation
add_class_relation(Clase,NewRelation,OtherClass,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,NewRels,Objetos),OriginalKB,NewKB),
	append_relation(Rels,NewRelation,OtherClass,NewRels).
append_relation(Rels,not(NewRelation),OtherClass,NewRels):-
	append(Rels,[[not(NewRelation=>OtherClass),0]],NewRels).
append_relation(Rels,NewRelation,OtherClass,NewRels):-
	append(Rels,[[NewRelation=>OtherClass,0]],NewRels).

%%Add new class relation preference
add_class_relation_preference(Clase,NewPreference,Weight,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,NewRels,Objetos),OriginalKB,NewKB),
	append_preference(Rels,NewPreference,Weight,NewRels).

%Add new object
add_object(NewObject,Clase,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,NewKB),
	append(Objetos,[[id=>NewObject,[],[]]],NewObjects).

%Add new object property
add_object_property(Objeto,NewProperty,Valor,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,NewKB),
	verificarPertenenciaALista([id=>Objeto,Propiedades,Relaciones],Objetos),
	remplazarAtomo([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,NuevasPropiedades,Relaciones],Objetos,NewObjects),
	append_property(Propiedades,NewProperty,Valor,NuevasPropiedades).

%Add new object property preference
add_object_property_preference(Objeto,NewPreference,Weight,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,NewKB),
	verificarPertenenciaALista([id=>Objeto,Propiedades,Relaciones],Objetos),
	remplazarAtomo([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,NuevasPropiedades,Relaciones],Objetos,NewObjects),
	append_preference(Propiedades,NewPreference,Weight,NuevasPropiedades).

%Add new object relation
add_object_relation(Objeto,NewRelation,OtherObject,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,NewKB),
	verificarPertenenciaALista([id=>Objeto,Propiedades,Relaciones],Objetos),
	remplazarAtomo([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,RelacionesResultantes],Objetos,NewObjects),
	append_relation(Relaciones,NewRelation,OtherObject,RelacionesResultantes).

%Add new object relation preference
add_object_relation_preference(Objeto,NewPreference,Weight,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,NewKB),
	verificarPertenenciaALista([id=>Objeto,Propiedades,Relaciones],Objetos),
	remplazarAtomo([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,RelacionesResultantes],Objetos,NewObjects),
	append_preference(Relaciones,NewPreference,Weight,RelacionesResultantes).
