%--------------------------------------------------------------------------------------------------
%Operations for changing classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------
change_value_object_property(Objeto,Propiedad,NewValue,KB,NewKB):-
	rm_object_property(Objeto,Propiedad,KB,TemporalKB),
	add_object_property(Objeto,Propiedad,NewValue,TemporalKB,NewKB).
change_value_object_relation(Objeto,Relacion,NewObjectRelated,KB,NewKB):-
	rm_object_relation(Objeto,Relacion,KB,TemporalKB),
	add_object_relation(Objeto,Relacion,NewObjectRelated,TemporalKB,NewKB).
change_value_class_property(Clase,Propiedad,NewValue,KB,NewKB):-
	rm_class_property(Clase,Propiedad,KB,TemporalKB),
	add_class_property(Clase,Propiedad,NewValue,TemporalKB,NewKB).
change_value_class_relation(Clase,Relacion,NewClassRelated,KB,NewKB):-
	rm_class_relation(Clase,Relacion,KB,TemporalKB),
	add_class_relation(Clase,Relacion,NewClassRelated,TemporalKB,NewKB).

%%Preferences change weights
change_weight_object_property_preference(Objeto,Preference,NewWeight,KB,NewKB):-
	rm_object_property_preference(Objeto,Preference,KB,TemporalKB),
	add_object_property_preference(Objeto,Preference,NewWeight,TemporalKB,NewKB).
change_weight_object_relation_preference(Objeto,Preference,NewWeight,KB,NewKB):-
	rm_object_relation_preference(Objeto,Preference,KB,TemporalKB),
	add_object_relation_preference(Objeto,Preference,NewWeight,TemporalKB,NewKB).
change_weight_class_property_preference(Clase,Preference,NewWeight,KB,NewKB):-
	rm_class_property_preference(Clase,Preference,KB,TemporalKB),
	add_class_property_preference(Clase,Preference,NewWeight,TemporalKB,NewKB).
change_weight_class_relation_preference(Clase,Preference,NewWeight,KB,NewKB):-
	rm_class_relation_preference(Clase,Preference,KB,TemporalKB),
	add_class_relation_preference(Clase,Preference,NewWeight,TemporalKB,NewKB).
%Change the name of an object
%
change_object_name(Objeto,NewName,OriginalKB,NewKB) :-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(Clase,Madre,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	verificarPertenenciaALista([id=>Objeto|Propiedades],Objetos),
	remplazarAtomo([id=>Objeto|Propiedades],[id=>NewName|Propiedades],Objetos,NewObjects),
	change_relations_with_object(Objeto,NewName,TemporalKB,NewKB).
change_relations_with_object(_,_,[],[]).
change_relations_with_object(Objeto,NewName,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|ListaResultante]):-
	change_relations(Objeto,NewName,O,NewO),
	change_relation(Objeto,NewName,R,NewR),
	change_relations_with_object(Objeto,NewName,T,ListaResultante).
change_relations(_,_,[],[]).
change_relations(Objeto,NewName,[[id=>N,P,R]|T],[[id=>N,P,NewR]|ListaResultante]):-
	change_relation(Objeto,NewName,R,NewR),
	change_relations(Objeto,NewName,T,ListaResultante).
change_relation(_,_,[],[]).
change_relation(OldName,NewName,[[R=>OldName,Weight]|T],[[R=>NewName,Weight]|ListaResultante]):-
	change_relation(OldName,NewName,T,ListaResultante).
change_relation(OldName,NewName,[[not(R=>OldName),Weight]|T],[[not(R=>NewName),Weight]|ListaResultante]):-
	change_relation(OldName,NewName,T,ListaResultante).
change_relation(OldName,NewName,[H|T],[H|ListaResultante]):-
	change_relation(OldName,NewName,T,ListaResultante).

%Change the name of a class
change_class_name(Clase,NewName,KB,NewKB):-
	remplazarAtomo(class(Clase,Madre,Props,Rels,Objetos),class(NewName,Madre,Props,Rels,Objetos),KB,TemporalKB),
	changeMother(Clase,NewName,TemporalKB,TemporalKB2),
	change_relations_with_object(Clase,NewName,TemporalKB2,NewKB).
