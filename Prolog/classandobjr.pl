%--------------------------------------------------------------------------------------------------
%Operations for removing classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------
%Remove a class property
rm_class_property(Class,Property,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameProperty(Property,Props,Aux),
	deleteElement([not(Property),_],Aux,Aux2),
	deleteElement([Property,_],Aux2,NewProps).

%Remove a class property preference
rm_class_property_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Props,NewProps).

%Remove a class relation
rm_class_relation(Class,not(Relation),OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameNegatedProperty(Relation,Rels,NewRels).
rm_class_relation(Class,Relation,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameProperty(Relation,Rels,NewRels).

%Remove a class relation preference
rm_class_relation_preference(Class,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteElement([Preference,_],Rels,NewRels).

%Remove an object property
rm_object_property(Object,Property,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	deleteAllElementsWithSameProperty(Property,Properties,Aux),
	deleteElement([not(Property),_],Aux,Aux2),
	deleteElement([Property,_],Aux2,NewProperties).

%REmove an object property preference
rm_object_property_preference(Object,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	deleteElement([Preference,_],Properties,NewProperties).

%Remove an object relation
rm_object_relation(Object,not(Relation),OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteAllElementsWithSameNegatedProperty(Relation,Relations,NewRelations).
rm_object_relation(Object,Relation,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteAllElementsWithSameProperty(Relation,Relations,NewRelations).

%Remove an object relation preference
rm_object_relation_preference(Object,Preference,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteElement([Preference,_],Relations,NewRelations).

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
