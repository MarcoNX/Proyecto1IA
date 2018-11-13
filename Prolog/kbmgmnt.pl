%------------------------------------------------------------
% Main KB Services
%------------------------------------------------------------
%Class extension
class_extension(Class,KB,Objects):-
	objects_of_a_class(Class,KB,Objects).

% Property extension
property_extension(Property,KB,Result):-
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

% Relation extension
relation_extension(Relation,KB,FinalResult):-
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

%Classes of individual
classes_of_individual(Object,KB,Classes):-
	there_is_object(Object,KB,yes),
	class_of_an_object(Object,KB,X),
	class_ancestors(X,KB,Y),
	append([X],Y,Classes).
classes_of_individual(_,_,unknown).

% Properties of individual
properties_of_individual(Object,KB,Properties):-
	object_properties(Object,KB,Properties).

% Relations of individual
relations_of_individual(Object,KB,ExpandedRelations):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	expand_classes_to_objects(Relations,ExpandedRelations,KB).
relations_of_individual(_,_,unknown).
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
