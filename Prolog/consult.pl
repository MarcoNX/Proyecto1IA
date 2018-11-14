%--------------------------------------------------------------------------------------------------
%Operations for consulting
%--------------------------------------------------------------------------------------------------
%Verify if a class exists
there_is_class(_,[],unknown).
there_is_class(Class,[class(not(Class),_,_,_,_)|_],no).
there_is_class(Class,[class(Class,_,_,_,_)|_],yes).
there_is_class(Class,[_|T],Answer):-
	there_is_class(Class,T,Answer).

%Verify if an object exists
there_is_object(_,[],unknown).
there_is_object(Object,[class(_,_,_,_,O)|_],no):-
	isElement([id=>not(Object),_,_],O).
there_is_object(Object,[class(_,_,_,_,O)|_],yes):-
	isElement([id=>Object,_,_],O).
there_is_object(Object,[_|T],Answer):-
	there_is_object(Object,T,Answer).

%Consult the mother of a class
mother_of_a_class(_,[],unknown).
mother_of_a_class(Class,[class(Class,Mother,_,_,_)|_],Mother).
mother_of_a_class(Class,[_|T],Mother):-
	mother_of_a_class(Class,T,Mother).

%Consult the ancestors of a class
class_ancestors(Class,KB,ClassAncestors):-
	there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,ClassAncestors).
class_ancestors(Class,KB,unknown):-
	there_is_class(Class,KB,unknown).
list_of_ancestors(top,_,[]).
list_of_ancestors(Class,KB,Ancestors):-
	mother_of_a_class(Class,KB,Mother),
	append([Mother],GrandParents,Ancestors),
	list_of_ancestors(Mother,KB,GrandParents).

%Consult the properties of a class
class_properties(top,KB,Properties):-
	properties_only_in_the_class(top,KB,Properties).
class_properties(Class,KB,AllProperties):-
	there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_properties([Class|Ancestors],KB,Temp),
	prefer(Temp,TempPref),
	delete_repeated_properties(TempPref,AllProperties).
class_properties(Class,KB,unknown):-
	there_is_class(Class,KB,unknown).
class_all_properties(top,KB,Properties):-
        properties_only_in_the_class(top,KB,Properties).
class_all_properties(Class,KB,AllProperties):-
        there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_properties([Class|Ancestors],KB,Temp),
	prefer(Temp,AllProperties).
properties_only_in_the_class(_,[],[]).
properties_only_in_the_class(Class,[class(Class,_,Properties,_,_)|_],Properties).
properties_only_in_the_class(Class,[_|T],Properties):-
	properties_only_in_the_class(Class,T,Properties).
concat_ancestors_properties([],_,[]).
concat_ancestors_properties([Ancestor|T],KB,TFinal):-
	concat_ancestors_properties(T,KB,NewT),
	properties_only_in_the_class(Ancestor,KB,Properties),
	append(Properties,['?'],NewProperties),
	append(NewProperties,NewT,TFinal).
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

%Unir Lista
unir_lista([],L,L).
unir_lista([H|T],L,[H|M]):-
	unir_lista(T,L,M).

%Parte de lista
parte_de(E,[E|_]).
parte_de(E,[_|T]):-
	parte_de(E,T).

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
preordenar(['?'|Pref],Aux,PrefF):-
	ordenar(Aux,AuxO),
	preordenar(Pref,[],PrefO),
	unir_lista(AuxO,PrefO,PrefF).
preordenar([H|Pref],Aux,PrefO):-
	preordenar(Pref,[H|Aux],PrefO).

%%%Prefer handler
prefer(Prop,NewProp):-
	%print(Prop),
	prefer_extract(Prop,PropE,Pref),
	delete_repeated_properties(PropE,PropEE),
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

%Verify if a class has a specific property
class_has_property(Class,Property,KB,Answer):-
	class_properties(Class,KB,Properties),
	incomplete_information(Property,Properties,Answer).
incomplete_information(_,[], unknown).
incomplete_information(Atom, List, yes):- isElement(Atom,List).
incomplete_information(not(Atom), List, no):- isElement(Atom,List).
incomplete_information(Atom, List, no):- isElement(not(Atom),List).
incomplete_information(_, _, unknown).

%Return the value of a class property
class_property_value(Class,Property,KB,Value):-
	class_properties(Class,KB,ClassProperties),
	find_value(Property,ClassProperties,Value).

%Return list of values of a class property
class_property_list(Class,Property,KB,Values):-
        class_all_properties(Class,KB,ClassProperties),
	find_list_value(Property,ClassProperties,Values).
class_property_list(_,_,_,unknown).

find_value(_,[],unknown).
find_value(Attribute,[Attribute=>Value|_],Value).
find_value(Attribute,[not(Attribute)|_],no).
find_value(Attribute,[Attribute|_],yes).
find_value(Attribute,[_|T],Value):-
	find_value(Attribute,T,Value).
find_list_value(_,[],[]).
find_list_value(Attribute,[Attribute=>Value|T],[Value|TV]):-
	find_list_value(Attribute,T,TV).
find_list_value(Attribute,[_|T],Value):-
	find_list_value(Attribute,T,Value).
find_weight(_,[],unknown).
find_weight(Preference,[[Preference,Weight]|_],Weight).
find_weight(Preference,[_|PT],Weight):-
	find_weight(Preference,PT,Weight).

%Shows the class of an object
class_of_an_object(_,[],unknown):-!.
class_of_an_object(Object,[class(C,_,_,_,O)|_],C):-
	isElement([id=>Object,_,_],O).
class_of_an_object(Object,[_|T],Class):-
	class_of_an_object(Object,T,Class).

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
object_properties(_,_,unknown).
object_all_properties(Object,KB,AllProperties):-
	there_is_object(Object,KB,yes),
	properties_only_in_the_object(Object,KB,ObjectProperties),
	class_of_an_object(Object,KB,Class),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_properties([Class|Ancestors],KB,ClassProperties),
	append(ObjectProperties,['?'],ObjectProperties2),
	append(ObjectProperties2,ClassProperties,Temp),
	prefer(Temp,AllProperties).

	%delete_repeated_properties(TempPref,AllProperties).
object_all_properties(_,_,unknown).
object_property_preferences(Object,KB,AllPreferences):-
	there_is_object(Object,KB,yes),
	properties_only_in_the_object(Object,KB,ObjectProperties),
	class_of_an_object(Object,KB,Class),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_properties([Class|Ancestors],KB,ClassProperties),
	append(ObjectProperties,['?'],ObjectProperties2),
	append(ObjectProperties2,ClassProperties,Temp),
	prefer_extract(Temp,_,Pref),
	preordenar(Pref,[],AllPreferences).
object_property_preferences(_,_,[]).
properties_only_in_the_object(_,[],[]).
properties_only_in_the_object(Object,[class(_,_,_,_,O)|_],Properties):-
	isElement([id=>Object,Properties,_],O).
properties_only_in_the_object(Object,[_|T],Properties):-
	properties_only_in_the_object(Object,T,Properties).

%Return the value of an object property
object_property_value(Object,Property,KB,Value):-
	there_is_object(Object,KB,yes),
	object_properties(Object,KB,Properties),
	find_value(Property,Properties,Value).
object_property_value(_,_,_,unknown).

%Return list of values of an object property
object_property_list(Object,Property,KB,Values):-
	there_is_object(Object,KB,yes),
	object_all_properties(Object,KB,Properties),
	find_list_value(Property,Properties,Values).
object_property_list(_,_,_,[]).

%Return the weight of an object property preference
object_property_preference_value(Object,Preference,KB,Value):-
	there_is_object(Object,KB,yes),
	object_property_preferences(Object,KB,Preferences),
	find_weight(Preference,Preferences,Value).
object_property_preference_value(_,_,_,unknown).

%Consult the relations of a class
class_relations(top,KB,Relations):-
	relations_only_in_the_class(top,KB,Relations).
class_relations(Class,KB,Relations):-
	there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_relations([Class|Ancestors],KB,Temp),
	prefer(Temp,TempPref),
	delete_repeated_properties(TempPref,Relations).
class_relations(_,_,unknown).
relations_only_in_the_class(_,[],[]).
relations_only_in_the_class(Class,[class(Class,_,_,Relations,_)|_],Relations).
relations_only_in_the_class(Class,[_|T],Relations):-
	relations_only_in_the_class(Class,T,Relations).
concat_ancestors_relations([],_,[]).
concat_ancestors_relations([Ancestor|T],KB,TFinal):-
	concat_ancestors_relations(T,KB,NewT),
	relations_only_in_the_class(Ancestor,KB,Relations),
	append(Relations,['?'],NewRelations),
	append(NewRelations,NewT,TFinal).

%Return the value of a class relation
class_relation_value(Class,Relation,KB,Value):-
	there_is_class(Class,KB,yes),
	class_relations(Class,KB,Relations),
	find_value_relation(Relation,Relations,Value).
class_relation_value(_,_,_,unknown).
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
object_relations(_,_,unknown).
relations_only_in_the_object(_,[],[]).
relations_only_in_the_object(Object,[class(_,_,_,_,O)|_],Relations):-
	isElement([id=>Object,_,Relations],O).
relations_only_in_the_object(Object,[_|T],Relations):-
	relations_only_in_the_object(Object,T,Relations).

%Return the value of an object relation
object_relation_value(Object,Relation,KB,Value):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	find_value_relation(Relation,Relations,Value).
object_relation_value(_,_,_,unknown).

% Return the son classes of a class
sons_of_class(Class,KB,Answer):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Answer).
sons_of_class(_,_,unknown).
sons_of_a_class(_,[],[]).
sons_of_a_class(Class,[class(Son,Class,_,_,_)|T],Sons):-
	sons_of_a_class(Class,T,Brothers),
	append([Son],Brothers,Sons).
sons_of_a_class(Class,[_|T],Sons):-
	sons_of_a_class(Class,T,Sons).

% Return the sons of a list of classes of a class
sons_of_a_list_of_classes([],_,[]).
sons_of_a_list_of_classes([Son|T],KB,Grandsons):-
	sons_of_a_class(Son,KB,Sons),
	sons_of_a_list_of_classes(T,KB,Cousins),
	append(Sons,Cousins,Grandsons).

% Return all the descendant classes of a class
descendants_of_a_class(Class,KB,Descendants):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Sons),
	all_descendants_of_a_class(Sons,KB,Descendants).
descendants_of_a_class(_,_,unknown).
all_descendants_of_a_class([],_,[]).
all_descendants_of_a_class(Classes,KB,Descendants):-
	sons_of_a_list_of_classes(Classes,KB,Sons),
	all_descendants_of_a_class(Sons,KB,RestOfDescendants),
	append(Classes,RestOfDescendants,Descendants).

% Return the names of the objects listed only in a specific class
objects_only_in_the_class(_,[],unknown).
objects_only_in_the_class(Class,[class(Class,_,_,_,O)|_],Objects):-
	extract_objects_names(O,Objects).
objects_only_in_the_class(Class,[_|T],Objects):-
	objects_only_in_the_class(Class,T,Objects).
extract_objects_names([],[]).
extract_objects_names([[id=>Name,_,_]|T],Objects):-
	extract_objects_names(T,Rest),
	append([Name],Rest,Objects).

% Return all the objects of a class
objects_of_a_class(Class,KB,Objects):-
	there_is_class(Class,KB,yes),
	objects_only_in_the_class(Class,KB,ObjectsInClass),
	descendants_of_a_class(Class,KB,Sons),
	objects_of_all_descendants_classes(Sons,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objects).
objects_of_a_class(_,_,unknown).
objects_of_all_descendants_classes([],_,[]).
objects_of_all_descendants_classes([Class|T],KB,AllObjects):-
	objects_only_in_the_class(Class,KB,Objects),
	objects_of_all_descendants_classes(T,KB,Rest),
	append(Objects,Rest,AllObjects).

checa_objeto_unknown(X):-
	X=unknown,
	write('No lo se').
checa_objeto_unknown(X):-
	write(X).
