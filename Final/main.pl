
% Inteligencia Artificial Semestre 1 2019-1

% Ing. Jessica Sarahi Mendez Rincon
% Ing. Juan Daniel Lawrence Pedroza
% Ing. Marco Tulio Sanchez Rodriguez
% Ing. Nahet Cortez Fuerte
% Ing. Rodrigo Terpán Arenas

% Profesores: Dr. Luis A. Pineda Cortes
%             Mtro.Ivan Torres Rodriguez
%             Dr. Arturo Rodriguez Garcia

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
	listaAApodosProp(X,Y),
	checa_objeto_unknown(Y,1).

%Extension de una clase
consulta_extension_clase(Clase):-
	getEnv(KB),
	class_extension(Clase,KB,X),
	listaAApodos(X,Y),
	checa_objeto_unknown(Y,1).

%Extension de una relacion
consulta_extension_relacion(Relacion):-
		getEnv(KB),
		relation_extension(Relacion,KB,X),
		write(X),
		listaAApodosRel(X,Y),
		checa_objeto_unknown(Y,1).
%Todas las clases de un objeto
consulta_todas_clases(Objeto):-
	(
		verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		classes_of_individual(Nombre,KB,X),
		checa_objeto_unknown(X,1);
		verifica_apodo(Objeto,no),
                write('No lo se')
	).

%Todas las propiedades de un objeto
consulta_todas_propiedades_objeto(Objeto):-
	(
	        verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		properties_of_individual(Nombre,KB,X),
		checa_objeto_unknown(X,1);
                verifica_apodo(Objeto,no),
                write('No lo se')
	 ).

%Todas las propiedades de una clase
consulta_todas_propiedades_clase(Clase):-
		getEnv(KB),
		class_properties(Clase,KB,X),
		checa_objeto_unknown(X,1).



%Todas las relaciones de un objeto
consulta_todas_relaciones_objeto(Objeto):-
	(
	        verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		relations_of_individual(Nombre,KB,X),
		checa_objeto_unknown(X,1);
                verifica_apodo(Objeto,no),
                write('No lo se')
	 ).

%Todas las relaciones de un objeto
consulta_todas_relaciones_clase(Clase):-
		getEnv(KB),
		class_relations(Clase,KB,X),
		checa_objeto_unknown(X,1).

%------------------------------
%Eliminar
%-----------------------------
%Objetos

borrar_objetos_relaciones_preferencias(Objeto,RelacionPreferencia):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    rm_object_relation_preference(Nombre,RelacionPreferencia,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    rm_object_relation_preference(Nombre,RelacionPreferencia,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')

         ).



borrar_objetos_relaciones(Objeto,Relacion):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    rm_object_relation(Nombre,Relacion,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    rm_object_relation(Nombre,Relacion,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).

borrar_objetos_propiedades_preferencias(Objeto,PropiedadPreferencia):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    rm_object_property_preference(Nombre,PropiedadPreferencia,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    rm_object_property_preference(Nombre,PropiedadPreferencia,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).


borrar_objetos_propiedades(Objeto,Propiedad):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    rm_object_property(Nombre,Propiedad,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    rm_object_property(Nombre,Propiedad,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).


borrar_objetos(anonimo):-
	write('No se puede eliminar al anonimo').
borrar_objetos(Objeto):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		rm_object(Nombre,KB,KB2),
		eliminar_todos_apodos(Nombre),
		checa_objeto_unknown(KB2,2);

                verifica_apodo(Objeto,no),
                write('No lo se')
           ).



%clases
borrar_clases_relaciones_preferencias(Clase,RelacionPreferencia):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		rm_class_relation_preference(Clase,RelacionPreferencia,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

borrar_clases_relaciones(Clase,Relacion):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		rm_class_relation(Clase,Relacion,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

borrar_clases_propiedades_preferencias(Clase,PropiedadPreferencia):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		rm_class_property_preference(Clase,PropiedadPreferencia,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

borrar_clases_propiedades(Clase,Propiedad):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		rm_class_property(Clase,Propiedad,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).


borrar_clases(Clase):-
		getEnv(KB),
		rm_class(Clase,KB,KB2),
		checa_objeto_unknown(KB2,2).
%------------------------------
%Insertar
%-----------------------------

inserta_clases(Nombre,Padre):-
        getEnv(KB),
	(
	    there_is_class(Nombre,KB,yes),
	    write('Ya existe la clase');
	    there_is_class(Nombre,KB,unknown),
	    add_class(Nombre,Padre,KB,KB2),
	    updEnv(KB2)
	).

inserta_clases_propiedad(Clase,NuevaPropiedad,Valor):-
	 getEnv(KB),
	(
		there_is_class(Clase,KB,yes),
		add_class_property(Clase,NuevaPropiedad,Valor,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

inserta_clases_propiedades_preferencias(Clase,PropiedadPreferencia,Peso):-
	getEnv(KB),
	(
		there_is_class(Clase,KB,yes),
		add_class_property_preference(Clase,PropiedadPreferencia,Peso,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

inserta_clases_relaciones(Clase,NuevaRelacion,ClaseRelacionada):-
	getEnv(KB),
	(
		there_is_class(Clase,KB,yes),
                add_class_relation(Clase,NuevaRelacion,ClaseRelacionada,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

inserta_clases_relaciones_preferencias(Clase,RelacionPreferencia,Peso):-
	getEnv(KB),
	(
		there_is_class(Clase,KB,yes),
		add_class_relation_preference(Clase,RelacionPreferencia,Peso,KB,KB2),
		updEnv(KB2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).


%Objetos
inserta_objetos([Objeto|Apodos],Clase):-
	(
            existeNomAp([Objeto|Apodos],yes),
            write('Ya existe ese objeto');
            existeNomAp([Objeto|Apodos],no),
		getEnv(KB),
		(
		    %there_is_class(Clase,KB,yes),
		    %there_is_object(Objeto,KB,yes),
		    %write('Ya existe el objeto');
		    %there_is_class(Clase,KB,yes),
		    %there_is_object(Objeto,KB,no),
		    %write('Ya existe el objeto');
		    there_is_class(Clase,KB,yes),
		    %there_is_object(Objeto,KB,unknown),
		    add_object(Objeto,Clase,KB,KB2),
		    crear_apodos([Objeto],Apodos),

		    updEnv(KB2);
		    there_is_class(Clase,KB,unknown),
		    write('No lo se')
		)
	 ).


inserta_objetos_propiedades(Objeto,NuevaPropiedad,Valor):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
               (
		    there_is_object(Nombre,KB,yes),
		    add_object_property(Nombre,NuevaPropiedad,Valor,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    add_object_property(Nombre,NuevaPropiedad,Valor,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).



inserta_objetos_propiedades_preferencias(Objeto,PropiedadPreferencia,Peso):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    add_object_property_preference(Nombre,PropiedadPreferencia,Peso,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    add_object_property_preference(Nombre,PropiedadPreferencia,Peso,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).


inserta_objetos_relaciones(Objeto,NuevaRelacion,ObjetoRelacionado):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    add_object_relation(Nombre,NuevaRelacion,ObjetoRelacionado,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    add_object_relation(Nombre,NuevaRelacion,ObjetoRelacionado,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).

inserta_objetos_relaciones_preferencias(Objeto,PropiedadPreferencia,Peso):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		    there_is_object(Nombre,KB,yes),
		    add_object_relation_preference(Nombre,PropiedadPreferencia,Peso,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,no),
		    add_object_relation_preference(Nombre,PropiedadPreferencia,Peso,KB,KB2),
		    updEnv(KB2);
		    there_is_object(Nombre,KB,unknown),
		    write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).


%------------------------------
%Modificar
%-----------------------------

%Clases
modificar_clases(Clase,NuevoNombre):-
	getEnv(KB),
	(
	    there_is_class(Clase,KB,yes),
	    there_is_class(NuevoNombre,KB,yes),
	    write('Ya existe una clase con ese nombre');

	    there_is_class(Clase,KB,yes),
	    there_is_class(NuevoNombre,KB,unknown),
	    change_class_name(Clase,NuevoNombre,KB,KB2),
	    updEnv(KB2);

	    there_is_class(Clase,KB,unknown),
	    write('No conozco esa clase')
	).

modificar_clases_propiedades(Clase,Propiedad,Valor):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		change_value_class_property(Clase,Propiedad,Valor,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).


modificar_clases_propiedades_preferencias(Clase,Preferencia,Peso):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		change_weight_class_property_preference(Clase,Preferencia,Peso,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

modificar_clases_relaciones(Clase,Relacion,ClaseRelacionada):-%ya
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		change_value_class_relation(Clase,Relacion,ClaseRelacionada,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

modificar_clases_relaciones_preferencias(Clase,Preferencia,Peso):-
		getEnv(KB),
		(
		there_is_class(Clase,KB,yes),
		change_weight_class_relation_preference(Clase,Preferencia,Peso,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_class(Clase,KB,unknown),
		write('No lo se')
		).

%Objetos

modificar_objetos(Objeto,NuevoNombre):- %ya
	(
	 verifica_apodo(NuevoNombre,yes),
	 write('Ya existe ese objeto');
	verifica_apodo(NuevoNombre,no),
	(
	verifica_apodo(Objeto,yes),
        relacion_apodo(Objeto,Nombre),
	Nombre=Objeto,
	getEnv(KB),
	(
	    there_is_object(Nombre,KB,yes),
	    there_is_object(NuevoNombre,KB,yes),
	    write('Ya existe un objeto con ese nombre');

	    there_is_object(Nombre,KB,yes),
	    there_is_object(NuevoNombre,KB,unknown),
	    change_object_name(Nombre,NuevoNombre,KB,KB2),
	    cambiar_nombre(Nombre,NuevoNombre),
	    updEnv(KB2);

	    there_is_object(Nombre,KB,unknown),
	    write('No conozco ese Objeto')
	);
	verifica_apodo(Objeto,yes),
        relacion_apodo(Objeto,Nombre),
	Nombre\=Objeto,
	cambiar_apodo(Objeto,NuevoNombre);
	verifica_apodo(Objeto,no),
	write('No lo se')
	)
	).


modificar_objetos_propiedades(Objeto,Propiedad,Valor):-
	   (
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),

		getEnv(KB),
		(
		there_is_object(Nombre,KB,yes),
		change_value_object_property(Nombre,Propiedad,Valor,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_object(Nombre,KB,unknown),
		write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).

modificar_objetos_propiedades_preferencias(Objeto,Preferencia,Peso):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		there_is_object(Nombre,KB,yes),
		change_weight_object_property_preference(Nombre,Preferencia,Peso,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_object(Nombre,KB,unknown),
		write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).

modificar_objetos_relaciones(Objeto,Relacion,ClaseRelacionada):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		there_is_object(Nombre,KB,yes),
		change_value_object_relation(Nombre,Relacion,ClaseRelacionada,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_object(Nombre,KB,unknown),
		write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).

modificar_objetos_relaciones_preferencias(Objeto,Preferencia,Peso):-
	(
                verifica_apodo(Objeto,yes),
                relacion_apodo(Objeto,Nombre),
		getEnv(KB),
		(
		there_is_object(Nombre,KB,yes),
		change_weight_object_relation_preference(Nombre,Preferencia,Peso,KB,KB2),
		checa_objeto_unknown(KB2,2);
		there_is_object(Nombre,KB,unknown),
		write('No lo se')
		);
                verifica_apodo(Objeto,no),
                write('No lo se')
           ).





