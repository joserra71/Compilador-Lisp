;:PROYECTO:
;;	<Lisp>
;:CLASIFICACION:
;;	<FUN>
;:FUNCION / TITULO: CompFun. Devuelve una lista de funciones directas y derivadas involucradas en una funcion
;:SINTAXIS:
;;	<(CompFun [Ruta y nombres del archivo de funcion lsp] [Ruta de directorio de funciones])>
;:DESCRIPCION:
;;	<<
;;	CompFun. A partir de la definición de la ruta y nombre
;;	de un archivo lsp y la ruta del directorio de funciones:.
;;	Devuelve las funciones directas y derivadas involucradas
;;	en dicha funcion.
;;	>>
;:EJEMPLO:
;;	<<
;;	>
;;	>>
;:DEVUELVE:
;;	<<
;;	>coRes. Lista de funciones involucradas (con extension)
;;	<(xxx.lsp ….)>
;;	>>
;:ARGUMENTOS:
;;	<<
;;	> 1. coLsAr0. Ruta y nombre del archivo de funcion
;;		<c:\\xxx\\...\\xxx.lsp>
;;	> 2. coRutFun0. Ruta de funciones <c:\\xxx\\...\\>
;;	>>
;:DEPENDIENTES:
;;	<>
;:LOCALIZACION:
;;<<
;;	>
;;>>
;:HISTORIAL DE CORRECCIONES:
;;<<
;;	>
;;>>
;:ANOTACIONES:
;;<<
;;	>
;;>>
;:ESTADO:
;;<<
;;	> En curso
;;>>
;;;(brkstep1 '("") (list ) "001TEMP" "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(cond
;	(
;		(=  ;Ent nil)
;		(setq ;Dec 0.00015)
;	)
;	(
;		(< 0 ;Ent)
;		(setq ;Dec
;			(expt 10.0 (* -1 ;Ent))
;		)
;	)
;	(
;		(= ;Ent 0)
;		(setq ;Dec 0.15)
;	)
;)
(defun CompFun ;Devuelve funciones directas y derivadas
	(coLsAr0 coRutFun0 /
		coLsAr coRutFun
		coLsOrig coItem
		coCont coRes
	)
;	(setq coLsAr nil coRutFun nil
;		coLsOrig nil coItem nil
;		coCont nil coRes nil
;	)
	;br 
;	(brkstep1
;		'("coLsAr0" "coRutFun0") (list coLsAr0 coRutFun0)
;		"COMPFUN:0000" "0000"
;	)
	;br..
	(cond
		( ;Cond type T
			(and
				(= (type coLsAr0) 'STR)
				(= (type coRutFun0) 'STR)
			)
			(setq coLsAr coLsAr0 coRutFun coRutFun0)
			;br 
;			(brkstep1
;				'("coLsAr" "coRutFun")
;				(list coLsAr coRutFun)
;				"COMPFUN:0100" "0100"
;			)
			;br..
			(setq  coCont 1
				coLsOrig (LecFun coLsAr coRutFun 1)
			)
			(while
				(< coCont (length coLsOrig))
				(setq coLsOrig
					(lva
						(append
							coLsOrig
							(LecFun coLsAr coRutFun 1)
						) () 0
					)
					coCont (+ coCont 1)
					coItem (nth coCont coLsOrig)
				)
				(if
					coItem
					(setq coLsAr (strcat coRutFun coItem))
				)
				;br 
;				(brkstep1
;					'("coLsOrig" "coCont" "coItem" "coLsAr")
;					(list coLsOrig coCont coItem coLsAr)
;					"COMPFUN:0100" ""
;				)
				;br..
			) ;While..
			;br 
;			(brkstep1
;				'("coLsOrig") (list coLsOrig)
;				"COMPFUN:0200" "0200"
;			)
			;br..
		) ;Cond type t..
	)
	(setq coRes coLsOrig)
) ;ComFun..