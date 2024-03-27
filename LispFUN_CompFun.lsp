;:PROYECTO:
;;	<Lisp>
;:CLASIFICACION:
;;	<FUN>
;:FUNCION / TITULO: CompFun. Escribe en un archivo CSV Y Devuelve una lista de funciones directas y derivadas involucradas en una funcion
;:SINTAXIS:
;;	<(CompFun [Ruta y nombres del archivo de funcion lsp] [Ruta de directorio de funciones] [Modo de resultado])>
;:DESCRIPCION:
;;	<<
;;	CompFun. A partir de la definición de la ruta y nombre
;;	de un archivo lsp:. ruta del directorio de funciones Y modo de resultado
;;	Crea un archivo CSV y devuelve las funciones directas y
;;	derivadas involucradas en dicha funcion.
;;	El ID de directorio del archivo csv es "BDATOS"
;;	El ID de directorio de funciones es "FUNCIONES"
;;	El nombre del archivo csv de escritura de funciones es CompFile.csv
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
;;	> 3. coMd0. Modo de resultado:
;;	0. No genera o escribe el archivo csv
;;	1. Genera o escribe el arvhivo csv
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
;;	> El ID de directorio del archivo csv es "BDATOS"
;;	> El ID de directorio de funciones es "FUNCIONES"
;;	> El nombre del archivo csv de escritura de funciones es CompFile.csv
;;
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
	(coLsAr0 coRutFun0 coMd0 /
		coLsAr coRutFun coMd
		coLsOrig coItem
		coCont coRes
		coFile coRutPpal
	)
;	(setq coLsAr nil coRutFun nil coMd nil
;		coLsOrig nil coItem nil
;		coCont nil coRes nil
;		coFile nil coRutPpal nil coArchRt nil
;		coFilSep nil
;	)
	;br 
;	(brkstep1
;		'("coLsAr0" "coRutFun0" "coMd0") (list coLsAr0 coRutFun0 coMd0)
;		"COMPFUN:0000" "0000"
;	)
	;br..
	(cond
		( ;Cond type T
			(and
				(= (type coLsAr0) 'STR)
				(= (type coRutFun0) 'STR)
				(= (type coMd0) 'INT)
			)
			(setq coLsAr coLsAr0 coRutFun coRutFun0 coMd coMd0)
			;br 
;			(brkstep1
;				'("coLsAr" "coRutFun" "caMd")
;				(list coLsAr coRutFun coMd)
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
	(if (= coMd 1) ;ESCRITURA DE ARCHIVO
		(progn
			(setq
				coRutPpal
					(Vere01
						(list
							(list "FUNCIONES" "SELECCIONE EL DIRECTORIO FUNCIONES")
							(list "BDATOS" "SELECCIONE EL DIRECTORIO BDATOS")
						)
					)
				coFile
				(getstring "ESCRIBA EL NOMBRE DEL NUEVO ARCHIVO CSV (ENTER PARA EXISTENTE): ")
			)
			;br 
			(brkstep1
				'("coRutPpal" "coFile") (list coRutPpal coFile)
				"COMPFUN:0300" "0300"
			)
			;br..
			(if ;CSV EXISTENTE / NUEVO
				(not coFile)
				(setq coFile (Ruta "csv" "SELECCIONE EL ARCHIVO DE ESCRITURA"))
				(progn
					(setq coFile
						(strcat
							(Qnth02 coRutPpal "FUNCIONES" "RUTA" 0.001)
							coFile ".csv"
						)
					)
					;br 
					(brkstep1
						'("coFile") (list coFile)
						"COMPFUN:0400" "0400"
					)
					;br..
				)
			)
			(if ;CSV PREDETERMINADO
				(not coFile)
				(progn
					(setq coFile
						(strcat (Qnth02 coRutPpal "BDATOS" "RUTA" 0.001) "CompFile.csv")
					)
					;br 
					(brkstep1
						'("coFile") (list coFile)
						"COMPFUN:0500" "0500"
					)
					;br..
				)
			)
			(if
				coFile
				(progn
					(setq coFile (Sepa coFile "\\"))
					(Escr1 coRes
						(nth 0 (sepa (nth 1 coFile)))
						"csv" (Term (nth 0 coFile)) 0
					)
				)
			)
		)
	)
	coRes
) ;ComFun..