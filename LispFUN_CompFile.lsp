;:PROYECTO:
;;	<Lisp>
;:CLASIFICACION:
;;	<FUM>
;:FUNCION / TITULO: CompFile.Escribe un grupo de funciones en un archivo
;:SINTAXIS:
;;	<(CompFile [ModoIntroduccionLSP])>
;:DESCRIPCION:
;;	<<
;;	CompFile. A partir de la definicion de una lista csv
;;	o lista de funciones:. creara un archivo con las funciones o
;; 	DCL.
;;	>>
;:EJEMPLO:
;;	<<
;;	>
;;	>>
;:DEVUELVE:
;;	<<
;;	> coFileR. Nombre del archivo grabado <STR>
;;	> nil. Si no se escribio <nil>
;;	>>
;:ARGUMENTOS:
;;	<<
;;		> 1. md0. Modo de introduccion de datos:.
;;		0. Unicamente para funciones lisp con introduccion
;;		por medio de lista de funciones.
;;		1. Para introduccion de datos por medio de CSV.
;;	>>
;:DEPENDIENTES:
;;	<BrkStep1 >
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
;;	>El ID de directorio de donde extrae las funciones es "FUCIONES"
;;	>El ID de directorio de la lista de funciones es "BDATOS"
;;	
;;>>
;:ESTADO:
;;<<
;;	>En Curso
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
(defun CompFile
	(md0 /
;		md
;		cols coNam coRtLec coRtEsc coMod
;		coType coPreRt coRes
	)
	(setq md nil coLs nil coNam nil coRtLec nil coRtEsc nil coMod nil
		coType nil coPreRt nil coRes nil
	)
	;br 
	(brkstep1
		'("md0") (list md0)
		"COMPFILE:0000" "0000"
	)
	;br..
	(cond
		(
			(= (type md0) 'INT)
			(setq md md0)
			;br 
			(brkstep1
				'("md") (list md)
				"COMPFILE:0100" "0100"
			)
			;br..
			(setq
				coPpalRt
				(Vere01
					(list
						(list
							"FUNCIONES" "SELECCIONE LA CARPETA FUNCIONES"
						)
						(list
							"BDATOS" "SELECCIONE LA CARPETA BDATOS"
						)
					)
				)
			)
			(initget 128 "Lsp Dcl")
			(setq coType
				(getkword 
					"INDIQUE EL TIPO DE ARCHIVOS PARA COMPILAR [Lsp / Dcl] : "
				)
			)
			;br 
;			(brkstep1
;				'("coPpalRt" "coType")
;				(list coPpalRt coType)
;				"COMPFILE:0000" "0000"
;			)
			;br..
			;NOMBRE DE ARCHIVO DE ESCRITURA (0)
			(if
				(= (strcase coType) "DCL")
				(progn
					(setq
						coExt "dcl"
						coNam (Ruta "dcl" "COMPILADO DCL")
						coRtLec (getfiled "SELECCIONE LA RUTA DE LOS ARCHIVOS DCL" "DCL" " " 3)
					)
				)
				(progn
					(setq
						coExt "lsp"
						coNam (Ruta "lsp" "COMPILADO LSP")
						coRtLec (getfiled "SELECCIONE LA RUTA DE LAS FUNCIONES" "LSP" " " 3)
					)
				)
			) ;IF..
			;br 
			(brkstep1
				'("coExt" "coNam" "CoRtLec")
				(list coExt coNam coRtLec)
				"COMPFILE:0100" "0100"
			)
			;br..
			(if
				coRtLec
				(setq coRtLec (Term (nth 0 (sepa coRtLec "\\"))))
				(setq coRtLec (Qnth02 coPpalRt "FUNCIONES" "RUTA" 0.01))
			)
			;br
			(brkstep1
				'("coNam" "coRtLec") (list coNam coRtLec)
				"COMPFILE:0200" "0200"
			)
			;br..
			(if
				(not coNam)
				(progn
					(setq coNam
						(getstring "INDIQUE EL NOMBRE DEL ARCHIVO: ")
						coRtEsc
						(getfiled
							"INDIQUE LA RUTA DEL ARCHIVO COMPILADO"
							(strcase coExt) " " 3
						)
					)
					(if
						coRtEsc
						(setq coRtEsc (Term (nth 0 (sepa coRtEsc "\\"))))
						(setq coRtEsc (Qnth02 coPpalRt "FUNCIONES" "RUTA" 0.01))
					)
				)
				(progn
					(setq coNam (sepa coNam "\\")
						coRtEsc (Term (nth 0 coNam))
						coNam (nth 0 (sepa (nth 1 coNam) "."))
					)
				)
			)
			;br 
			(brkstep1
				'("coType" "coNam" "coRtEsc" "coRtLec")
				(list coType coNam coRtEsc coRtLec)
				"COMPFILE:0300" "0300"
			)
			;br..
			(if
				(setq
					coLs
					(Ruta "csv" "SELECCIONE LA LISTA CSV DE ARCHIVOS PARA COMPILAR")
				)
				(progn
					(setq coLs
						(BindLs
							(lec 0 coLs)
						)
					)
					;br
					(brkstep1
						'("coLs") (list coLs)
						"COMPFILE:0400" "0400"
					)
					;br..
				)
				(progn
					(setq coLs
						(BindLs
							(lec 0
								(strcat
									(Qnth02 coPpalRt "BDATOS" "RUTA" 0.01)
									"CompFile.csv"
								)
							)
						)
					)
					;br
					(brkstep1
						'("coLs") (list coLs)
						"COMPFILE:0500" "0500"
					)
					;br..
				)
			)
			;br 
			(brkstep1
				'("cols" "coExt" "coRtLec" "coRtEsc" "coNam")
				(list coLs coExt coRtLec coRtEsc coNam)
				"COMPFILE:0600" "0600"
			)
			;br..
			(initget 128 "Si No")
			(setq coMod
				(getkword "DESEA INTEGRAR LOS COMENTARIOS?: [Si / No]: ")
			)
			(if
				(= coMod "Si")
				(setq coMod 1)
				(setq coMod 0)
			)
			(if
				(= (strcase coExt) "DCL")
				(setq coRes (comLsp () coLs () coRtLec coRtEsc coNam coMod))
				(setq coRes (comLsp coLs () coRtLec () coRtEsc coNam coMod))
			)
		) ;COND TYPE T..
	) ;COND..
	coRes
) ;CompFile..