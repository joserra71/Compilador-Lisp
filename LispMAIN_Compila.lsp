;:PROYECTO:
;;	<LISP>
;:CLASIFICACION:
;;	<>
;:FUNCION / TITULO:
;:SINTAXIS:
;;	<>
;:DESCRIPCION:
;;	<<
;;
;;	>>
;:EJEMPLO:
;;	<<
;;	>
;;	>>
;:DEVUELVE:
;;	<<
;;	
;;	>>
;:ARGUMENTOS:
;;	<<
;;
;;	>>
;:DEPENDIENTES:
;;	< >
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
;;	>
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
(defun c:Compila ( /
	comPH
	)
	(setq comPH nil)
	(setq comPH
		(Vere01
			(list
				(list "FUNCIONES" "SELECCIONE EL DIRECTORIO DE FUNCIONES")
			)
		)
	)
	;br 
;	(brkstep1
;		'("comPH") (list comPh)
;		"COMPILA:0000" "0000"
;	)
	;br..
	(if
		(not (nth 0 (atoms-family 1 (list "CompFile"))))
		(progn
			(load (strcat (Qnth02 comPH "FUNCIONES" "RUTA" 0.01) "LispFUN_CompFile.lsp"))
		)
	)
	(CompFile)
) ; C:Compila..





