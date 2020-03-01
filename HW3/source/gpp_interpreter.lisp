; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Ahmed Semih Ozmekik              *
; *  File  : gpp_interpreter.lisp 			  *
; * 										  *
; *  Syntax Analyser for G++.		  		  * 
; *********************************************

;; Quick Note: 
;; There were some inconsistencies in the slides.
;; i.e. a keyword to indicate null was 'nil', yet in some parts
;; it specified as 'null'. As I did in the prev homework, I go with nil.
;; Another one is: CFG specified as: LISTVALUE -> ‘( VALUES ) | ‘() | null
;; But '‘' symbol didn't specified in lexical part. Rather instead,
;; I used my CFG - which is also compatible with prev hw - with for:
;; LISTVALUE -> (VALUES) | () | nil
;; And therefore in the homework pdf, correct usage of this one :(list 1 2 123)
;; must be (list (1 2 123))


;; Previous homework included for;
;; Getting the tokens and lexical syntax analysis.
(load "gpp_lexer.lisp") 


;; I needed to define one global variable to 
;; store the defined variables in the program.
;; It is only used to store variables.
;; It could be done with sending this table
;; via parameters, yet I didn't want function's
;; signatures to grow so much and prevent complexity 
;; in internal function calls and etc.  
(defvar *vars* (make-hash-table :test 'equal))


(defun token-list (seq)
	"Tokenize the line sequence and returns token list."

	(let ((lst (input-to-lst seq)))
			(let ((token-lst 
			(if (string= (car lst) ";;") "COMMENT"  
			(map 'list #'(lambda (token) (tokenize token (get-token-lst))) lst))))
			token-lst)))



(defun parser-interpret-shell()
		(loop (format t "~%>>> ") (parse-START (read-line))))

(defun parser-interpret-file (filename)
	(let ((in (open filename :if-does-not-exist nil)))		
	  (when in (loop for line = (read-line in nil)
	         	while line do (parse-START line)) (close in))
	  (unless in (format t "ERROR: No such file: '~a'" filename))))

;; Non-terminals:
;; START, INPUT, EXPLISTI, EXPI, EXPB.

;; START -> INPUT
(defun parse-START (seq)
	(let ((token-name (input-to-lst seq)) (token-id (token-list seq)))
		(parse-INPUT token-name token-id)
		)
	)

;; INPUT -> EXPI | EXPLISTI 
;; For testing purposes I added EXPB too.
(defun parse-INPUT (token-name token-id)

	(let ((stack (list '(0) '(0))) (n 0)) ;; init op-stack and val-stack.

		(cond ((car (is-EXPI token-id))
			 	(let ((expi (parse-EXPI token-name token-id stack))) 
			 	(setq stack (car expi))
				(setq n (cadr (is-EXPI token-id)))))
			 ((car (is-EXPB token-id))
				(let ((expb (parse-EXPB token-name token-id stack))) 
			 	(setq stack (car expb))
				(setq n (cadr (is-EXPB token-id)))))
			  ((car (is-EXPLISTI token-id))
				(let ((explisti (parse-EXPLISTI token-name token-id stack))) 
			 	(setq stack (car explisti))
				(setq n (cadr (is-EXPLISTI token-id)))))
			  ((car (is-EXPI2 token-id))
				(let ((expi2 (parse-EXPI2 token-name token-id stack))) 
			 	(setq stack (car expi2))
				(setq n (cadr (is-EXPI2 token-id)))))
			  (t (let () (princ "SYNTAX_ERROR Expression not recognized.") 
			  	(return-from parse-INPUT)))
			  )

	(if (/= (list-length token-id) n) 
		(let () (princ "SYNTAX_ERROR Expression not recognized.") 
		(return-from parse-INPUT)))

	(terpri)
	(princ "SYNTAX OK.") (terpri)
	(princ "Result: ")	
	(setq val-stack (cadr stack))	
	(princ (caadr stack))

	))


;; EXPI -> (+ EXPI EXPI) |
;; 		   (- EXPI EXPI) | 
;; 		   (* EXPI EXPI) |
;; 		   (/ EXPI EXPI) | 
;; 					  Id | 
;; 		    IntegerValue | 
;; 			(Id EXPLISTI).
(defun parse-EXPI (token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
		(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))
			(if (string= "OP_OP" t0) ;; ("OP" EXPI EXPI)  
				(let ((n 2))
					(setq op-stack (cons (car token-name) op-stack)) ;; push '('.
					(setq op-stack (cons (cadr token-name) op-stack)) ;; push 'op' or id.
					(setq stack (list op-stack val-stack))

					(if (is-op t1) ;; push 2x EXPI
					(dotimes (i 2) 
					(let ((expi (parse-EXPI (remove-first-n token-name n) 
						  (remove-first-n token-id n) stack)))
						(setq stack (car expi))
						(setq n (+ n (cadr expi))))))
					(if (string= "IDENTIFIER" t1) ;; push EXPLISTI
						(let ((expi (parse-EXPLISTI (remove-first-n token-name n) 
						  (remove-first-n token-id n) stack)))
						(setq stack (car expi))
						(setq n (+ n (cadr expi)))))

					;; evaluate and clean the stack.
					(let ((op1 (caadr stack)) (op2 (cadadr stack)) (op (caar stack)))
						(setq stack (list (cdar stack) (cddadr stack)))
						(setq stack (list (cdar stack) (cons (perform-op op2 op1 op) (cadr stack))))
						)

					(return-from parse-EXPI (list stack (+ n 1)))))

			;; ID
			(if (string= "IDENTIFIER" t0) 
				(let ((value nil))
					(if (gethash (car token-name) *vars*) (setq value 
						(gethash (car token-name) *vars*))(print "Error, no symbol found symbol table!"))
					(setq val-stack (cons value val-stack)) ;; push id.
					(setq stack (list op-stack val-stack))
					(setq n 1) (return-from parse-EXPI (list stack n))))

			;; VALUE
			(if (string= "VALUE" t0)
				(let ()
					(setq val-stack (cons (parse-integer (car token-name)) val-stack)) ;; push id.
					(setq stack (list op-stack val-stack))
					(setq n 1) (return-from parse-EXPI (list stack n))))

			(list nil 0)))) ;; wrong parsing method picked.
		


;; EXPI lookahead prediction.
;; After peeking, returns (t n) or (nil 0)
;; n being the token number.
(defun is-EXPI (token-id)
	;; (assert (null (cdr token-id))) ;; must be one item in list.
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))  
		 
		;; ("OP" EXPI EXPI) 
		(if (and (string= "OP_OP" t0) (is-op t1))
			(let ((p nil) (n 2)) ;; "(", "OP" -> n=2.
			(setq p	
			(and  (let ((expi (is-EXPI (remove-first-n token-id n))))	
					(setq n (+ n (cadr expi))) (car expi)) ;; EXPI
				  (let ((expi (is-EXPI (remove-first-n token-id n))))
					(setq n (+ n (cadr expi))) (car expi)) ;; EXPI
				  (string= "OP_CP" (car (remove-first-n token-id n))))) ;; ")"
			(return-from is-EXPI (list p (+ n 1)))))
		
		;; ID, VALUE
		(if (or (string= "IDENTIFIER" t0) (string= "VALUE" t0))
			(let ()
				(setq n 1) (return-from is-EXPI (list t n))))

		;; (Id EXPLISTI)
		(if (and (string= "OP_OP" t0) (string= "IDENTIFIER" t1))
			(let ((p nil) (n 2))
				(setq p
				(and (let ((expi (is-EXPLISTI (remove-first-n token-id n))))
						(setq n (+ n (cadr expi))) (car expi))
					 (string= "OP_CP" (car (remove-first-n token-id n)))))
				(return-from is-EXPI (list p (+ n 1)))))

		(list nil n)))

;; consists of other EXPI cfgs.
;; Those are handled different because they were
;; a little bit more than primitive expressions.
;; and some of them may require state changes.
;; And also they cannot be considered as EXPI because they are
;; not returning values, they instead returns lists.
;; So I seperated them.
;; EXPI -> (if EXPB EXPLISTI)
;; EXPI -> (if EXPB EXPLISTI EXPLISTI)
;; EXPI -> (list LISTVALUE)
;; EXPI -> (set ID EXPI) (different from above all)
(defun parse-EXPI2(token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
		(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))
			(if (and (string= "OP_OP" t0) (string= "KW_IF" t1)) ;; (if EXPB EXPLISTI)  
				(let ((n 2) (bool nil))
					(let ((expb (parse-EXPB (remove-first-n token-name n)
						(remove-first-n token-id n) stack)))
						(setq stack (car expb))
						(setq n (+ n (cadr expb))))

					(let ((explisti (parse-EXPLISTI (remove-first-n token-name n) 
						(remove-first-n token-id n) stack)))
						(setq stack (car explisti))
						(setq n (+ n (cadr explisti))))

					(setq lst (caadr stack))
					(setq bool (cadadr stack))
					(setq bool (caddr (cadr stack)))
					(setq val-stack (cddadr stack))
					(setq stack (list (car stack) (cdr val-stack)))

					(if (string= "OP_CP" (car (remove-first-n token-id n)))
						(let () (if (get-logic-val bool) (let () 
						(setq val-stack (cons lst (cddadr stack)))
						(setq stack (list (car stack) val-stack))
						(return-from parse-EXPI2 (list stack (+ n 1))))))
						)
					

					(let ((explisti (parse-EXPLISTI (remove-first-n token-name n) 
					(remove-first-n token-id n) stack)))
					(setq stack (car explisti))
					(setq n (+ n (cadr explisti))))

					(setq lst2 (cadr stack))
					(setq stack (list (car stack) (cddadr stack)))
					(if (get-logic-val bool) (setq val-stack (cons lst (cddadr stack)))
						(setq val-stack (cons lst2 (cddadr stack)))
						)

					(setq stack (list (car stack) val-stack))
					(return-from parse-EXPI2 (list stack (+ n 1)))))

			(if (and (string= "OP_OP" t0) (string= "KW_LIST" t1))
				(let ((n 2)) ;; EXPI -> (list LISTVALUE)
					(return-from parse-EXPI2 (parse-LISTVALUE 
						(remove-first-n token-name n) (remove-first-n token-id n) stack))))

			(if (and (string= "OP_OP" t0) (string= "KW_SET" t1))
				(let ((n 2)) ;; EXPI -> (set id EXPI)
					(setq var (car (remove-first-n token-name n)))
					(setq n (+ n 1))
					(let ((expi (parse-EXPI (remove-first-n token-name n)
						(remove-first-n token-id n) stack)))
						(setq stack (car expi))
						(setq n (+ n (cadr expi))))
					(setq val-stack (cadr stack))
					(setq value (car val-stack))
					(setf (gethash var *vars*) value)
					(setq stack (list (car stack) val-stack))
					(return-from parse-EXPI2 (list stack (+ n 1)))))

			(list nil 0))))

(defun is-EXPI2 (token-id)
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))  
		 
		
		(if (and (string= "OP_OP" t0) (string= "KW_IF" t1))
			(let ((p1 nil) (n 2)) ;; "(", "if" -> n=2.
			(setq p1	;; ("if" EXPB EXPLISTI) 
			(and  (let ((expb (is-EXPB (remove-first-n token-id n))))	
					(setq n (+ n (cadr expb)))(car expb)) ;; EXPB
				  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))
					(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
				  (string= "OP_CP" (car (remove-first-n token-id n))))) ;; ")"


			(setq n 2)
			(setq p2 ;; ("if" EXPB EXPLISTI EXPLISTI) 
			(and (let ((expb (is-EXPB (remove-first-n token-id n))))	
					(setq n (+ n (cadr expb))) (car expb)) ;; EXPB
				  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))
					(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
				  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))
					(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
				  (string= "OP_CP" (car (remove-first-n token-id n))))) ;; ")"
			(return-from is-EXPI2 (list (or p1 p2) (+ n 1)))))


		;; (set ID EXPI)
		(if (and (string= "OP_OP" t0) (string= "KW_SET" t1))
			(let ((p nil) (n 3))
				(setq p
				(and 	(string= "IDENTIFIER" (car (remove-first-n token-id (- n 1))))
						(let ((expi (is-EXPI (remove-first-n token-id n))))
						(setq n (+ n (cadr expi))) (car expi))
					 (string= "OP_CP" (car (remove-first-n token-id n)))))
				(return-from is-EXPI2 (list p (+ n 1)))))

		;; (list LISTVALUE)
		(if (and (string= "OP_OP" t0) (string= "KW_LIST" t1))
			(let ((p nil) (n 2))
				(setq p
				(and (let ((listvaluep (is-LISTVALUE (remove-first-n token-id n))))
						(setq n (+ n (cadr listvaluep))) (car listvaluep))
					 (string= "OP_CP" (car (remove-first-n token-id n)))))
				(return-from is-EXPI2 (list p (+ n 1)))))
		(list nil n)))


(defun remove-first-n (lst n)
	(if (= n 0) lst (remove-first-n (cdr lst) (- n 1))))

(defun perform-op (op1 op2 op)
	(let ((num1 op1) (num2 op2)) 
	(cond ((string= "+" op) (+ num1 num2))
		  ((string= "-" op) (- num1 num2))
		  ((string= "*" op) (* num1 num2))
		  ((string= "/" op) (/ num1 num2)))))

;; EXPB -> (and EXPB EXPB) |
;; 			(or EXPB EXPB) | 
;; 			    (not EXPB) |
;; 		 (equal EXPB EXPB) | 
;;		 (equal EXPI EXPI) | 
;;			   BinaryValue .
(defun parse-EXPB (token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
		(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))

			(if (string= "OP_OP" t0) ;; (and, equal, or, not EXPB EXPB)  
			(let ((n 2))
				(setq op-stack (cons (car token-name) op-stack)) ;; push '('.
				(setq op-stack (cons (cadr token-name) op-stack)) ;; push KW.
				(setq stack (list op-stack val-stack))

				;; (equal EXPI EXPI)
				(if (string= "KW_EQUAL" t1)
					(if (car (is-EXPI (remove-first-n token-id n)))
						(let () (dotimes (i 2) 
						(let ((expi (parse-EXPI (remove-first-n token-name n) 
							  (remove-first-n token-id n) stack)))
							(setq stack (car expi))
							(setq n (+ n (cadr expi)))))

						(let ((op1 (caadr stack)) (op2 (cadadr stack)) (op (caar stack)))
						(setq stack (list (cdar stack) (cddadr stack)))
						(setq stack (list (cdar stack) (cons (perform-logic-op op op1 op2) (cadr stack)))))
						(return-from parse-EXPB (list stack (+ n 1))))))


				;; EXPB.
				(let ((expb (parse-EXPB (remove-first-n token-name n) 
					  (remove-first-n token-id n) stack)))
					(setq stack (car expb))
					(setq n (+ n (cadr expb))))

				;; EXPB.
				(if (not (string= "KW_NOT" t1))
					(let ((expb (parse-EXPB (remove-first-n token-name n) 
					  (remove-first-n token-id n) stack)))

					(setq stack (car expb))
					(setq n (+ n (cadr expb)))

					;; evaluate and clean the stack.
					(let ((op1 (caadr stack)) (op2 (cadadr stack)) (op (caar stack)))
						(setq stack (list (cdar stack) (cddadr stack)))
						(setq stack (list (cdar stack) (cons (perform-logic-op op op1 op2) (cadr stack))))
						))
					(let ((op1 (caadr stack)) (op2 (cadadr stack)) (op (caar stack)))
						(setq stack (list (cdar stack) (cddadr stack)))
						(setq stack (list (cdar stack) (cons (perform-logic-op op op1) (cadr stack))))
						)
					)
				(return-from parse-EXPB (list stack (+ n 1)))))

			(if (or (string= "KW_TRUE" t0) (string= "KW_FALSE" t0))
				(let ()
					(setq val-stack (cons (car token-name) val-stack)) ;; push id.
					(setq stack (list op-stack val-stack))
					(setq n 1) (return-from parse-EXPB (list stack n))))))

			(list nil 0)) ;; wrong parsing method picked.

(defun perform-logic-op (op op1 &optional op2)

	(if (and op2 (string= "equal" op) (and (integerp op1) 
		(integerp op2)))
		(if (= op1 op2) 
			(return-from perform-logic-op "true")
			(return-from perform-logic-op "false")))

	(if (let ((var1 (get-logic-val op1)) (var2 (get-logic-val op2)))
		(if op2 (cond ((string= "and" op) (and var1 var2))
					((string= "or" op) (or var1 var2))
					((string= "equal" op) (equal var1 var2)))
				(if (string= "not" op) (not var1)))) "true" "false"))

(defun get-logic-val (op)
	(if (and (not (integerp op)) op (or (string= "true" op) (string= "false" op)))
	(string= "true" op)))

;; EXPI lookahead prediction.
(defun is-EXPB (token-id)
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))  
		;; (and, or, equal, not ..) 
		(if (string= "OP_OP" t0)
			(let ((p nil) (n 2)) ;; "(", "OP" -> n=2.

				(setq p (let ((expb (is-EXPB (remove-first-n token-id n))))
						(setq n (+ n (cadr expb))) (car expb)))    ;; EXPB

				(if (not (string= "KW_NOT" t1))	
					 (setq p (and p (let ((expb (is-EXPB (remove-first-n token-id n))))
					 	(setq n (+ n (cadr expb ))) (car expb))))) ;; EXPB

				(if (string= "KW_EQUAL" t1)
					(setq p (or p (let ((p1 nil) (m 2))
						(setq p1
						(and (let ((expi (is-EXPI (remove-first-n token-id m))))
								(setq m (+ m (cadr expi))) (car expi))   ;; EXPI
							 (let ((expi (is-EXPI (remove-first-n token-id m))))
							 	(setq m (+ m (cadr expi))) (car expi)))) ;; EXPI
						(if p1 (setq n m)) p1))))

				(setq p (and p (string= "OP_CP" (car (remove-first-n token-id n)))))
				(return-from is-EXPB (list p (+ n 1)))))

		;; Binary Value.
		(if (or (string= "KW_TRUE" t0) (string= "KW_FALSE" t0)) 
			(let ()
				(setq n 1) (return-from is-EXPB (list t n))))
		(list nil n)))
	


;; EXPLISTI -> (concat EXPLISTI EXPLISTI) | 
;; 				   (append EXPI EXPLISTI) | 
;; 								LISTVALUE | 
;; 									nil   .
(defun parse-EXPLISTI (token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))
		
		;; null
		(if (string= "KW_NIL" t0) (let ()
				(setq n 1) (setq val-stack (cons (car token-name) val-stack))
				(setq stack (list op-stack val-stack))
				(return-from parse-EXPLISTI (list stack n))))

		(if (car (is-LISTVALUE token-id)) (return-from parse-EXPLISTI 
			(parse-LISTVALUE token-name token-id stack)))

		(if (string= "OP_OP" t0) 
		;; (concat EXPLISTI EXPLISTI) 
		(let ()
		(if (and (string= "KW_CONCAT" t1))
			(let ((n 2)) ;; "(", "concat" -> n=2.


				(dotimes (i 2) ;; parse EXPLISTI
				(let ((explisti (parse-EXPLISTI (remove-first-n token-name n)
					(remove-first-n token-id n) stack)))

					(setq n (+ n (cadr explisti)))
					(setq stack (car explisti))))

			#| 
			(setq val-stack (cadr stack))
			(let ((val-stack1 (car val-stack)) (val-stack2 (cadr val-stack)) (r '()))
				(setq r (concatenate val-stack2 (cdr val-stack1)))
				(setq val-stack (cons r (cddr val-stack))))
			(setq stack (list op-stack val-stack))
			|# 

			(return-from parse-EXPLISTI (list stack (+ n 1)))))

		(if (and (string= "KW_APPEND" t1))
			(let ((n 2)) ;; "(", "append" -> n=2.

			 ;; parse EXPI
			 (let ((expi (parse-EXPI (remove-first-n token-name n) 
				(remove-first-n token-id n) stack)))
				(setq n (+ n (cadr expi)))
				(setq stack (car expi)))


			;; parse EXPLIST
			(let ((explisti (parse-EXPLISTI (remove-first-n token-name n) 
				(remove-first-n token-id n) stack)))
				(setq n (+ n (cadr explisti)))
				(setq stack (car explisti)))
	
			(setq val-stack (cadr stack))

			(let ((val-stack1 (car val-stack)) (val (cadr val-stack)) (r '()))
				(setq r (add-to-last val-stack1 val))
				(setq val-stack (cons r (cddr val-stack))))
			(setq stack (list op-stack val-stack))
			(return-from parse-EXPLISTI (list stack (+ n 1))))

			)

		)

		)))

		(list nil 0))


;; EXPLISTI lookahead.
(defun is-EXPLISTI (token-id)
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))

		;; null
		(if (string= "KW_NIL" t0) (let ()
				(setq n 1) (return-from is-EXPLISTI (list t n))))

		;; LISTVALUE
		(let ((listvaluep (is-LISTVALUE token-id)))
			(if (car listvaluep) (return-from is-EXPLISTI listvaluep)))


		(if (string= "OP_OP" t0) 
		;; (concat EXPLISTI EXPLISTI) 
		(let ()
		(if (and (string= "KW_CONCAT" t1))
			(let ((p nil) (n 2)) ;; "(", "concat" -> n=2.
			(setq p	
			(and  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))	
					(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
				  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))
					(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
				  (string= "OP_CP" (car (remove-first-n token-id n))))) ;; ")"
			(return-from is-EXPLISTI (list p (+ n 1)))))
			

			;; (append EXPI EXPLISTI)
			(if (and (string= "KW_APPEND" t1))
				(let ((p nil) (n 2)) ;; "(", "append" -> n=2.	
				(setq p	
				(and  (let ((expi (is-EXPI (remove-first-n token-id n))))	
						(setq n (+ n (cadr expi))) (car expi)) ;; EXPI
					  (let ((explisti (is-EXPLISTI (remove-first-n token-id n))))
						(setq n (+ n (cadr explisti))) (car explisti)) ;; EXPLISTI
					  (string= "OP_CP" (car (remove-first-n token-id n))))) ;; ")"
				(return-from is-EXPLISTI (list p (+ n 1)))))))

		(list nil n)))


;; LISTVALUE -> (VALUES) | () | nil
(defun parse-LISTVALUE (token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))
		
		;; null
		(if (string= "KW_NIL" t0) (let ()
				(setq n 1) (setq val-stack (cons (car token-name) val-stack))
				(setq stack (list op-stack val-stack))
				(return-from parse-LISTVALUE (list stack n))))


		(if (string= "OP_OP" t0) 
			(let ((n 1))

			;; ()
			(if (string= "OP_CP" t1)
				(let ((n 2)) (setq val-stack (cons '(0) val-stack))
					(setq stack (list op-stack val-stack))
					(return-from parse-LISTVALUE (list stack n))))

			;; VALUES
			(if (car (is-VALUES (cdr token-id))) (let (valuesp '())
				(setq valuesp (parse-VALUES (cdr token-name) (cdr token-id) stack))
				(setq n (+ 2 (cadr valuesp)))
				(return-from parse-LISTVALUE (list (car valuesp) n)))))))

		(list nil 0)))

(defun is-LISTVALUE (token-id)
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0)) 

		;; null
		(let () (if (string= "KW_NIL" t0) (let ()
				(setq n 1) (return-from is-LISTVALUE (list t n)))))

		(if (string= "OP_OP" t0) 
			(let ((n 1))

			;; ()
			(if (string= "OP_CP" t1)
				(let ((n 2)) (return-from is-LISTVALUE (list t n))))

			;; VALUES
			(if  (and (let ((valuesp (is-VALUES (cdr token-id))))
				 (setq n (+ n (cadr valuesp)))(car valuesp))
				 (string= "OP_CP" (car (remove-first-n token-id n))))
			(return-from is-LISTVALUE (list t (+ n 1))))))

		(list nil n)))


(defun add-to-last (lst a)
	(push a (cdr (last lst))) lst)

;; VALUES -> VALUES IntegerValue | IntegerValue
(defun parse-VALUES (token-name token-id stack)
	(let ((op-stack (car stack)) (val-stack (cadr stack)))
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0) (val-lst (car val-stack)))
		(if token-id (let ()
			(if (not (listp val-lst)) (setq val-lst '(0))) ;; val-lst is ready.

			(let ((n 1)) (if (string= "VALUE" t0)
				(let ()
				(setq val-lst (car val-stack))	

				;; 	liste degil ve 0 ise, direkt ekle ;; (= (car val-stack) 0)
				(if (and (not (listp (car val-stack))) t) 
					(setq val-stack (cons val-lst val-stack)))


				;; val-lst liste.
				(if (listp val-lst)	
				(setq val-lst (add-to-last val-lst (parse-integer (car token-name))))
				(setq val-lst (list (parse-integer (car token-name)))))

					
				(if (listp (car val-stack)) (setq val-stack (cons val-lst (cdr val-stack)))
				(setq val-stack (cons val-lst val-stack)))

				(setq stack (list op-stack val-stack))
				(let ()
					(if (car (is-VALUES (cdr token-id))) 
						(let ((valuesp (parse-VALUES (cdr token-name) 
						(cdr token-id) stack)))
						(setq n (+ n (cadr valuesp)))
						(return-from parse-VALUES (list (car valuesp) n)))))

				(return-from parse-VALUES (list stack n)))))))))

		(list nil 0)) 

(defun is-VALUES (token-id)
	(let ((t0 (car token-id)) (t1 (cadr token-id)) (n 0))
		(if token-id (let ()

		;; VALUES IntegerValue | IntegerValue
		(let ((n 1)) (if (string= "VALUE" t0) 
			(let ((valuesp (is-VALUES (cdr token-id))))
			(if (car valuesp) (let () (setq n (+ n (cadr valuesp)))
				(return-from is-VALUES (list t n)))
				(return-from is-VALUES (list t 1))))))


		(list nil n)))))

(defun remove-last(l)
    (reverse (cdr (reverse l))))

(defun is-op (token)
	(or (string= "OP_PLUS" token)
		(string= "OP_MINUS" token)
		(string= "OP_DIV" token)
		(string= "OP_MULT"token)))

(defun is-logic (token)
	(or (string= "and" token)
		(string= "or" token)
		(string= "equal" token)
		(string= "not" token)))


(defun parser-gppinterpreter (&optional filename)
	(if filename (parser-interpret-file filename) (parser-interpret-shell))
	)

(if *args* (parser-gppinterpreter (car *args*)) (parser-gppinterpreter))

