; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Ahmed Semih Ozmekik              *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"


(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."

	(with-open-file (stream filename)
    	(loop for word = (read-line stream nil)
          while word collect (string-to-list word) into lst
          finally (return (pgraph2symbol lst)))))


;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***



(defun pgraph2symbol (lst)
;; Converts char list to symbol list which is the desired format in assignment.
(loop for word in lst
	collect (loop for letter in word
		collect (intern (string (i2c (- (c2i letter) 32)))))))

(defun symbol2pgraph (lst)
;; Converts symbol list to char list which is the desired format for to 
;; work better in the flow of our program.
	(loop for word in lst
		collect (loop for letter in word
			collect (i2c (+ 32 (c2i (char (string letter) 0)))))))

(defun string-to-list (str) 
	(coerce str 'list))

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun words-equalp (word1 word2)
	(let ((s1 (list-to-string word1)) (s2 (list-to-string word2)))
		(string-equal s1 s2)))

(defun create-word-map (wlist) 

	(let ((wmap (make-hash-table :test 'equalp)))
		(loop for word in wlist
			do (setf (gethash (list-to-string word) wmap) t))
	wmap))

(defun read-pgraph-as-list (filename)
    (with-open-file (stream filename)
      (loop while (peek-char nil stream nil nil)
           collect (string-to-list (string-downcase(symbol-name (read stream)))))))

(defun spell-checker-0 (word dictlist) 
	(loop for w in dictlist
		do (if (words-equalp w word) (return-from spell-checker-0 t)))
 	nil)

(defun spell-checker-1 (word dictmap)
 	(gethash (list-to-string word) dictmap))

(defun create-word-table (wlist) 

	(let ((wmap (make-hash-table :test 'equalp)))
		(loop for word in wlist
			do (setf (gethash (list-to-string word) wmap) t))
	wmap))

(defun get-alphabet ()
	(loop with a = (c2i #\a)
		for i below 26
		collect (i2c (+ a i))))

(defun nth-perm (lst n)
	;; Generates the nth-perm from given lst alphabet in const time. 
	(if (null lst) '() 
		(let ((x (factorial (- (list-length lst) 1))))
			(setq i (truncate (floor (/ n x))))

			(merge-lists (list (nth i lst)) (nth-perm 
			(merge-lists (subseq lst 0 i) (subseq lst (+ i 1) 
			(list-length lst))) (mod n x))))))


(defun merge-lists (lst1 lst2) 
	(reduce #'cons lst1 :initial-value lst2 :from-end t))

(defun factorial (n) 
	(if (= n 0) (return-from factorial 1) (* n (factorial (- n 1)))))

(defun find-encode-rule-A (pgraph)

	(let ((dict (create-word-table (read-pgraph-as-list "dictionary2.txt"))))
	(print "Brute Forcing....")
	(dotimes (n (factorial 26))
		(format t "Testing... ~D~%" n)
		(let ((rule (nth-perm (get-alphabet) n)) (score 0))
			(loop for word in (decode-pgraph pgraph rule)
				do (if (spell-checker-1 word dict) (incf score)))
			(format t "Score: ~D~%" score)
			(if (= score (list-length pgraph)) (return-from find-encode-rule-A rule)))
		)
	))

(defun decode-pgraph (pgraph permlst)
	(let ((score 0))
		(loop for word in pgraph
			collect (decode-word word permlst))))

(defun encode-file (infile outfile permlst)
	(let ((outfile (open outfile :direction :output
										   :if-exists :overwrite
										   :if-does-not-exist :create)) 
		(pgraph (read-pgraph-as-list infile))
		(word-per-line 8))
		(loop for word in pgraph
			do (if (eq (+ 0 (mod (position word pgraph) word-per-line)) 0) 
				(fresh-line outfile) (write-char #\SPACE outfile))
			do (format outfile (list-to-string (encode-word word permlst))))
		(close outfile))

	)

(defun encode-word (word permlst)
	(loop for lttr in word
		collect (nth (c2i lttr) permlst)))

(defun decode-word (word permlst)

	(loop for lttr in word
		collect (i2c (position lttr permlst))))

(defun find-encode-rule-B0 (pgraph)
	(let ((dict (create-word-table (read-pgraph-as-list "dictionary2.txt")))
		 (perm-map (make-hash-table)))

	(map-lists perm-map (most-frequent-list) (frequent-letters pgraph))
	(print perm-map)


	(print "Brute Forcing....")
	(dotimes (n (factorial 26))
		(format t "Testing... ~D~%" n)
		(let ((rule (nth-perm-2 perm-map n)) (score 0))
			(loop for word in (decode-pgraph pgraph rule)
				do (if (spell-checker-1 word dict) (incf score)))
			(format t "Score: ~D~%" score)
			(if (= score (list-length pgraph)) (return-from find-encode-rule-B0 rule)))
		)
	))

(defun frequency-analysis (pgraph)

	(let ((freq-map (make-hash-table)))

		(loop for word in pgraph
		 do (loop for letter in word
		 	 do (let ((value (gethash letter freq-map)))
		 	 		(if value 
		 	 		(setf (gethash letter freq-map) (+ value 1)) 
					(setf (gethash letter freq-map) 1)))))
        freq-map))

(defun get-table-keys (table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) table)
    keys))

(defun get-table-values (table)
	(loop for letter in (get-alphabet)
		when (gethash letter table)
		collect (gethash letter table)))

(defun frequent-letters (pgraph)
	(let ((table  (frequency-analysis pgraph)))
	  (setq freq-letters 
	  (sort (get-table-keys table)
	  (lambda (k1 k2)
	    (> (gethash k1 table)
	       (gethash k2 table)))))
	  (subseq freq-letters 0 6) ; the most frequent 6 letter in order.
	  ))
	
(defun rest-alphabet (lst)
	(remove-if (lambda (x) (find x lst)) (get-alphabet)))

(defun most-frequent-list ()
	(string-to-list "etaoin"))

(defun map-lists (table keys values)
	(dotimes (n (list-length keys))
		(setf (gethash (nth n keys) table) (nth n values))))

(defun nth-perm-2 (perm-map n)
	(let ((values (get-table-values perm-map))
		  (keys (get-table-keys perm-map))
		  (rule (make-hash-table))
		  )
	; keys most frequent
	; values frequency analysis of the file

	; deep copies of perm-map 
	(loop for key being the hash-keys of perm-map using (hash-value value)
		do (setf (gethash key rule) value))


	(let ((rest-values (nth-perm (rest-alphabet values) n))
		  (rest-keys (rest-alphabet keys)))
		(map-lists rule rest-keys rest-values)
		(get-table-values rule))))


(defun word2-dual-list (word)

	(loop for n from 0 to (- (list-length word) 2)
		collect (string-to-list (format nil "~C~C" (nth n word)
			(nth (+ n 1) word))))
	)

(defun create-dual-table (pgraph)

	(let ((dual-table (make-hash-table :test 'equalp)))
	(loop for word in pgraph
		do (loop for dual in (word2-dual-list word)
			do (setq dual (list-to-string dual))
			do (if (null (gethash dual dual-table)) 
				(setf (gethash dual dual-table) 1)
				(incf (gethash dual dual-table)))
			)
		)

	(print dual-table)
	dual-table))

(defun get-co (dual-table letter align)
	(if align
		(loop for key being the hash-keys of dual-table 
			when (eq letter (nth 1 (string-to-list key)))
			collect (nth 0 (string-to-list key)))
		(loop for key being the hash-keys of dual-table 
			when (eq letter (nth 0 (string-to-list key)))
			collect (nth 1 (string-to-list key)))
		)

	)

(defun table2aslist (table)
  (let ((aslist nil)) (maphash (lambda (k v) (push (cons k v) aslist))
             table)
    aslist))


(defun table-top-value (table)
  "Returns the top entry from hash table."
  (car (car (subseq (sort (table2aslist table) #'> :key #'cdr) 0 1))))


(defun get-co-table (pgraph)
	(let ((co-table (make-hash-table)) ; most co-occurence map: ab -> a . b
		  (dual-table (create-dual-table pgraph))
		  (dual ())	; dual word: ab
		  (first-letter ()) ; first letter: a
		  (second-letter ())) ; second letter: b

		  ;(print dual-table)

		  (loop while (> (hash-table-count dual-table) 0) ; absorbing whole dict
				do (setq dual (table-top-value dual-table))
				do (setq first-letter (first (string-to-list dual)))
				do (setq second-letter (first (last (string-to-list dual))))
				do (reduce-dual-table dual-table first-letter nil) ; Remove x* 
				do (reduce-dual-table dual-table second-letter t)  ; Remove *x	
				do (setf (gethash first-letter co-table) second-letter))
		co-table))


(defun match (dict-table custom-table rule-table)
	"Find new matches from rule-table regarding to co-occurence tables."
	(let ((dict-co ()) (custom-co ()) (before-size (hash-table-count rule-table))) 
	(loop for key being the hash-keys of rule-table using (hash-value value)
		; say; mapping: i->a key: i value: a
		do (setq dict-co (gethash key dict-table) ; co-occurence: is -> s
		do (setq custom-co (gethash value custom-table))) ; co-occurence: ab -> b

		when (and custom-co dict-co) ; both not null.
		do (if (null (gethash dict-co rule-table)) ; key in rule is not occupied.
				; value in rule is not occupied.
				(if (null (find custom-co (get-table-values rule-table)))
					(setf (gethash dict-co rule-table) custom-co)))); s->b
	; return t, if rule-table has changed
	(/= before-size (hash-table-count rule-table)))) 

(defun get-co-occurence-rule (pgraph)
	;; dictionary3.txt is specially made dictionary for just to solve
	;; document3.txt, it contains letters from document3.txt
	;; For to simulate a test case scenario, I needed a perfectly 
	;; fitting dictionary for a document to get fitting frequent statistics of duals
	;; and decrypt this document regarding to this 'perfect' statistics.
	(let ((dict (get-co-table (read-pgraph-as-list "dictionary3.txt")))
		 (encoded (get-co-table pgraph))
		 (rule-table (make-hash-table)))

		(terpri)
		(princ "Dictionary co-occurences:")
		(print dict)
		(terpri)
		(princ "Encoded co-occurences:")
		(print encoded)
		
		(map-lists rule-table (most-frequent-list) 
				   (frequent-letters pgraph))
		(loop while (match dict encoded rule-table))
		rule-table))

(defun reduce-dual-table (dual-table letter align)
	(let ((key ()) (value ()))
		(if align 
		(setq key (loop for i in (get-alphabet); Right Alignment of letter: *i
		 collect (concatenate 'string (string i) (string letter))))
		(setq key (loop for i in (get-alphabet); Left Alignment of letter. i*
		 collect (concatenate 'string (string letter) (string i)))))

		;; Remove others since we only need the most frequent of them.
		(loop for k in key 
			do (remhash k dual-table))
	))


(defun find-encode-rule-B1 (pgraph)
	(let ((rule-table (get-co-occurence-rule pgraph))
		  (dict (create-word-table (read-pgraph-as-list "dictionary2.txt"))))

		(print "Brute Forcing Other Letters....")
		(dotimes (n (factorial 26))
		(format t "Testing... ~D~%" n)
		(let ((rule (nth-perm-2 rule-table n)) (score 0))
			(loop for word in (decode-pgraph pgraph rule)
				do (if (spell-checker-1 word dict) (incf score)))
			(format t "Score: ~D~%" score)
			(if (= score (list-length pgraph)) (return-from find-encode-rule-B1 rule)))
		)))



;; DECODE FUNCTIONS
(defun Gen-Decoder-A (paragraph)

	(let ((rule (find-encode-rule-A (symbol2pgraph paragraph))))
		(princ "---------------- FOUND ----------------")
		(function (lambda (word) (list-to-string 
			(decode-word (string-to-list word) rule))))))

(defun Gen-Decoder-B-0 (paragraph)

  	(let ((rule (find-encode-rule-B0 (symbol2pgraph paragraph))))
  		(princ "---------------- FOUND ----------------")
  		(function (lambda (word) (list-to-string 
  			(decode-word (string-to-list word) rule ))))))

(defun Gen-Decoder-B-1 (paragraph)

  	(let ((rule (find-encode-rule-B1 (symbol2pgraph paragraph))))
  		(princ "---------------- FOUND ----------------")
  		(function (lambda (word) (list-to-string 
  			(decode-word (string-to-list word) rule ))))))


(defun Code-Breaker (document decoder)
  	(loop for word in (symbol2pgraph document)
  		collect (string-to-list (funcall decoder word)) into pgraph
  		finally (return (loop for word in pgraph
  				do (push #\SPACE (cdr (last word)))
  				collect (list-to-string word) into text
  				finally (return (list-to-string text))))))

;; Test code...

(defun test-encode-files ()

	;; they were encrypted according to first generations of combinations  
	;; so that it won't take much time to decrypt during the test phase
	;; 4000- <<< 26!

	;; You can increase the generation number, which will make 
	;; the combinations better mixed up yet this time you should need to 
	;; wait longer for the decoder to solve them.
	(encode-file "document1.txt" "encoded_file1.txt" (nth-perm (get-alphabet) 2000))
	(encode-file "document2.txt" "encoded_file2.txt" (nth-perm (get-alphabet) 3508))
	(encode-file "document3.txt" "encoded_file3.txt" (nth-perm (get-alphabet) 500))
	(encode-file "document4.txt" "encoded_file4.txt" (nth-perm (get-alphabet) 700))
	(encode-file "document5.txt" "encoded_file5.txt" (nth-perm (get-alphabet) 1500))
	)

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(let ((doc (read-as-list "dictionary1.txt")))
		(print doc))

	;; get documents
	(let ((d1 (pgraph2symbol (read-pgraph-as-list "encoded_file1.txt")))
		  (d2 (pgraph2symbol (read-pgraph-as-list "encoded_file2.txt")))
		  (d3 (pgraph2symbol (read-pgraph-as-list "encoded_file3.txt")))
		  (d4 (pgraph2symbol (read-pgraph-as-list "encoded_file4.txt")))
		  (d5 (pgraph2symbol (read-pgraph-as-list "encoded_file5.txt"))))

		(princ "Printing encrypted files:")
		(print d1)
		(print d2)
		(print d3)
		(print d4)
		(print d4)
		

		(print "Starting to decrypting phase...")
		;; You can uncomment those decoders check them..
		;; get decoders
		(let ((f1 (Gen-Decoder-A d1))
			 (f2 (Gen-Decoder-A d2))
			 (f3 (Gen-Decoder-B-0 d3))
			 (f4 (Gen-Decoder-B-1 d4))
			 (f5 (Gen-Decoder-B-0 d5)))

			 (print "Decryping is done!")
			 (print "Documents, in order: ")
			 (print (Code-Breaker d1 f1))
			 (terpri)
			 (print (Code-Breaker d2 f2))
			 (terpri)
			 (print (Code-Breaker d3 f3))
			 (terpri)
			 (print (Code-Breaker d4 f4))
			 (terpri)
			 (print (Code-Breaker d5 f5))
			)
		)
)


;; test code...
(test-encode-files)
(test_on_test_data)
