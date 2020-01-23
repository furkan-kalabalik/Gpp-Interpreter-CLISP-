;; (defconstant KW_AND 10)
;; (defconstant KW_OR 11)
;; (defconstant KW_NOT 12)
;; (defconstant KW_EQUAL 20)
;; (defconstant KW_LESS 21)
;; (defconstant KW_NIL 666)
;; (defconstant KW_LIST 30)
;; (defconstant KW_APPEND 31)
;; (defconstant KW_CONCAT 32)
;; (defconstant KW_SET 40)
;; (defconstant KW_DEFFUN 41)
;; (defconstant KW_FOR 50)
;; (defconstant KW_IF 51)
;; (defconstant KW_EXIT 60)
;; (defconstant KW_LOAD 61)
;; (defconstant KW_DISP 70)
;; (defconstant KW_TRUE 71)
;; (defconstant KW_FALSE 72)

;; (defconstant OP_PLUS 0)
;; (defconstant OP_MINUS 1)
;; (defconstant OP_DIV 2)
;; (defconstant OP_MULT 3)
;; (defconstant OP_OP 4)
;; (defconstant OP_CP 5)
;; (defconstant OP_DBLMULT 6)
;; (defconstant OP_OC 7)
;; (defconstant OP_CC 8)
;; (defconstant OP_COMMA 9)

;; (defconstant COMMENT 100)
;; (defconstant VALUE 101)
;; (defconstant IDENTIFIER 102)

(defvar *current* nil)

(defconstant OP_LIST '("+" "-" "/" "*" "(" ")" "**" "," "'"));;Defines operations
(defconstant KW_LIST '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "defvar" "while" "for"));;Defines keywords
(defconstant KW_LIST_COR '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_DEFVAR" "KW_WHILE" "KW_FOR"));;These are for printing correspondance keywords
(defvar *IDENTIFIERS* nil)
(defun get-alphabetics (inputStr lexeme)
    (let ((c (peek-char nil inputStr nil)))
        (cond ((not (eq nil (member (string c) OP_LIST :test #'string=))) (reverse lexeme))
              ((or (eq c #\Space) (eq c nil)) (reverse lexeme))
              (t (progn (push (read-char inputStr) lexeme) (get-alphabetics inputStr lexeme)))
        )
    )
)

;;Returns character class. We have to class either alphanumeric which is a-z0-9 or an operation
(defun get-char-class (char)
    (cond ((alphanumericp char) 0);;For chars and numbers
          (t 1);;For operations
    )
)

;;Checks lexeme is all consist of chars and digits or not
(defun check-alphanumeric (lexeme)
    (cond ((null lexeme) t)
          ((not (alphanumericp (car lexeme))) nil) 
          (t (check-alphanumeric (cdr lexeme)))
    )
)

;;this functions checks that given lexeme is a value which means only consist of numbers or not
(defun check-value (lexeme)
    (cond ((null lexeme) t)
          ((not (digit-char-p (car lexeme))) nil) 
          (t (check-value(cdr lexeme)))
    )
)

;;This functions checks given lexeme can be identifier, keyword or cannot be tokenized.
;;If lexeme has leading digit this means that this lexeme cannot be tokenized. If this is not the case we can search this lexeme in keywords to find
;;is there any correspondence for given lexeme. If we have a match this means that we have keyword and we print that. Otherwise this is a identifier and
;;we can categorize that lexeme as an identifier.
(defun check-identifier (lexeme)
    (cond ((digit-char-p (car lexeme)) (progn (format t "~%Syntax Error ~{~a~} cannot be tokenized~%" lexeme) -1))
          (t (if (not (eq (position (coerce lexeme 'string) KW_LIST :test #'string=) nil)) (push (list (nth (position (coerce lexeme 'string) KW_LIST :test #'string=) KW_LIST_COR) (format nil "~{~a~}" lexeme)) *current*) (push (list "IDENTIFIER" (format nil "~{~a~}" lexeme)) *current*)))
    )
)

;;Check lexeme is a function that decides alphanumeric lexeme is proper for given grammar rules.
;;If there is 0 in start of lexeme this is not proper for both of Identifier and value but if there only one zero as lexeme this is a value.
;;The last condition firstly check that if given lexeme all consist of numbers this means that we have a value but if there is a one character this means that this lexeme possibly can be a identifier. We need to determine that
(defun check-lexeme (lexeme)
    (cond ((and (not (eq (list-length lexeme) 1)) (char= #\0 (car lexeme))) (progn (format t "~%Syntax Error ~{~a~} cannot be tokenized~%" lexeme) -1))
          ((not (check-alphanumeric lexeme)) (progn (format t "~%Syntax Error ~{~a~} cannot be tokenized~%" lexeme) -1))
          ((and (eq (list-length lexeme) 1) (char= #\0 (car lexeme))) (push (list "VALUE" (format nil "~{~a~}" lexeme)) *current*))
          (t (if (not (check-value lexeme)) (check-identifier lexeme) (push (list "VALUE" (format nil "~{~a~}" lexeme)) *current*)))
    )
)

;;Reads until finding closing quote
(defun read-quote (inputStr)
    (if (eq #\" (read-char inputStr nil)) (push (list "OP" "OP_CC") *current*) (read-quote inputStr))
)


;;This is main function.
;;We can have filename as a input or not. For that we have a optional input.
;;After that if this is a file. We read line by line until we reach its end.
;;From there process is same for both case we have input stream.
;;We read stream line by line. After that read char by char. We determine char's class.
;;If we encounter ;; this is a comment. We must read until line ends.
;;If we have operation we pass it to function and print that operation.
;;If we have " this means we must read this section until finding closing mark.
;;If we have lexeme first we must read it until we find any operation or whitespace. From there we can determine that lexeme is proper or not with above functions.
(defun gpplexer(expression)
    (setq *current* nil)
    (let ((str (make-string-input-stream expression)))
        (loop while (not (eq (peek-char nil str nil) nil)) do
            (let ((c (read-char str)) (lexeme))
                (cond ((and (char= #\; c) (char= (peek-char nil str nil) #\;)) (progn (push (list "COMMENT" "COMMENT") *current*) (return )))
                    ((eq (get-char-class c) 0) (progn (push c lexeme) (if (eq -1 (check-lexeme (get-alphabetics str lexeme))) (return ))))
                    ((eq c #\") (progn (push (list "OP" "OP_OC") *current*) (read-quote str)))
                    ((eq (get-char-class c) 1) (if (and (string= "*" (peek-char nil str nil)) (string= c "*")) (operation-control (concatenate 'string (string c) (string (read-char str)))) (operation-control c)))
                )
            )   
        )
    )
    (reverse *current*)
)

;;For given operation determines operation type and print out
(defun operation-control (operation)
    (cond ((string= operation "+")(push (list "OP" "OP_PLUS") *current*))
          ((string= operation "-")(push (list "OP" "OP_MINUS") *current*))
          ((string= operation "/")(push (list "OP" "OP_DIV") *current*))
          ((string= operation "*")(push (list "OP" "OP_MULT") *current*))
          ((string= operation "(")(push (list "OP" "OP_OP") *current*))
          ((string= operation ")")(push (list "OP" "OP_CP") *current*))
          ((string= operation "**")(push (list "OP" "OP_DBLMULT") *current*))
          ((string= operation ",")(push (list "OP" "OP_COMMA") *current*))
          ((string= operation "\"")(push (list "OP" "OP_OC") *current*))
          ((string= operation "\"")(push (list "OP" "OP_CC") *current*))
          ((string= operation "'")(push (list "OP" "OP_QUO") *current*))
    )
)



