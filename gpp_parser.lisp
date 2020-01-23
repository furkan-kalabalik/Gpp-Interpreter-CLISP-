;;Loads gpp_lexer.lisp to get token list of given input.
(load "gpp_lexer.lisp")

;;REMINDERS
;;(defconstant EXPI '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA" "IDENTIFIER" "VALUE"))
;;(defconstant EXPB '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_TRUE" "KW_FALSE"))
;;(defconstant EXPLISTI '("KW_CONCAT" "KW_APPEND"))
;;(defconstant NONTERM '("START" "INPUT" "EXPLISTI" "EXPI" "EXPB"))

(defvar num nil);;Global num for printing terminals and non-terminals in tree manner
(defvar tokens nil);;Main token list for store tokens that comes from gpp_lexer
(defvar syntax-flag nil);;Indicate syntax error
(defvar symbol-flag nil);;Indicate symbol error
(defvar operation-stack nil);;Operation stack for evaluation
(defvar symbol-table nil);;Symbol table that consist of tuples that consist of name and value
;;Starting values for testing
(push (list "ali" 3) symbol-table)
(push (list "veli" 3) symbol-table)
(push (list "deneme" 5) symbol-table)
(push (list "merve" 6) symbol-table)
(push (list "furkan" 10) symbol-table)


;;Decides input is expi or explist with respect to tokens
(defun start-parsing ()
    (cond ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_PLUS")) (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MINUS"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MULT"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_DIV"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "set"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "deffun"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "if"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "for"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "while"))  (expi-handler ))
          ((and (string= (firstToken tokens) "IDENTIFIER") (eq nil (cdr tokens)))  (expi-handler))
          ((string= (firstToken tokens) "VALUE")  (expi-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondToken tokens) "IDENTIFIER"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "defvar"))  (expi-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "concat"))  (explist-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "append"))  (explist-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "list"))  (explist-handler ))
          ((and (string= (firstLexeme tokens) "OP_QUO") (string= (secondLexeme tokens) "OP_OP"))  (explist-handler ))
          ((string= (firstLexeme tokens) "nil")  (explist-handler))
          ((string= (firstLexeme tokens) "COMMENT") (progn (setf tokens (cdr tokens)) (setf syntax-flag 0) (setf symbol-flag 1)))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "exit"))  (exit-handler ))
          (t (setf syntax-flag 1))
    )
)

;;While parsing we generate a operation stack for evaluation
(defun start-eval ()
    (cond 
        ((string= (car operation-stack) "EXPI") (expi-eval))
        ((string= (car operation-stack) "EXPLISTI") (explisti-eval))
        ((string= (car operation-stack) "EXBI") (exbi-eval))
    )
)

;;Exits when user print (exit)
(defun exit-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_EXIT~%")
    (push "OP_PLUS" operation-stack)
    (setf tokens (restOf tokens))
    (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens)) (cl-user::quit)) 
        (setf syntax-flag 1) )
)

;;After decide expression is expi we decide which expi currently we handling in parsing manner and send expression to related function handler
(defun expi-handler ()
    (print-line num)
    (format t "EXPI~%")
    (push "EXPI" operation-stack)
    (incf num)
    (cond ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_PLUS")) (plus-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MINUS"))  (minus-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MULT"))  (mult-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_DIV"))  (div-handler))
          ((string= (firstToken tokens) "IDENTIFIER")  (ident-handler))
          ((string= (firstToken tokens) "VALUE")  (value-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondToken tokens) "IDENTIFIER"))  (func-handler))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "set"))  (set-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "defvar"))  (defvar-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "deffun"))  (deffun-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "if"))  (if-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "for"))  (for-handler ))
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "while"))  (while-handler ))
    )
)

;;Expi eval evaluates expi expressions with inputs from operation stack
(defun expi-eval ()
    (setf operation-stack (cdr operation-stack));;Get rid of expi we decided
    (cond 
        ((string= (car operation-stack) "OP_PLUS") (plus-eval ))
        ((string= (car operation-stack) "OP_MINUS") (minus-eval ))
        ((string= (car operation-stack) "OP_MULT") (mult-eval ))
        ((string= (car operation-stack) "OP_DIV") (div-eval ))
        ((string= (car operation-stack) "VALUE")  (value-eval ))
        ((and (string= (car (cdr operation-stack) )"IDENTIFIER") (string= (car (cdr (cdr (cdr operation-stack)))) "EXPLISTI"))  (func-eval))
        ((string= (car operation-stack) "IDENTIFIER") (ident-eval ))
        ((string= (car operation-stack) "KW_SET") (set-eval ))
        ((string= (car operation-stack) "KW_DEFVAR") (defvar-eval ))
        ((string= (car operation-stack) "KW_DEFFUN") (deffun-eval ))
        ((string= (car operation-stack) "KW_IF") (if-eval ))
        ((string= (car operation-stack) "KW_WHILE") (while-eval ))
        ((string= (car operation-stack) "KW_FOR") (for-eval ))
    )
)

;;Parse plus operation and its operands
(defun plus-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "OP_PLUS~%")
    (push "OP_PLUS" operation-stack);;For operation stack
    (setf tokens (restOf tokens))
    (expi-handler)
    (expi-handler)
    (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) 
        (setf syntax-flag 1) )
)

;;Evaluates plus with getting its operands with evaluation
(defun plus-eval ()
    (setf operation-stack (cdr operation-stack))
    (+ (start-eval ) (start-eval ))
)

;;Parse minus operation and its operands
(defun minus-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "OP_MINUS~%")
    (push "OP_MINUS" operation-stack);;For operation stack
    (setf tokens (restOf tokens))
    (expi-handler)
    (expi-handler)
    (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) 
        (setf syntax-flag 1) )
)

;;Evaluates minus with getting its operands with evaluation
(defun minus-eval ()
    (setf operation-stack (cdr operation-stack))
    (- (start-eval ) (start-eval ))
)

;;Parse multiplication operation and its operands
(defun mult-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "OP_MULT~%")
    (push "OP_MULT" operation-stack);;For operation stack
    (setf tokens (restOf tokens))
    (expi-handler)
    (expi-handler)
     (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) 
        (setf syntax-flag 1) )
)

;;Evaluates multiplication with getting its operands with evaluation
(defun mult-eval ()
    (setf operation-stack (cdr operation-stack))
    (* (start-eval ) (start-eval ))
)

;;Parse multiplication operation and its operands
(defun div-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "OP_DIV~%")
    (push "OP_DIV" operation-stack);;For operation stack
    (setf tokens (restOf tokens))
    (expi-handler)
    (expi-handler)
     (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) 
        (setf syntax-flag 1) )
)

;;Evaluates divison with getting its operands with evaluation
(defun div-eval ()
    (setf operation-stack (cdr operation-stack))
    (floor (start-eval ) (start-eval ))
)

;;handle a single identifier and push it to operation stack with its name
(defun ident-handler ()
    (print-line num)
    (format t "IDENTIFIER~%")
    (push "IDENTIFIER" operation-stack)
    (print-line (+ 1 num))
    (format t "~A~%" (firstLexeme tokens))
    (push (firstLexeme tokens) operation-stack)
    (decf num)
    (setf tokens (cdr tokens))
)

;;Try to get identifier from symbol table
(defun ident-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((ident (car operation-stack)))
        (setf operation-stack (cdr operation-stack))
        (get-ident ident symbol-table)
    )
)

;;Search in symbol table for given symbol. If it don't find symbol returns 0 and makes symbol-flag 1 to indicate that case.
(defun get-ident (ident symbols)
    (if (not (eq nil symbols))
        (if (string= (car (car symbols)) ident) (car (cdr (car symbols))) (get-ident ident (cdr symbols)))
        (progn (setf symbol-flag 1) (+ 0 0))
    )
)

;;Handle a single value with pushing operation-stack
(defun value-handler ()
    (print-line num)
    (format t "VALUE~%")
    (push "VALUE" operation-stack)
    (print-line (+ 1 num))
    (format t "~A~%" (firstLexeme tokens))
    (push (firstLexeme tokens) operation-stack)
    (decf num)
    (setf tokens (cdr tokens))
)

;;Parses integer and returns as a number
(defun value-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((val (parse-integer (car operation-stack))))
        (setf operation-stack (cdr operation-stack))
        val
    )
)

;;Check the function calling syntax is OK. 
(defun func-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (setf tokens (cdr tokens))
    (expi-handler)
    (explist-handler)
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)


;;Evaluate function call and return 0 as said.
(defun func-eval ()
    (+ 0 0)
)

;;Parses set operation
(defun set-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_SET~%")
    (push "KW_SET" operation-stack)
    (setf tokens (restOf tokens))
    (if (not (string= (firstToken tokens) "IDENTIFIER")) (setf syntax-flag 1))
    (expi-handler )
    (expi-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Evaluates set operation. First it tries to get ident from symbol table. If it can get, this means symbol is in table, so it can be assing a value.
(defun set-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((ident (car (cdr (cdr operation-stack)))))
        (setf operation-stack (cdr (cdr (cdr operation-stack))))
        (get-ident ident symbol-table)
        (if (= symbol-flag 0) (set-value ident (expi-eval ) symbol-table))
    )    
)

;;set value to identifier if it present on table
(defun set-value (ident value symbol)
    (if (string= ident (car (car symbol))) (setf (car (cdr (car symbol))) value)
        (set-value ident value (cdr symbol))
    )
)

;;Handles defvar operation
(defun defvar-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_DEFVAR~%")
    (push "KW_DEFVAR" operation-stack)
    (setf tokens (restOf tokens))
    (if (not (string= (firstToken tokens) "IDENTIFIER")) (setf syntax-flag 1))
    (expi-handler )
    (expi-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Evaluates defvar operation. It tries to get symbol from table. If it can get, symbol is already in table. If it not, then we add new entry to table with value
(defun defvar-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((ident (car (cdr (cdr operation-stack)))))
        (setf operation-stack (cdr (cdr (cdr operation-stack))))
        (get-ident ident symbol-table)
        (if (= 0 symbol-flag) (progn (format t "~A symbol is already in table~%" ident) (setf symbol-flag 1)) (progn (setf symbol-flag 0) (push (list ident (expi-eval)) symbol-table) (car (cdr (car symbol-table))))) 
    )
)


;;Parses deffun operation
(defun deffun-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_DEFFUN~%")
    (push "KW_DEFFUN" operation-stack)
    (setf tokens (restOf tokens))
    (if (not (string= (firstToken tokens) "IDENTIFIER")) (setf syntax-flag 1))
    (expi-handler )
    (if (string= (firstLexeme tokens) "OP_OP") (progn (setf tokens (cdr tokens)) (print-line num) (format t "OP_OP~%")) (setf syntax-flag 1))
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (push "OP_CP" operation-stack)
    (format t "OP_CP~%") (setf tokens (cdr tokens))) (if (string= "IDENTIFIER" (firstToken tokens)) (idents-handler ) (setf syntax-flag 1)))
    (explist-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1)) 
)

;;Returns 0 for deffun
(defun deffun-eval ()
    (+ 0 0)
)

;;Handles IDLIST with parsing identifier one by one
(defun idents-handler ()
    (expi-handler)
    (if (string= "IDENTIFIER" (car (car tokens))) (idents-handler ) 
        (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (format t "OP_CP~%") (push "OP_CP" operation-stack) (setf tokens (cdr tokens))) (setf syntax-flag 1)))
)


;;parse if operation
(defun if-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_IF~%")
    (push "KW_IF" operation-stack)
    (setf tokens (restOf tokens))
    (exbi-handler )
    (explist-handler )
    (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (progn (explist-handler ) (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (push "OP_CP" operation-stack) (setf tokens (cdr tokens))) (setf syntax-flag 1))))
)

;;Gets the bool value of if and makes control. Also check else condition by looking operation stack that if there is operation or not
(defun if-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((bool) (result1) (result2))
        (setf bool (exbi-eval))
        (setf result1 (explisti-eval))
        (if (null operation-stack)(if bool result1)(progn (setf result2 (explisti-eval)) (if bool result1 result2)))
    )
)


;;Parse while operation
(defun while-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_WHILE~%")
    (push "KW_WHILE" operation-stack)
    (setf tokens (restOf tokens))
    (exbi-handler )
    (explist-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Gets the value of exbi of while operation
(defun while-eval ()
    (setf operation-stack (cdr operation-stack))
    (exbi-eval )
)

;;Parses for operation
(defun for-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_FOR~%")
    (push "KW_FOR" operation-stack)
    (setf tokens (restOf tokens))
    (if (string= (firstLexeme tokens) "OP_OP") (progn (print-line num) (format t "OP_OP~%") (setf tokens (cdr tokens)) (if (string= (firstToken tokens) "IDENTIFIER") (progn (expi-handler ) (expi-handler ) (expi-handler ) (if (string= (firstLexeme tokens) "OP_CP") (progn (print-line num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))) (setf syntax-flag 1) ))(setf syntax-flag 1))
    (explist-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Returns 0 for evaluation as said.
(defun for-eval ()
    (+ 0 0)
)


;After decide expression is explist we decide which expi currently we handling in parsing manner and send expression to related function handler
(defun explist-handler ()
    (print-line num)
    (format t "EXPLISTI~%")
    (push "EXPLISTI" operation-stack)
    (incf num)
    (cond 
        ((and (string= (firstLexeme tokens) "OP_QUO") (string= (secondLexeme tokens) "OP_OP"))  (list-handler ))
        ((string= (firstLexeme tokens) "nil")  (progn (print-line num) (setf tokens (cdr tokens)) (format t "KW_NIL~%") (decf num)))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "concat"))  (concat-handler ))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "append"))  (append-handler ))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "list"))  (list-func-handler ))
    )
)


;;Evaluates explist operations and returns values of explist
(defun explisti-eval ()
    (setf operation-stack (cdr operation-stack))
    (cond 
        ((and (string= (car operation-stack) "OP_QUO") (string= (car (cdr operation-stack)) "OP_OP"))  (list-eval ))
        ((and (string= (car operation-stack) "KW_CONCAT"))   (concat-eval ))
        ((and (string= (car operation-stack) "KW_APPEND"))   (append-eval ))
        ((and (string= (car operation-stack) "KW_LIST"))   (list-func-eval ))
        )
)


;;Parses concat operation
(defun concat-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_CONCAT~%")
    (push "KW_CONCAT" operation-stack)
    (setf tokens (restOf tokens))
    (explist-handler )
    (explist-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)


;;Concatane two list with lisp function
(defun concat-eval ()
    (setf operation-stack (cdr operation-stack))
    (concatenate 'list (explisti-eval) (explisti-eval))
)


;;Parses append operation
(defun append-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_APPEND~%")
    (push "KW_APPEND" operation-stack)
    (setf tokens (restOf tokens))
    (expi-handler )
    (explist-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)


;;Append the value to given list
(defun append-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((expi-val (expi-eval))(explisti-val (explisti-eval)))
        (append explisti-val (list expi-val))
    )
    
)

;;Parses list function
(defun list-func-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_LIST~%")
    (push "KW_LIST" operation-stack)
    (setf tokens (restOf tokens))
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (push "OP_CP" operation-stack)
    (format t "OP_CP~%") (setf tokens (cdr tokens))) (if (string= "VALUE" (firstToken tokens)) (values-handler ) (setf syntax-flag 1))) 
    (decf num)
)

;;Fill an empty list with given values to construct list
(defun list-func-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((liste '()))
        (create-list liste)
    )
)

;;Parses list operation.
(defun list-handler ()
    (print-line num)
    (format t "OP_QUO~%")
    (push "OP_QUO" operation-stack)
    (print-line num)
    (format t "OP_OP~%")
    (push "OP_OP" operation-stack)
    (setf tokens (restOf tokens))
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (push "OP_CP" operation-stack)
    (format t "OP_CP~%") (setf tokens (cdr tokens))) (if (string= "VALUE" (firstToken tokens)) (values-handler ) (setf syntax-flag 1))) 
    (decf num)
)

;;Fill an empty list with given values to construct list
(defun list-eval ()
    (setf operation-stack (cdr (cdr operation-stack)))
    (let ((liste '()))
        (create-list liste)
    )
)

;;Evaluates given values and push one by one to list. At the end reverse list and returns
(defun create-list (liste)
    (if (string= (car operation-stack) "EXPI")
        (progn (push (expi-eval ) liste) (create-list liste))
        (progn (setf operation-stack (cdr operation-stack)) (reverse liste))
    )
)

;;For list handling evaluates values one by one and add to operation-stack 
(defun values-handler ()
    (expi-handler)
    (if (string= "VALUE" (car (car tokens))) (values-handler ) 
        (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (format t "OP_CP~%") (push "OP_CP" operation-stack)  (setf tokens (cdr tokens))) (setf syntax-flag 1)))
)

;;Handles exbi-operations and decides which one is.
(defun exbi-handler ()
    (print-line num)
    (format t "EXBI~%")
    (push "EXBI" operation-stack)
    (incf num)
    (cond 
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "and"))  (and-handler ))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "or"))  (or-handler ))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "not"))  (not-handler ))
        ((string= (firstLexeme tokens) "true")  (binary-handler))
        ((string= (firstLexeme tokens) "false")  (binary-handler))
        ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "equal"))  (equal-handler ))
    )
)

;;Evaluates exbi-operation by deciding its operation
(defun exbi-eval ()
    (setf operation-stack (cdr operation-stack))
    (cond 
        ((string= (car operation-stack) "KW_AND") (and-eval ))
        ((string= (car operation-stack) "KW_OR") (or-eval ))
        ((string= (car operation-stack) "KW_NOT") (not-eval ))
        ((string= (car operation-stack) "BINARY_VALUE") (binary-eval ))
        ((string= (car operation-stack) "KW_EQUAL")  (equal-eval ))
    )
)

;;Parses and function
(defun and-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_AND~%")
    (push "KW_AND" operation-stack)
    (setf tokens (restOf tokens))
    (exbi-handler )
    (exbi-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Retrieves exbi operands of "and" and "and" them
(defun and-eval ()
    (setf operation-stack (cdr operation-stack))
    (and (start-eval ) (start-eval ))
)

;;Parses or function
(defun or-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_OR~%")
    (push "KW_OR" operation-stack)
    (setf tokens (restOf tokens))
    (exbi-handler )
    (exbi-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Retrieves exbi operands of "or" and "or" them
(defun or-eval ()
    (setf operation-stack (cdr operation-stack))
    (or (start-eval ) (start-eval ))
)

;;Parses not function
(defun not-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_NOT~%")
    (push "KW_NOT" operation-stack)
    (setf tokens (restOf tokens))
    (exbi-handler )
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Retrieves exbi operand of "not" and "not" that operand
(defun not-eval ()
    (setf operation-stack (cdr operation-stack))
    (not (start-eval ))
)

;;Handles binary values
(defun binary-handler ()
    (print-line num)
    (format t "BINARY_VALUE~%")
    (push "BINARY_VALUE" operation-stack)
    (print-line (+ 1 num))
    (format t "~A~%" (firstLexeme tokens))
    (push (firstLexeme tokens) operation-stack)
    (decf num)
    (setf tokens (cdr tokens))
)

;;With the true or false return corresponding clisp type
(defun binary-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((val (car operation-stack)))
        (setf operation-stack (cdr operation-stack))
        (if (string= val "true") t nil)
    )
)

;;Parses equal function. Check operands is expi or not. If expi, it handles expis if not it handles exbis
(defun equal-handler ()
    (print-line num)
    (format t "OP_OP~%")
    (print-line num)
    (format t "KW_EQUAL~%")
    (push "KW_EQUAL" operation-stack)
    (setf tokens (restOf tokens))
    (if (is-expi) (progn (expi-handler)(expi-handler)) (progn (exbi-handler ) (exbi-handler )))
    (if (string= "OP_CP" (firstLexeme tokens)) (progn (print-line num) (decf num) (format t "OP_CP~%") (push "OP_CP" operation-stack) (setf tokens (cdr tokens))) (setf syntax-flag 1))
)

;;Handles equal operation. With operation stack we know operands expi or expi.
(defun equal-eval ()
    (setf operation-stack (cdr operation-stack))
    (let ((result))
        (setf result (if (string= (car operation-stack) "EXPI") (equal (expi-eval) (expi-eval)) (equal (exbi-eval ) (exbi-eval ))))
        (setf operation-stack (cdr operation-stack))
        result
    )
    
)

;;Decides current tokens is expi
(defun is-expi ()
    (cond ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_PLUS")) t)
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MINUS"))  t)
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_MULT"))  t)
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondLexeme tokens) "OP_DIV"))  t)
          ((string= (firstToken tokens) "IDENTIFIER")  t)
          ((string= (firstToken tokens) "VALUE")  t)
          ((and (string= (firstLexeme tokens) "OP_OP") (string= (secondToken tokens) "IDENTIFIER")) t)
          (t nil)
    )
)

;;Print - for print tree
(defun print-line (n)
    (if (= n 0) nil (progn (format t "-") (print-line (- n 1))))
)

;;Gets the first token of token list
(defun firstToken (tokens)
    (car (car tokens))
)

;;Gets the second token of token list
(defun secondToken (tokens)
    (car (car (cdr tokens)))
)

;;Gets the first lexeme of token list
(defun firstLexeme (tokens)
    (car (cdr (car tokens)))
)
;;Gets the second lexeme of token list
(defun secondLexeme (tokens)
    (car (cdr (car (cdr tokens))))
)

;;Removes first two entry from token list
(defun restOf (tokens)
    (cdr (cdr tokens))
)

;;This is the interpreter. It resets all values, all flags at first. After that it looks for filename. If there is a file
;;first it handle that file, line by line. After that again restart and make normal operation of interpreter.
(defun gppinterpreter (&optional filename)
    (setf operation-stack nil)
    (setf num 0)
    (setf syntax-flag 0)
    (setf symbol-flag 0)
    (setf tokens nil)
    (if (not (eq filename nil))
        (with-open-file (s filename)
            (loop while (peek-char nil s nil) do
                
                    (setf tokens (gpplexer (read-line s)))
                    (start-parsing)
                    (if (not (eq nil tokens)) (setf syntax-flag 1))
                    (if (= syntax-flag 0)
                        (progn (format t "SYNTAX OK~%") (setf operation-stack (reverse operation-stack)) 
                            (let ((result (start-eval )))
                                (if (= symbol-flag 0) (format t "~%Result: ~A~%" result) (format t "Identifier not found on symbol table~%"))
                            )
                        ) 
                        (format t "SYNTAX_ERROR Expression not recognized~%")
                    )
                
            )
        )
    )
    (setf operation-stack nil)
    (setf num 0)
    (setf syntax-flag 0)
    (setf symbol-flag 0)
    (setf tokens nil)
    (format t ">")
    (let ((str (read-line )))
        (setf tokens (gpplexer str))
        (start-parsing)
        (if (not (eq nil tokens)) (setf syntax-flag 1))
        (if (= syntax-flag 0)
            (progn (format t "SYNTAX OK~%") (setf operation-stack (reverse operation-stack)) 
                (let ((result (start-eval )))
                    (if (= symbol-flag 0) (format t "~%Result: ~A~%" result) (format t "Identifier not found on symbol table~%"))
                )
            ) 
            (format t "SYNTAX_ERROR Expression not recognized~%")
        )
    )
)

;;Waits for g++ input and optionally filename.
(defun start-interpreter ()
    (format t "$")
        (with-input-from-string (str (read-line ))
            (read str)
            (let ((filename (read str nil)))
                (if (string= (string-downcase (string filename)) "nil") (loop while t do (gppinterpreter))
                    (progn (gppinterpreter filename) (loop while t do (gppinterpreter)))
                )
            )
        )
)

(start-interpreter)