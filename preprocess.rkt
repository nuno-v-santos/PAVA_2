#lang racket
(provide add-active-token def-active-token process-string)

;List that saves the tokens that were defined
;stores the pairs <token,function>
(define tokens '())

#|def-active-token
takes an active token, a parameter list, and a body
expands into an appropriate use of the function add-active-token.|#
(define-syntax-rule
  (def-active-token token (var) body)
  (add-active-token token (lambda (var) body)))


#|add-active-token
takes a string describing an active token and a function that should be triggered when that token is found.
This last function takes a string and returns a string.
This function stores the association between the token and the corresponding function.|#
(define (add-active-token token f)
  (set! tokens (append tokens (list (list token f)))))   ;stores the associaton <token,function>


#|process-string
takes a string and returns a string and is responsible for triggering the active token actions,
transforming the string until no more active tokens are found.
To this end, whenever an active token is found, the associated function is called with the substring that starts immediately after the token,
and the result of that function replaces the substring that starts with the token.|#
(define (process-string str)
  (define old-str str)       ;saves string (to check in the end if it was modified)
  (for ([entry tokens])      ;for each defined token
    (let*([token (car entry)]
          [f (cadr entry)]
          [token-match (regexp-match-positions token str)])  ;tries to find first ocurrence of the token
      (if (equal? token-match #f)  ;if no match found
          (display "")
          (set! str (transform-string str f token-match)) ;transforms the string    
      )
    )
  )
  
  (if (equal? old-str str)   ;if after going for all tokens the string remains the same
      str                   ;then return the string
      (process-string str)) ;else does everything again
)

;with the token match in the string, calls the function f on the string after the token,
;then appends the string before the token with the result of the function
(define (transform-string str f token-match)
  (let* ([pos-token (caar token-match)]               ;position of the first char of the token
         [pos-after-token(cdar token-match)]          ;position of the first position after the token
         [pre-string (substring str 0 pos-token)]     ;string before the token
         [pos-string (substring str pos-after-token)] ;string after the token
         [result-pos-string (f pos-string)])          ;calls function on the string after the token
    (string-append pre-string result-pos-string)      ;returns string before the token + return of function
  )
)



;some tokens from project description
(def-active-token ";;" (str)
  (or (for/or ((c (in-string str))
               (i (in-naturals)))
        (and (char=? c #\newline)
             (substring str (+ i 1))))
      ""))

(define ns (make-base-namespace))
(def-active-token "//eval " (str)
  (call-with-input-string
   str
   (lambda (in)
     (string-append (~a (eval (read in) ns))
                    (port->string in)))))



;tokens that we need to define

;Local Type Inference
(def-active-token "var" (str)
                               ; var                  nameeeeeee                   =            new            (type)             (  args             )              ;
  (let* ([type (regexp-replace #px"^[[:space:]]+[[:alpha:]_][[:word:]]*[[:space:]]*=[[:space:]]*new[[:space:]]+(\\S+)[[:space:]]*[(][[:word:],\"() ]*[)][[:space:]]*[;].*" str "\\1")])
    (if (equal? type str)          ;if didn't found match
        (string-append "var" str)  ;return "var" + the input string 
        (string-append type str)   ;else type + the input string
    )
  )
)


;String Interpolation
(def-active-token "#" (str)
                      ;#{expWithout"}"}          \"+exp+\" 
  (regexp-replace* #px"#[{]([^\\}]+)[}]" str "\" + (\\1) + \"")
)


;Type Aliases
(def-active-token "alias" (str)
                     ;              (left-operand)           =            (right-operand);
  (let* ([regexExpr #px"[[:space:]]+([[:word:]]+)[[:space:]]*=[[:space:]]*([^;]+);"]
         [alias-op (regexp-match regexExpr str)]
         [left-op (cadr alias-op)]
         [right-op (caddr alias-op)])
    (set! str (regexp-replace regexExpr str ""))     ;removes the line of the alias definition
    (set! str (string-replace str left-op right-op)) ;replaces the first ocurrence of left-operand with right operand
    str
  )
)


(define (test-str)
  (displayln (process-string #<<end
public class Foo {
    public static void main(String[] args) {
		var a= new G();
		var b =new F();
		var c=new E();
		var 	 d= 	new D();
		var e = 
				new C();
		var 
			f = new B(); var 
			g = new A()		;
    }
}
end
)))