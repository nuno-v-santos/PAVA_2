#lang racket
(provide add-active-token def-active-token process-string)

;List that saves the tokens that were defined
;(now only stores the functions, should store the pairs <token,function>)
(define tokens '())


#|add-active-token
takes a string describing an active token and a function that should be triggered when that token is found.
This last function takes a string and returns a string.
This function stores the association between the token and the corresponding function.|#
(define (add-active-token str f)
  (set! tokens (append tokens (list f)))   ;stores the function
  (displayln tokens))


#|process-string
takes a string and returns a string and is responsible for triggering the active token actions,
transforming the string until no more active tokens are found.
To this end, whenever an active token is found, the associated function is called with the substring that starts immediately after the token,
and the result of that function replaces the substring that starts with the token.|#
(define (process-string str)
  (for ([token-saved tokens])
    (displayln (token-saved str))))      ;should do a for loop for each token that was defined, then an if or something to try to apply it


#|def-active-token
takes an active token, a parameter list, and a body
expands into an appropriate use of the function add-active-token.|#
(define-syntax-rule
  (def-active-token token (var) body)
  (add-active-token token (lambda (var) body)))



;some tokens from project description
(def-active-token ";;" (str)
  (or (for/or ((c (in-string str))
               (i (in-naturals)))
        (and (char=? c #\newline)
             (substring str (+ i 1))))
      ""))

#|(define ns (make-base-namespace))
(def-active-token "//eval " (str)
  (call-with-input-string
   str
   (lambda (in)
     (string-append (~a (eval (read in) ns))
                    (port->string in)))))|#


