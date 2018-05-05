#lang racket
(provide add-active-token def-active-token process-string)
(require dyoo-while-loop)

;List that saves the tokens that were defined
;stores the pairs <token,function>
(define tokens '())


#|add-active-token
takes a string describing an active token and a function that should be triggered when that token is found.
This last function takes a string and returns a string.
This function stores the association between the token and the corresponding function.|#
(define (add-active-token token f)
  (set! tokens (append tokens (list (list token f)))))   ;stores the function
  


#|process-string
takes a string and returns a string and is responsible for triggering the active token actions,
transforming the string until no more active tokens are found.
To this end, whenever an active token is found, the associated function is called with the substring that starts immediately after the token,
and the result of that function replaces the substring that starts with the token.|#
(define (process-string str)
  (for ([entry tokens])                                                   ;for each defined token
    (let ([token (car entry)]
          [f (cadr entry)])
      (define token-match (regexp-match-positions token str))             ;tries to find the token in the string
      (while (not (equal? token-match #f))                                ;while there is a match
             (set! str (transform-string str f token-match))              ;transforms the string
             (set! token-match (regexp-match-positions token str)))))     ;tries to find match in new string
  str)


;with the token match in the string, calls the function f on the string after the token,
;then appends the string before the token with the result of the function
(define (transform-string str f token-match)
  (define pos-token (caar token-match))
  (define pos-after-token (cdar token-match))
  (define pre-string (substring str 0 pos-token))
  (define pos-string (substring str pos-after-token))
  (define result-pos-string (f pos-string))
  (string-append pre-string result-pos-string)
)


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
  (let* ([initial (cdar (regexp-match-positions #px"[[:space:]]+[[:word:]]+[[:space:]]*=[[:space:]]*new" str))]
        [end (- (cdar (regexp-match-positions #px"[[:space:]]+[[:word:]]+[[:space:]]*=[[:space:]]*new[[:space:]]+\\S+[(][)][;]" str)) 3)]
        [type (string-trim(substring str initial end))])
    (string-append type str)))

;String Interpolation
(def-active-token "#" (str)
  ;do stuff
  "b"
  )

;Type Aliases
(def-active-token "alias" (str)
  ;do stuff
  "c"
  )