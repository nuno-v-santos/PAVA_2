#lang racket

(cadr '(1 2 3)) ; 2

(define x 2)  ; x=2

(let ([a 0]
      [b 1])
  (+ a b))  ; 0+1=1

(let* ([a 0]
      [b a])
  (+ a b))   ;0+0=0

;(define (func_name arg1 arg2 arg3) body)

(define (double x)
  (* 2 x))

(double 1) ;=2

((lambda (x)
  (* 2 x)) 1)  ;return 2

(map (lambda (x) (* 2 x))
     '(1 2 3))      ;return '(2 4 6)

(struct document
  (author
   title))

(define doc (document "Nuno" "Racket is cool *not*"))

(document-author doc)  ;get author of struct doc -> returns Nuno ofc



;Readable Regular Expressions
;Printable Regular Expressions
;regexp -> #rx
;pregexp -> #px

(regexp "ap*le")
;#rx"ap*le"

;regexp-match & regexp-match*
(regexp-match #rx"a|b" "cat")
;result -> '("a")

(regexp-match* #rx"a|b" "cata")
;result -> '("a","a")

(regexp-match #rx"a(t|b)" "cat")
; result -> '("at","t")

(regexp-match-positions #rx"a|b" "cata")
;result -> '((1 2))

(regexp-replace #rx"a|b" "cata" "u")
;result -> "cuta"

(regexp-replace #rx"a|b" "cata" string-upcase)
;result -> "cAta"



;Pattern based matching
(match '(1 2 3)
  [(list _ _ a) a])
;result -> 3

(match "yes"
  ["no" #f]
  ["yes" #t])
;return #t

(match '(1 2 3)
  [(list a b c)(list c b a)])
;return '(3 2 1)




(define (bar f)
  (f 2))

;(define-syntax-rule
;       pattern
;       template

(define-syntax-rule
  (foo (var) body)
  (bar (lambda (var) body)))

(foo (y)
     (+ 1 y))

;racket will use the syntax rule and will transform to
(bar (lambda (y) (+ 1 y)))  ;result -> 3

(define (test-str)
  (define str #<<end
 Cache = bache;
public class Foo {
	public static Cache mergeCaches(Cache a, Cache b) {
		Cache vary = new Cache();
	}
}
end
)
  (displayln (regexp-match #px"^[[:space:]]+([[:word:]]+)[[:space:]]*=[[:space:]]*([^;]+);" str)))