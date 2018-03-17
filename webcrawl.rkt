#lang racket

(require net/url
         (planet neil/html-parsing:3:0))

(provide get
	 map-matches)

;;;;;;;;;; Configurables ;;;;;;;;;;
(define price-low-limit 1000)
(define price-high-limit 15000)


(define (get url-string)
  (html->xexp (get-pure-port (string->url url-string))))

(define-syntax-rule (matches? template x)
  (match x
    (template #t)
    (_ #f)))
(matches? (cons (== 1) _) '(1 3))

;; map-matches: match-pat? list? thunk/expr -> (listof any)
;;
;; Match the pattern TEMPLATE to elements in LST at any depth or
;; position, and for each match evaluate EXPR when the matched pattern
;; variables are still bound for a result.
;;
;; TEMPLATE is a pattern for use by `match', which see.
;;
;; EXPR may have the special value 'identity to just return the
;; entire match.
(define-syntax-rule (map-matches template lst expr)
  (let map-more-matches ([in lst]
                          [matches empty])
    (match in
      ((cons head tail) #:when (matches? template head)
       (map-more-matches tail
                         (cons
                          (if (equal? 'expr ''identity)
                              head
                              (match head
                                (template expr)))
                          matches)))

      ((cons head tail)
       (append matches
               (map-more-matches head empty)
               (map-more-matches tail empty)))

      (empty/not-a-list
       matches))))
