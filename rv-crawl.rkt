#lang racket

(require "./webcrawl.rkt"
	 racket/serialize)

(define-serializable-struct entry (title price date url)
  #:transparent)

(define (in-range?/inclusive low high x)
  (and (>= x low)
       (<= x high)))
(define (get-price entry)
  (string->number (second (regexp-match "\\$([0-9]+)"
					(entry-price entry)))))
(define (price-within? price-low-limit price-high-limit)
  (λ (entry)
    (in-range?/inclusive price-low-limit
			 price-high-limit
			 (get-price entry))))

(define (title-matches? what)
  (λ (entry)
    (regexp-match? what (entry-title entry))))



(define base-url "https://boston.craigslist.org/search/sss?query=rv&\
excats=5-15-48-28-1-22-30-13-2-3-5-7-2-9&sort=rel&hasPic=1&searchNearby=1")

(define (find-results base-url)
  (flatten
   (for/list ([page (range 10)])
     (define search-url (string-append base-url
				       "&s="
				       (number->string (* page 120))))
     (define search-results (get search-url))
     (define results-body (fifth search-results))
     (define results-title&prices
       (map-matches `(p (@ (class "result-info")) ,_ ...
			(time (@ (class "result-date") (datetime ,date) ,_ ...) ,_ ...) ,_ ...
			(a (@ (href ,url) ,_ ...) ,title ,_ ...) ,_ ...
			(span (@ (class "result-meta")) ,_ ... (span (@ (class "result-price")) ,price) ,_ ...) ,_ ...)
		    results-body
		    (make-entry title price date url)))

     (filter result-filter
	     results-title&prices))))

(define (pretty-print results)
  (for ([result results])
    (printf "~a\n~a\n~a\n~a\n\n\n" (entry-title result)
	    (entry-price result)
	    (entry-date result)
	    (entry-url result))))

(define (load-prev-results)
  (with-handlers ([exn? (λ (e) empty)])
    (map deserialize (call-with-input-file "seen.dat" read))))

(define (save-results results)
  (call-with-output-file "seen.dat" (curry write (map serialize results))
			 #:exists 'truncate))

(define (result=? a b)
  (and (string=? (entry-title a) (entry-title b))
       (string=? (entry-price a) (entry-price b))))

(define result-filter
  (and/c (price-within? 500 15000)
	 (or/c (title-matches? "(?i:rv)")
	       (title-matches? "(?i:trailer)")
	       (title-matches? "(?i:home)")
	       (title-matches? "(?i:camper)"))
	 (not/c (or/c (title-matches? "(?i:enclosed)")
		      (title-matches? "(?i:cargo)")
		      (title-matches? "(?i:fifth wheel)")
		      (title-matches? "(?i:5th wheel)")
		      (title-matches? "(?i:drings)")
		      (title-matches? "(?i:trade.*for.*your.*rv)")
		      (title-matches? "(?i:scooter)")
		      (title-matches? "(?i:dump trailer)")
		      (title-matches? "(?i:equipment trailer)")
		      (title-matches? "(?i:rv cam)")))))


(printf "Crawling for new results...\n\n")

(define current-results (find-results base-url))
(define prev-results (load-prev-results))
(define new-results (remove* prev-results
			     current-results
			     result=?))
(printf "Found these new results:\n\n")
(pretty-print new-results)
(printf "\nSaving new results...\n")
(save-results (append prev-results new-results))
(printf "Done.\n")
