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

(define base-url "https://chicago.craigslist.org/search/hhh\
?query=edgewater&sort=rel&hasPic=1&max_bedrooms=1&availabilityMode=0\
&sale_date=all+dates")

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
			(time (@ (class "result-date")
				 (datetime ,date)
				 ,_ ...)
			      ,_ ...)
			,_ ...
			(a (@ (href ,url) ,_ ...) ,title ,_ ...)
			,_ ...
			(span (@ (class "result-meta"))
			      ,_ ...
			      (span (@ (class "result-price")) ,price)
			      ,_ ...)
			,_ ...)
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
    (map deserialize (call-with-input-file "seen-apt-edgewater.dat" read))))

(define (save-results results)
  (call-with-output-file "seen-apt-edgewater.dat"
    (curry write (map serialize results))
    #:exists 'truncate))

(define (result=? a b)
  (and (string=? (entry-title a) (entry-title b))
       (string=? (entry-price a) (entry-price b))))

(define result-filter
  (and/c (price-within? 500 1700)
	 (not/c (or/c (title-matches?
		       "SPACIOUS EDGEWATER HOME @ GREAT PRICE - MUST SEE")
		      (title-matches?
		       "NEW CONSTRUCTION APARTMENT( HOME)? IN EDGEWATER\\*\\*\\*")))))


(define result-limit (make-parameter 50))
(command-line
 #:program "apt-crawl"

 #:once-any
 [("-l" "--limit") l
  "Limit the number of results."
  (result-limit (string->number l))])

(printf "Crawling for new results...\n\n")

(define current-results (find-results base-url))
(define prev-results (load-prev-results))
(define new-results (take (remove* prev-results
				   current-results
				   result=?)
			  (result-limit)))
(printf "Found these new results:\n\n")
(pretty-print new-results)
(printf "\nSaving new results...\n")
(save-results (append prev-results new-results))
(printf "Done.\n")

