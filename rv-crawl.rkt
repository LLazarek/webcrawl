#lang racket

(require "./webcrawl.rkt")

(define (in-range?/inclusive low high x)
  (and (>= x low)
       (<= x high)))
(define (get-price entry)
  (string->number (second (regexp-match "\\$([0-9]+)"
					(second entry)))))
(define (price-within? price-low-limit price-high-limit)
  (λ (entry)
    (in-range?/inclusive price-low-limit
			 price-high-limit
			 (get-price entry))))

(define (title-matches? what)
  (λ (entry)
    (regexp-match? what (first entry))))



(define base-url "https://sarasota.craigslist.org/search/sss?query=rv&\
excats=5-15-48-28-1-22-30-13-2-3-5-7-2-9&sort=rel&hasPic=1&searchNearby=1")

(define (find-results base-url)
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
		   (list title price url)))

    (filter (and/c (price-within? 1000 15000)
		   (or/c (title-matches? "(?i:vr)")
			 (title-matches? "(?i:trailer)")
			 (title-matches? "(?i:home)")))
	    results-title&prices)))

(define (pretty-print results)
  (for* ([page results]
	      [result page])
    (printf "~a\n~a\n~a\n\n\n" (first result)
	    (second result)
	    (third result))))

(define current-results (find-results base-url))

(printf "Crawling for results...\n\n")
(pretty-print current-results)
