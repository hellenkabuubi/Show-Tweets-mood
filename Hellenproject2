#lang racket

(require data-science-master)
(require plot)
(require math)
; read tweets from json file.
(define tweet-read(open-input-file"kabuubi.json"))

;Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet.
(define tweet-read-text (string-normalize-spaces
		   (remove-punctuation
                    (remove-urls
		    (string-downcase (port->string tweet-read ))))))

;extract each unique word and the number of times it occurred.
(define words (document->tokens tweet-read-text #:sort? #t))

;words

(define sentiment (list->sentiment words #:lexicon 'nrc))



;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))


;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

