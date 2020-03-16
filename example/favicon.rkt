#lang racket
(require pict)

(define redfish (standard-fish 16 16 #:color "red" #:direction 'right))
(define bluefish (standard-fish 20 20 #:color "blue" #:direction 'left))

(define icon
  (let* ([p (blank 32 32)]
         [p (lt-superimpose p (inset redfish 2))]
         [p (rb-superimpose p (inset bluefish 2))])
    p))

;; move this static/img
(void (send (pict->bitmap icon) save-file "favicon.png" 'png))
