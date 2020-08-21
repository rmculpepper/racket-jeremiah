#lang racket/base
(require web-server/dispatchers/dispatch
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/http/response-structs
         racket/match
         racket/list
         racket/file
         net/url
         "util.rkt")
(provide preview)

(define PORT 3500)

(define (preview-dispatcher base-url dir)
  (define (trim-final-/ ps)
    (if (and (pair? ps) (equal? (last ps) "")) (drop-right ps 1) ps))
  (define rel-url->path (make-url->path dir))
  (define base-paths (trim-final-/ (map path/param-path (url-path base-url))))
  (define (adjust-url in-url)
    (match-define (url scheme user host port #t pps '() fragment) in-url)
    (define suffix-pps
      (let loop ([base-paths base-paths] [pps pps])
        (cond [(null? base-paths) pps]
              [(null? pps) (next-dispatcher)]
              [(equal? (car base-paths) (path/param-path (car pps)))
               (loop (cdr base-paths) (cdr pps))]
              [else (next-dispatcher)])))
    (define suffix-pps* (if (null? suffix-pps) (list (path/param "" '())) suffix-pps))
    (define new-url (url scheme user host port #t suffix-pps* '() fragment))
    new-url)
  (sequencer:make
   (files:make #:url->path
               (lambda (in-url)
                 (rel-url->path (adjust-url in-url))))
   (lift:make
    (lambda (req)
      (response/full 404 #"Not Found"
                     (current-seconds) TEXT/HTML-MIME-TYPE
                     '() '(#"<html>File not found</html>\n"))))))

(define (preview #:dir dir #:url url)
  (ensure-external-browser-preference)
  (serve/launch/wait (lambda (_) (preview-dispatcher url dir))
                     #:launch-path (url->string (local-url url))
                     #:port PORT
                     #:listen-ip #f))

(define (ensure-external-browser-preference)
  ;; `serve/servlet` uses the `send-url` from `net/sendurl`, which
  ;; (unlike the `send-url` from `external/browser`) doesn't prompt
  ;; the user if no external browser preference is set. This can
  ;; matter on some Linux distributions, e.g. Ubuntu which needs
  ;; xdg-open or sensible-browser, but `net/sendurl` uses neither and
  ;; doesn't even prompt the user. So check for this case here, and if
  ;; no browser preference set yet, ask the user, like the `send-url`
  ;; from `external/browser` would do.
  (when (eq? 'unix (system-type 'os))
    (unless (get-preference 'external-browser)
      ((dynamic-require 'browser/external
                        'update-browser-preference) #f))))
