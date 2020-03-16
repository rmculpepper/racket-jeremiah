#lang racket/base
(require web-server/dispatchers/dispatch
         web-server/servlet-env
         racket/file
         net/url
         "config.rkt")
(provide preview)

(define PORT 3500)

(define (preview #:dir [dir (get-dest-dir)]
                 #:url [url (get-base-url)])
  (ensure-external-browser-preference)
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path (url->string (local-url url))
                 #:extra-files-paths (list dir)
                 #:port PORT
                 #:listen-ip #f
                 #:launch-browser? #t))

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
