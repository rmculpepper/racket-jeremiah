#lang racket/base
(require jeremiah/config
         racket/runtime-path
         net/url)

(define-runtime-path root ".")
(root-dir root)
(base-url (string->url "https://mysite.com/blog/"))
