#lang racket/base
(require racket/runtime-path
         racket/class
         jeremiah/config
         net/url
         web-server/templates)

(define-runtime-path root ".")
(root-dir root)
(base-url (string->url "https://mysite.com/blog/"))

(tag-uri-entity "ryanc@racket-lang.org,2020")
(tag-uri-prefix "blog")

(post-renderer
 (lambda (post prev-post next-post)
   (include-template "_posts/_post-template.html")))
