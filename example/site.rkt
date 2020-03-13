#lang racket/base
(require racket/runtime-path
         racket/string
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

(index-entry-renderer
 (lambda (post)
   (include-template "_posts/_index-entry-template.html")))

(index-renderer
 (lambda (page rel-prev rel-next)
   (include-template "_posts/_index-template.html")))
