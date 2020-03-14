#lang racket/base
(require racket/runtime-path
         racket/string
         racket/class
         jeremiah/config
         (only-in jeremiah/private/post xexpr->html)
         net/url
         web-server/templates)

(define-runtime-path root ".")
(root-dir root)
(base-url (string->url "https://mysite.com/"))

(tag-uri-entity "me@mysite.com,2020")
(tag-uri-prefix "blog")

(page-renderer
 (lambda (page content-html site)
   (include-template "page-template.html")))

(post-renderer
 (lambda (post prev-post next-post)
   (include-template "post-template.html")))

(index-entry-renderer
 (lambda (post)
   (include-template "index-entry-template.html")))
