#lang racket/base
(require racket/runtime-path
         jeremiah/config)

;; TODOs and FIXMEs
;; - pagination!
;; - use top bar instead of side bar? (as alternative?)
;; - fix display of feed link
;; - sidebar color and link color don't quite match

(define-runtime-path purecss-dir "static")
(pre-static-src-dirs (cons purecss-dir (pre-static-src-dirs)))

;; ----------------------------------------

(module use-templates racket/base
  (require web-server/templates
           racket/class
           racket/list
           racket/string
           net/url-structs
           jeremiah/config
           jeremiah/util)
  (index-entry-renderer
   (lambda (post)
     (define site (the-site))
     (include-template "index-entry.html")))
  (post-renderer
   (lambda (post)
     (define site (the-site))
     (include-template "post.html")))
  (page-renderer
   (lambda (page content-html)
     (define site (the-site))
     (include-template "page.html"))))

(require (submod "." use-templates))
