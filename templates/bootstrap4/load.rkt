#lang racket/base

(module use-templates racket/base
  (require web-server/templates
           racket/class
           racket/list
           racket/string
           net/url-structs
           jeremiah/config
           jeremiah/private/xexpr)
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
