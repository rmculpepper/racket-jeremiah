#lang racket/base
(require racket/runtime-path
         jeremiah/config
         net/url-structs)

(define-runtime-path root ".")
(root-dir root)
(base-url (string->url "https://mysite.com/"))

(site-author "Me, the Example User")
(site-title "My Blog")

;; Atom feeds and entries are identified using the tag URI scheme
;; (see https://taguri.org/, RFC 4151).
(tag-uri-entity "me@mysite.com,2020")
(tag-uri-prefix "myblog")
