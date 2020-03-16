#lang racket/base
(require racket/runtime-path
         jeremiah/config
         net/url)

;; Use the PureCSS (purecss.io) template. There's also a Bootstrap 4 template,
;; or you can copy either template directory to your own project and modify it.
(require jeremiah/templates/purecss/load)

;; Site author and title.
(site-author "Jeremiah B. Frog")
(site-title "Joy to the Fishes")

;; Declare the base path, which determines source and output directories.
;; - $base/static -- files to be copied to output dir without processing
;; - $base/posts -- contains post sources
;; - $base/cache -- cache of built posts
;; - $base/build -- output directory
(define-runtime-path here ".")
(base-dir here)

;; Declare the base URL. The URL components are used as follows:
;; - scheme, host - usually omitted from links, but used in Atom feed, etc
;; - path - included in most links
;; NOTE: preview currently fails if the url has a nonempty path. (FIXME)
;;(base-url (string->url "https://mysite.example.com/myblog/"))
(base-url (string->url "https://mysite.example.com/"))

;; Just so we can demo pagination without added lots of useless posts.
(site-posts-per-page 5)

;; Atom feeds and entries are identified using the tag URI scheme
;; (see https://taguri.org/, RFC 4151).
;; - The "tagging entity" consists of an "authority" and a "date".
;; - The authority should be a domain name or email address that you control
;;   on the given date.
;; - If you (might) use the same entity for different purposes, then choose a
;;   prefix to distinguish them. For example, "myblog" vs "my-other-site" vs
;;   "memos:2012-04".
;; - Once you start using an entity for a particular purpose, do *not* change
;;   it. In particular, do *not* update the entity date every year. It
;;   represents when this tag URI prefix was allocated, not the year a
;;   particular URI was created. Changing the entity date changes previously
;;   allocated feed/post IDs, which breaks your Atom feed.
(tag-uri-entity "me@example.com,2020")
(tag-uri-prefix "myblog")
