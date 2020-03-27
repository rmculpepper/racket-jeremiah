#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/path
         racket/date
         racket/hash
         racket/string
         net/url
         markdown
         (only-in xml xml-attribute-encode)
         "config.rkt"
         "data.rkt"
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Site

(define render-site%
  (class site%
    (inherit-field config)
    (super-new)

    ;; Rendering utilities for use by templates, etc

    (define/public (to-html x) (xexpr->html x))
    (define/public (to-attr s) (xml-attribute-encode s))

    (define/public (tags->links-html tags)
      (string-join (map xexpr->html (add-between (tags->links-xexprs tags) ", ")) ""))
    (define/public (tags->links-xexprs tags)
      (map (lambda (t) (tag->link-xexpr t)) tags))

    (define/public (tag->link-html tag)
      (xexpr->html (tag->link-xexpr tag)))
    (define/public (tag->link-xexpr tag)
      `(a ([href ,(send config get-index-main-link tag)]) ,tag))

    (define/public (date->html d)
      (xexpr->html (date->xexpr d)))
    (define/public (date->xexpr d)
      (if d `(time ([datetime ,d]) ,d) `(span)))

    (define/public (get-extra-html location page)
      ((send config get-extra-html) location page))
    ))

;; ============================================================
;; Pages

(define page<%>
  (interface ()
    get-page-type   ;; -> (U 'post 'index ...)

    get-url         ;; may point to a directory with an index.html file
    get-link

    get-page-url    ;; points specificially to the page's HTML file
    get-page-link

    get-title      ;; -> String
    get-keywords   ;; -> String (note, not (Listof String)!)
    get-feed-link  ;; -> String
    get-prev-link  ;; -> String/#f
    get-next-link  ;; -> String/#F

    render-content-html ;; -> String
    ))


;; ============================================================
;; Render index

(define render-index%
  (class index%
    (inherit-field config iname)
    (inherit get-posts
             get-title
             get-feed-dest-file
             get-feed-url
             get-index-main-url
             get-atom-id
             get-updated-8601)
    (super-new)

    ;; ----------------------------------------
    ;; Atom

    ;; References:
    ;; - https://validator.w3.org/feed/docs/atom.html
    ;; - https://tools.ietf.org/html/rfc4287

    ;; get-atom-feed-xexpr : Index -> XExpr
    ;; If tag is #f, feed name is "all" but tag index is main site.
    (define/public (get-atom-feed-xexpr)
      (define subtitle (if (eq? iname 'main) "All posts" (get-title)))
      (define updated (or (get-updated-8601) "N/A"))
      `(feed
        ([xmlns "http://www.w3.org/2005/Atom"]
         [xml:lang "en"])
        (title ([type "text"]) ,(format "~a: ~a" (send config get-site-title) subtitle))
        (author (name ,(send config get-site-author)))
        (link ([rel "self"] [href ,(url->string (get-feed-url))]))
        (link ([rel "alternate"] [href ,(url->string (get-index-main-url))]))
        (id ,(get-atom-id))
        (updated ,updated)
        ,@(for/list ([post (in-list (get-posts))]
                     [_ (in-range (send config get-site-max-feed-items))])
            (send post get-atom-entry-xexpr))))
    ))


;; ============================================================
;; Render index page

(define render-index-page%
  (class* index-page% (page<%>)
    (inherit-field config index page-num num-pages)
    (inherit get-link get-posts)
    (super-new)

    (define/public (get-page-type) 'index)
    (define/public (is-page-type? type) (eq? (get-page-type) type))

    (define/public (render-page-html)
      ((send config get-page-renderer) this))

    (define/public (render-content-html)
      (define rendered-posts
        (for/list ([post (in-list (get-posts))])
          (send post render-index-entry-html)))
      (string-join rendered-posts "\n"))

    (define/public (get-keywords) (or (send index get-tag) ""))
    (define/public (get-prev-link)
      (cond [(zero? page-num) #f] [else (get-link (sub1 page-num))]))
    (define/public (get-next-link)
      (cond [(= page-num (sub1 num-pages)) #f] [else (get-link (add1 page-num))]))
    ))


;; ============================================================
;; Render post

(define render-post%
  (class* post% (page<%>)
    (inherit-field config)
    (inherit get-title
             get-date
             get-date-8601
             get-authors
             get-tags
             get-more?
             get-link
             get-full-link
             get-atom-id
             get-title-xexpr
             get-blurb-xexprs
             get-body-xexprs)
    (super-new)

    (define/public (get-page-type) 'post)
    (define/public (is-page-type? type) (eq? (get-page-type) type))

    (define/public (render-page-html)
      ((send config get-page-renderer) this))

    (define/public (render-content-html)
      ((send config get-post-renderer) this))

    (define/public (render-index-entry-html)
      ((send config get-index-entry-renderer) this))

    (define/public (get-title-html) (xexpr->html (get-title-xexpr)))
    (define/public (get-blurb-html) (xexprs->html (get-blurb-xexprs)))
    (define/public (get-body-html) (xexprs->html (get-body-xexprs)))

    (define/public (get-keywords) (string-join (get-tags) ","))
    (define/public (get-next-link)
      (cond [(get-next-post) => (lambda (p) (send p get-link))] [else #f]))
    (define/public (get-prev-link)
      (cond [(get-prev-post) => (lambda (p) (send p get-link))] [else #f]))

    (define/public (get-prev-post)
      (let ([index (send (the-site) get-index)]) (send index get-prev this)))
    (define/public (get-next-post)
      (let ([index (send (the-site) get-index)]) (send index get-next this)))

    ;; ----------------------------------------
    ;; Atom

    (define/public (get-atom-entry-xexpr)
      `(entry
        (title ([type "text"]) ,(get-title))
        (link ([rel "alternate"] [href ,(get-full-link)]))
        (id ,(get-atom-id))
        (published ,(get-date-8601))
        (updated ,(get-date-8601))
        ,@(map (lambda (author) `(author (name ,author))) (get-authors))
        (content ([type "html"])
                 ,(get-blurb-html)
                 ,(cond [(get-more?)
                         (xexpr->html
                          `(a ([href ,(get-full-link)])
                              (em "More" hellip)))]
                        [else ""]))))
    ))
