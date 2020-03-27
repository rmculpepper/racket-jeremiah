#lang racket/base
(require racket/class
         racket/file
         racket/path
         racket/string
         net/url
         (only-in xml [xexpr->string xml:xexpr->string])
         (only-in racket/sequence in-slice)
         "config.rkt"
         "data.rkt"
         "render.rkt"
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Write Site

(define write-site%
  (class render-site%
    (super-new)
    ))

;; ============================================================
;; Write Index

(define write-index%
  (class render-index%
    (inherit-field config iname)
    (inherit get-posts
             get-feed-dest-file
             get-atom-feed-xexpr)
    (super-new)

    (define/public (write)
      (for ([index-page (in-list (build-pages))])
        (send index-page write))
      (unless (eq? iname 'draft)
        (write-atom-feed)))

    (define/public (build-pages)
      (define posts (get-posts))
      (define posts-per-page (get-posts-per-page))
      (define num-pages (ceiling (/ (length posts) posts-per-page)))
      (for/list ([page-num (in-range num-pages)]
                 [page-posts (in-slice posts-per-page posts)])
        (new write-index-page% (index this) (posts page-posts)
             (page-num page-num) (num-pages num-pages))))

    (define/public (get-posts-per-page)
      (case iname
        [(draft) (add1 (length (get-posts)))]
        [else (send config get-site-posts-per-page)]))

    (define/public (write-atom-feed)
      (define feed-file (get-feed-dest-file))
      (make-parent-directory* feed-file)
      (with-output-to-file feed-file
        #:exists 'replace
        (lambda ()
          (write-string "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
          (write-string (xml:xexpr->string (get-atom-feed-xexpr))))))

    ))

;; ============================================================
;; Write Index Page

(define write-index-page%
  (class render-index-page%
    (inherit get-dest-file
             render-page-html)
    (super-new)

    (define/public (write)
      (define page-html (render-page-html))
      (make-parent-directory* (get-dest-file))
      (with-output-to-file (get-dest-file)
        #:exists 'replace
        (lambda () (write-string page-html))))
    ))

;; ============================================================
;; Write Post

(define write-post%
  (class render-post%
    (inherit get-cachedir
             get-dest-dir
             render-page-html)
    (super-new)

    (define/public (write)
      (make-directory* (get-dest-dir))
      (define page-html (render-page-html))
      (with-output-to-file (build-path (get-dest-dir) "index.html")
        #:exists 'replace
        (lambda () (write-string page-html)))
      (parameterize ((current-directory (get-cachedir)))
        (for ([file (in-list (find-files file-exists?))]
              #:when (not (dont-copy-file? file)))
          (define dest-file (build-path (get-dest-dir) file))
          (when (file-exists? dest-file) (delete-file dest-file))
          (copy-file file dest-file))))

    (define/public (dont-copy-file? path)
      (regexp-match? #rx"^_" (path->string (file-name-from-path path))))
    ))
