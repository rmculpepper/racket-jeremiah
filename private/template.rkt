#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/function
         web-server/templates)
(provide render-template)

;; Beware, cargo cults be here!
;;
;; The idea here is that we make some variables visible in the
;; namespace in which the template is evaluated. Some of these are the
;; variables and values explicitly passed to us in `dict`. The others
;; are from the modules web-server/templates and frog/widgets.
;;
;; Now, web-server/templates are normally used "statically" --
;; compiled into the web-server application. However it's also
;; possible to use them "dynamically" -- to load and use one at run
;; time.
;;
;; I wish I had a crisper understanding how and why the following
;; works, but it does, following this Racket mailing list thread:
;; http://www.mail-archive.com/users@racket-lang.org/msg18108.html

;; The modules needed by the template. Note that these must
;; be required above normally in this template.rkt module.
(define mods '(racket web-server/templates))

;; Create a namespace in which to evaluate templates, attach and
;; require the desired modules, and keep reusing it (faster).

(define (create-template-namespace)
  (define orig-ns (current-namespace))
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ;; `namespace-attach-module` says the new namespace can reuse the
    ;; module already imported into orig-ns. Faster.
    (for-each (curry namespace-attach-module orig-ns) mods)
    ;; Require the files into the namespace, too. In the case of
    ;; racket, that's mandatory (sorta the #lang racket).  The others
    ;; we could `require` in the eval form, but simplest to handle
    ;; them here, too.
    (for-each namespace-require mods))
  ns)

(define template-namespace (create-template-namespace))

;; render-template : Path PathString Dict[Symbol => Any] -> String
(define (render-template dir filename dict)
  (parameterize ([current-namespace template-namespace]
                 [current-load-relative-directory dir])
    (eval `(let (,@(for/list ([(k v) (in-dict dict)])
                     (list k `(quote ,v))))
             (include-template ,filename))
          (current-namespace))))

;; ============================================================

;; Not a submodule. Avoid running the module at all.
(module test racket/base
  (require rackunit))
