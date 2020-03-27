#lang racket/base
(require racket/cmdline
         racket/match
         racket/class
         raco/command-name
         net/url
         "private/util.rkt"
         "private/config.rkt"
         "private/data.rkt"
         "private/main.rkt")
(provide)

;; There are no verbose flags, but there is a logger:
;;
;;   PLTSTDERR="info@jeremiah" raco jeremiah ....  # verbose
;;   PLTSTDERR="debug@jeremiah" raco jeremiah .... # very verbose

(define (uerror fmt . args)
  (raise-user-error
   (string-append (or (short-program+command-name) "jeremiah") ": "
                  (apply format fmt args))))

;; ============================================================
;; Run site generator

(define (cmd:make args)
  (define the-site-file #f)
  (define force-rebuild? #f)
  (command-line
   #:program (short-program+command-name)
   #:argv args
   #:once-each
   [("-s" "--site-file") site-file
    "Use the given site configuration file"
    (set! the-site-file site-file)]
   [("--force-rebuild")
    "Force rebuild of posts from source"
    (set! force-rebuild? #t)]
   #:args ()
   (define config (load-config the-site-file))
   (go config #:force-rebuild? force-rebuild?)))

(define SITE-FILE "site.rkt")

;; load-config : PathString -> Config
(define (load-config config-file)
  (define config-path
    (cond [config-file
           ;; Note: race possible, but probably usually helpful
           (unless (file-exists? config-file)
             (uerror "site file (set with `-s` or `--site-file`) does not exist\n  file: ~e"
                     config-file))
           (path->complete-path config-file)]
          [else
           (unless (file-exists? SITE-FILE)
             (uerror "default site file does not exist\n  file: ~e" SITE-FILE))
           (path->complete-path SITE-FILE)]))
  ;; Load site config file
  (log-jeremiah-debug "loading site configuration: ~e" config-path)
  (call/allow-configuration
   (lambda ()
     (dynamic-require config-path #f)
     (new config%))))

;; ============================================================
;; Preview

(require racket/lazy-require)
(lazy-require ["private/preview.rkt" (preview)])

(define (cmd:preview args)
  (define the-site-file #f)
  (define the-build-dir #f)
  (parameterize ((base-url (string->url "http://localhost/")))
    (command-line
     #:program (short-program+command-name)
     #:argv args
     #:once-any
     [("-s" "--site-file") site-file
      "Use the given site configuration file"
      (set! the-site-file site-file)]
     [("-d") build-dir
      "Preview the contents of build-dir"
      (set! the-build-dir build-dir)]
     #:args ()
     (unless (or the-site-file the-build-dir)
       (uerror "location not set (use `-s` or `-d`)"))
     (define config (and the-site-file (load-config the-site-file)))
     (preview #:dir (or the-build-dir (send config get-dest-dir))))))


;; ============================================================
;; Help

(define (cmd:help _args)
  (printf "Usage: ~a <command> <option> ... <arg> ...\n\n"
          (short-program+command-name))
  (printf "Commands:\n")
  (define command-field-width
    (+ 4 (apply max 12 (map string-length (map car subcommand-handlers)))))
  (for ([subcommand (in-list subcommand-handlers)])
    (match-define (list command _ help-text) subcommand)
    (define pad (make-string (- command-field-width (string-length command)) #\space))
    (printf "  ~a~a~a\n" command pad help-text)))


;; ============================================================
;; Main (command dispatch)

(define subcommand-handlers
  `(("help"    ,cmd:help     "show help")
    ("make"    ,cmd:make     "run site generator")
    ("preview" ,cmd:preview  "run web server and launch browser for preview")))

(define (dispatch-command)
  (define (call-subcommand handler name args)
    (parameterize ((current-command-name
                    (cond [(current-command-name)
                           => (lambda (prefix) (format "~a ~a" prefix name))]
                          [else #f])))
      (handler args)))
  (define args (vector->list (current-command-line-arguments)))
  (cond [(and (pair? args) (assoc (car args) subcommand-handlers))
         => (lambda (p) (call-subcommand (cadr p) (car args) (cdr args)))]
        [else (cmd:help args)]))

(module+ raco
  (dispatch-command))

(module+ main
  (dispatch-command))

(module test racket/base
  (void))
