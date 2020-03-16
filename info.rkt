#lang info

;; pkg info

(define version "0.1")
(define collection "jeremiah")
(define pkg-authors '(ryanc))

(define deps
  '("base"))

(define build-deps
  '())

;; collection info

(define name "jeremiah")

(define raco-commands
  '(("jeremiah" (submod jeremiah/command raco) "run static blog generator" #f)))

(define compile-omit-paths
  '("example"))
(define test-omit-paths
  '("example"))
