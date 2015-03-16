#lang racket/base

(provide (struct-out transaction))

(struct transaction (amount category timestamp)
        #:transparent)