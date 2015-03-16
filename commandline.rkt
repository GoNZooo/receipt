#lang racket/base

(require racket/cmdline
         racket/list

         "transaction.rkt"
         "data.rkt")

(define (add-transaction/interactive [transactions '()])

  (define (generate-timestamp)
    (define (leading-zero unit)
      (if (< unit 10)
          (format "0~a" unit)
          (format "~a" unit)))

    (let ([d (seconds->date (* 0.001
                               (current-inexact-milliseconds)))])
      (string-append (leading-zero (date-year d)) "-"
                     (leading-zero (date-month d)) "-"
                     (leading-zero (date-day d)) "/"
                     (leading-zero (date-hour d)) ":"
                     (leading-zero (date-minute d)) ":"
                     (leading-zero (date-second d)) ":"
                     (leading-zero (date*-nanosecond d)))))

  (printf "Amount (- for payments): ")
  (define amount (read-line))
  (printf "Category: ")
  (define category (read-line))

  (define current-transaction (transaction (string->number amount)
                                           category
                                           (generate-timestamp)))

  (if (and (not (equal? "" amount))
           (not (equal? "" category)))
      (add-transaction/interactive (cons current-transaction
                                         transactions))
      transactions))

(define (add-transaction/single amount category [transactions '()])

  (define (generate-timestamp)
    (define (leading-zero unit)
      (if (< unit 10)
          (format "0~a" unit)
          (format "~a" unit)))

    (let ([d (seconds->date (* 0.001
                               (current-inexact-milliseconds)))])
      (string-append (leading-zero (date-year d)) "-"
                     (leading-zero (date-month d)) "-"
                     (leading-zero (date-day d)) "/"
                     (leading-zero (date-hour d)) ":"
                     (leading-zero (date-minute d)) ":"
                     (leading-zero (date-second d)) ":"
                     (leading-zero (date*-nanosecond d)))))

  (define current-transaction (transaction (string->number amount)
                                           category
                                           (generate-timestamp)))
  
  (cons current-transaction transactions))

(define (load-transactions)
  (map (lambda (td)
         (transaction (first td)
                      (second td)
                      (third td)))
       (call-with-input-file data-path
         read)))

(define (consolidate-transactions new old)
  (if (null? new)
      old
      (consolidate-transactions (rest new)
                                (cons (first new)
                                      old))))

(define (write-transactions ts)
  (call-with-output-file data-path
    (lambda (op)
      (write (map
              (lambda (t)
                (list (transaction-amount t)
                      (transaction-category t)
                      (transaction-timestamp t)))
              ts)
             op))
    #:exists 'replace))

(define (calculate-balance ts)
  (let ([amounts (map transaction-amount ts)])
    (printf "Balance: ~a~n"
            (foldl + 0 amounts))))

(define (list-transactions ts)
  (define (calc-tabs cat)
    (cond
     [(> (string-length cat) 8) "\t"]
     [else "\t\t"]))
  
  (printf "Amt\tCategory\t\tTimestamp~n")
  (for-each
   (lambda (t)
     (printf (string-append "~a\t~a"
                            (calc-tabs (transaction-category t))
                            "~a~n")
             (transaction-amount t)
             (transaction-category t)
             (transaction-timestamp t)))
   ts))

(define (remove-transaction timestamp transactions [new '()])
  (cond
   [(null? transactions) (reverse new)]
   [(equal? (transaction-timestamp (first transactions))
            timestamp)
    (remove-transaction timestamp
                        (rest transactions)
                        new)]
   [else
    (remove-transaction timestamp
                        (rest transactions)
                        (cons (first transactions)
                              new))]))
      
(define (add-transaction-sequence/interactive old-ts)
  (write-transactions
   (consolidate-transactions
    (reverse (add-transaction/interactive)) old-ts)))

(define (add-transaction-sequence/single amount category old-ts)
  (write-transactions
   (consolidate-transactions
    (add-transaction/single amount category) old-ts)))

(define (remove-transaction-sequence timestamp old-ts)
  (write-transactions (remove-transaction timestamp
                                          old-ts)))

(define (edit-transaction-sequence timestamp old-ts)
  (write-transactions (add-transaction/interactive
                       (remove-transaction timestamp
                                           old-ts))))

(define (get-commandline-data)
  (define current-ts (load-transactions))
  (command-line
   #:program "receipt-cli"
   #:once-each
   [("-l" "--list")
    "List transactions"
    (list-transactions current-ts)]
   [("-b" "--balance")
    "Show balance"
    (calculate-balance current-ts)]
   [("-i" "--interactive")
    "Add new transactions"
    (add-transaction-sequence/interactive current-ts)]
   [("-r" "--remove") timestamp
    "Remove a transaction"
    (remove-transaction-sequence timestamp current-ts)]
   [("-e" "--edit") timestamp
    "Edit a transaction"
    (edit-transaction-sequence timestamp current-ts)]
   #:multi
   [("-a" "--add-single") amount category
    "Add a single transaction"
    (add-transaction-sequence/single amount
                                     category
                                     current-ts)]))

(module+ main
  (get-commandline-data))