#lang racket
(require file/sha1) ;for hex-string->bytes

; SET 1: Basics
; =============

; 1.1: Convert hex to base64
; --------------------------
;
; The string:
;     49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d
;
; Should produce:
;     SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t


(define (base64char n)
  (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" n))

; helper function to split a flat list into a list of lists where each
; contained list has the specified number (chunk-size) of elements.
(define (split-into chunk-size arg-list)
  (cond
    [(null? arg-list) '()]
    [(< (length arg-list) chunk-size) (list arg-list) ]
    [else
     (let-values ([(first-part second-part) (split-at arg-list chunk-size)])
       (cons first-part (split-into chunk-size second-part)))]))

(define (encode-base64-packet byte-list)
  (let ([byte-str (list->bytes byte-list)])
    (+ (arithmetic-shift (bytes-ref byte-str 0) 16)
       (arithmetic-shift (bytes-ref byte-str 1) 8)
       (bytes-ref byte-str 2))))

(define (my-base64-result num)
  (list->string (list (base64char (bitwise-and (arithmetic-shift num -18) 63))
                      (base64char (bitwise-and (arithmetic-shift num -12) 63))
                      (base64char (bitwise-and (arithmetic-shift num -6) 63))
                      (base64char (bitwise-and num 63)))))

; [NOT YET COMPLETE]
