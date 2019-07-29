#lang scheme
(require scheme/foreign)
(require scheme/system)

(provide say
         say*
         identity
         pad-string
         chmod
         backticks)

(unsafe!)

; 'printf' with a terminating newline.
(define (say* form . args)
  (apply printf (cons form args))
  (newline))

(define (say s)
  (display s)
  (newline))

; Pad a string to at least LEN characters.  Use CHAR at start of string for
; padding.
(define (pad-string str len (char #\space))
  (let ((diff (- len (string-length str))))
    (if (positive? diff)
        (string-append (make-string diff char) str)
        str)))

; Standard identity function.  f(x) => x.
(define (identity x) x)

; Thanks Eli.
(define chmod (get-ffi-obj "chmod" #f (_fun _path _int -> _int)))

; Emulation of Unix which(1).
(define (which command)
  #t)

; Run CMD through shell and return the output as a list of strings.
(define (backticks cmd)
  (let ((l (process cmd)))
    (close-output-port (second l))
    (close-input-port (fourth l))

    ((fifth l) 'wait)

    (let ((output (port->lines (first l))))
      (close-input-port (first l))
      output)))
