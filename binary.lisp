(in-package #:binary)
(install-syntax!)

(export
 (define (u8 value)
   "Truncate value to an unsigned byte."
   (logand #xFF value)))

(define-examples u8
  ((u8 #xfff) #xff))

(export
 (define (low-byte u16)
   "The low byte of an unsigned 16-bit integer."
   (u8 u16)))

(export
 (define (high-byte u16)
   "The high byte of an unsigned 16-bit integer."
   (u8 (ash u16 -8))))

(define-examples low-byte
  ((low-byte #xDEAD) #xAD))
(define-examples high-byte
  ((high-byte #xDEAD) #xDE))

(export
 (define (u16 high-byte low-byte)
   "Return a u16 constructed from two u8 bytes."
   (+ (ash high-byte 8) low-byte)))

(define-examples u16
  ((u16 #xde #xad) #xdead))

(export
 (define (u8->s8 byte)
   "Return the signed value of an 8-bit unsigned integer."
   (if (<= byte #x7F)
       byte
       (- byte #x100))))

(define-examples u8->s8
  ((u8->s8 127) 127)
  ((u8->s8 128) -128)
  ((u8->s8 255) -1))

(export
 (define (u16->s16 u16)
   "Return the signed value of a 16-bit unsigned integer."
   (if (<= u16 #x7FFF)
       u16
       (- u16 #x10000))))

(define-examples u16->s16
  ((u16->s16 32767) 32767)
  ((u16->s16 32768) -32768)
  ((u16->s16 #xffff) -1))

(export
 (define (bit-set byte bit-index)
   "Set the bit at bit-index."
   (logior (ash 1 bit-index) byte)))
(export
 (define (bit-reset byte bit-index)
   "Reset the bit at bit-index."
   (logand (- #xff (ash 1 bit-index)) byte)))

(define-examples bit-set
  ((bit-set #b11001101 4) #b11011101))

(define-examples bit-reset
  ((bit-reset #b11001101 3) #b11000101))

(export
 (define (bit-reset? byte bit-index)
   "True if the bit at bit-index is zero."
   (zero? (logand (ash 1 bit-index) byte))))
(export
 (define (bit-set? byte bit-index)
   "True if the bit at bit-index is one."
   (not (bit-reset? byte bit-index))))

(define-examples bit-set?
  ((bit-set? #b1011 1))
  ((not (bit-set? #b1011 2))))
(define-examples bit-reset?
  ((not (bit-reset? #b1011 1)))
  ((bit-reset? #b1011 2)))

(export
 (define (bit-value byte bit-index)
   "Return 1 or 0 if the bit is set/reset."
   (if (bit-set? byte bit-index)
       1
       0)))

(define-examples bit-value
  ((bit-value #b1011 1) 1)
  ((bit-value #b1011 2) 0))

(export
 (define (bit-value-set byte bit-index bit-value)
   "Set the bit if bit-value is 1, else reset the bit at bit index."
   (if (zero? bit-value)
       (bit-reset byte bit-index)
       (bit-set byte bit-index))))

(define-examples bit-value-set
  ((bit-value-set #b11001101 4 1) #b11011101)
  ((bit-value-set #b11001101 3 0) #b11000101))

(export
 (define (rotate-left byte bit0 continue)
   "Calls [continue new-byte old-bit7]
Byte will be shifted to the left 1 bit.
Bit0 will be the 0th bit in the new byte."
   [continue (u8 (logior (ash byte 1) bit0))
	     (bit-value byte 7)]))

(define-examples rotate-left
  ((let ((values))
     (rotate-left #b10010101 1
		  (lambda args (setq values args)))
     values)
   '(#b00101011 1)))

(export
 (define (rotate-right byte bit7 continue)
   "calls [continue new-byte old-bit0].
Byte will be shifted to the right 1 bit.
bit7 will be the 7th bit in the new byte."
   [continue (u8 (bit-value-set (ash byte -1) 7 bit7))
	     (bit-value byte 0)]))

(define-examples rotate-right
  ((let ((values))
     (rotate-right #b10010101 1
		   (lambda args (setq values args)))
     values)
   '(#b11001010 1)))


(export
 (define (low-nibble byte)
   "Return the lower 4-bits of a byte."
   (logand #xF byte)))
(export
 (define (high-nibble byte)
   "Return the upper 4-bits of a byte."
   (ash (logand #xf0 byte) -4)))
(export
 (define (swap-nibbles byte)
   "Swap the upper/lower nibbles of byte."
   (let ((low (low-nibble byte))
	 (high (high-nibble byte)))
     (logior (ash low 4) high))))

(define-examples low-nibble
  ((low-nibble #xab) #xb))
(define-examples high-nibble
  ((high-nibble #xab) #xa))
(define-examples swap-nibbles
  ((swap-nibbles #xab) #xba))

(export
 (define (decimal-adjusted-arithmetic-carry? subtraction? carry? half-carry? byte)
   "A carry occurs if the previous operation was addition, and one of the following:
   - a carry occurred
   - a half-carry occurred
   - the decimal value exceeded 99
   - the ones digit exceeded 9."
   (let ((ninety-nine #x99))
     (and (not subtraction?)
	  (or carry? (> byte ninety-nine)
	      half-carry? (> (low-nibble byte) 9))))))

(export
 (define (decimal-adjusted-arithmetic subtraction? carry? half-carry? byte)
   "Adjusts for decimal arithemic (DAA).
For additition:
  if either a carry occurs or the decimal value exceeds 99, add 6 to the upper digit.
  if either a half-carry occurs or the lower digit exceeds 9, add 6 to the lower digit.
For subtraction:
  if a borrow occurs, subtract 6 from the upper digit.
  if a half-borrow occurs, subtract 6 from the lower digit."
   (let* ((low-digit (low-nibble byte))
	  (adjustment 0))
     (cond
       (subtraction?
	(when carry? (decf adjustment #x60))
	(when half-carry? (decf adjustment 6)))
       (t
	(when (or carry? (> byte #x99)) (incf adjustment #x60))
	(when (or half-carry? (> low-digit 9)) (incf adjustment #x06))))
     (+ byte adjustment))))


(define (prefix-string string minimum-length prefix-char)
  "Prefixes string with prefix-char to ensure that that the resulting string has at least minimum-length characters."
  (let ((len (length string)))
    (if (< len minimum-length)
	(string-append (make-string (- minimum-length len) :initial-element prefix-char)
		       string)
	string)))

(define (hex-string num-bytes value)
  "Return a string representing the value in hex."
  (string-append "#x"
		 (prefix-string (number->string value 16)
				(* 2 num-bytes)
				#\0)))
(define (bin-string num-bytes value)
  "Return a string representing the value in binary."
  (string-append "#b"
		 (prefix-string (number->string value 2)
				(* 8 num-bytes)
				#\0)))

(export
 (define (bin8-string u8) (bin-string 1 u8)))
(export
 (define (hex8-string u8) (hex-string 1 u8)))
(export
 (define s8-string (compose #'number->string #'u8->s8)))
(export
 (define u8-string #'number->string))

(export
 (define (hex16-string u16) (hex-string 2 u16)))
(export
 (define s16-string (compose #'number->string #'u16->s16)))
(export
 (define u16-string #'number->string))

(check-all-examples)

(uninstall-syntax!)
#+nil
(defpackage-form :binary)
