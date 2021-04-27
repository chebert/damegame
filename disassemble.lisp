(unless (find-package :disassemble)
  (make-package :disassemble :use '(:schemeish.schemeish)))

(in-package #:disassemble)

(extend-package :disassemble
		(document "Provides disassemble for converting machine code to assembly.")
		(package-use :binary :example :assembly)
		(package-shadow 'cl:disassemble))

;; Registering and disassembling

(defvar *table* (make-hash-table :test 'equal)
  "Table of (OPCODE ...) -> DISASSEMBLER for single-byte opcodes.
where [disassmebler opcode immediate8 immediate16] returns the disassembly")
(defvar *extended-table* (make-hash-table :test 'equal)
  "Table of (OPCODE ...) -> DISASSEMBLER for extended (2-byte) opcodes.")

(define (get-table extended?)
  "Return the appropriate disassembler-table."
  (if extended?
      *extended-table*
      *table*))

(define (register-disassembler! extended? opcodes disassembler)
  "Registers [disassembler opcode immediate8 immediate16] for opcodes matching opcode-template."
  (hash-set! (get-table extended?) opcodes disassembler))

(define (register! extended? opcodes disassembler)
  "Registers [disassembler opcode] for opcodes matching opcode-template."
  (register-disassembler! extended? opcodes [(ignore-args 1 2) disassembler]))

(define (register-i8! extended? opcodes disassembler)
  "Registers [disassembler opcode immediate8] for opcodes that take an 8-bit immediate parameter."
  (register-disassembler! extended? opcodes [(ignore-args 2) disassembler]))

(define (register-i16! extended? opcodes disassembler)
  "Registers [disassembler opcode immediate16] for opcodes that take a 16-bitq immediate parameter."
  (register-disassembler! extended? opcodes [(ignore-args 1) disassembler]))

(define (find-disassembler extended? opcode)
  "Return the disassembler that matches opcode."
  (let ((table (get-table extended?)))
    (hash-ref table (hash-find-keyf table (lambda (opcodes) (member opcode opcodes))) nil)))

(defparameter *dispatch-opcode* #xcb
  "Returns the dispatch-opcode #XCB.")

(define (extended-opcode? opcode)
  "True if the opcode is 2-bytes."
  (= opcode *dispatch-opcode*))

(define (immediates memory address)
  "Assumes address is at the start of immediates. Returns (values immediate8 immediate16)"
  (let ((immediate8 (safe-vector-ref memory address 0)))
    (values immediate8
	    (u16 (safe-vector-ref memory (1+ address) 0) immediate8))))

(export
 (define (disassemble memory address)
   "Dissassembles the instruction in the memory-vector beginning at address.
If not enough bytes are provided for immediate values, the missing bytes are assumed to be 0."
   (let ((opcode (aref memory address)))
     (cond
       ((extended-opcode? opcode)
	(let ((extended-opcode (vector-ref memory (1+ address)))
	      (immediates-address (+ 2 address)))
	  (multiple-value-call (find-disassembler t extended-opcode) extended-opcode (immediates memory immediates-address))))
       (t (multiple-value-call (find-disassembler nil opcode) opcode (immediates memory (1+ address))))))))


;; Tools for extracting parameters from opcodes.

(define (r-code->spec code)
  "Given a code for r or r' return the appropriate register or '(HL)"
  (cond
    ((= code #b111) 'a)
    ((= code #b000) 'b)
    ((= code #b001) 'c)
    ((= code #b010) 'd)
    ((= code #b011) 'e)
    ((= code #b100) 'h)
    ((= code #b101) 'l)
    ((= code #b110) '(hl))))

(define (dd-code->spec code)
  "Return the register pair name for a dd-code."
  (cond
    ((= code #b00) 'bc)
    ((= code #b01) 'de)
    ((= code #b10) 'hl)
    ((= code #b11) 'sp)))

(define (qq-code->spec code)
  "Return the register pair name for a qq-code"
  (cond
    ((= code #b00) 'bc)
    ((= code #b01) 'de)
    ((= code #b10) 'hl)
    ((= code #b11) 'af)))

(define (cc-code->spec code)
  "Return FLAG or (NOT FLAG) for the condition code."
  (cond
    ((= code #b00) '(not zero?))
    ((= code #b01) 'zero?)
    ((= code #b10) '(not carry?))
    ((= code #b11) 'carry?)))

(define (t-code->address code)
  "Return the address for an RST jump."
  (* code #x08))

(define ss-code->spec
  "Return the register pair name for a dd-code"
  #'dd-code->spec)

(define (extract-r-dest-spec opcode)
  "Extract the r-code spec from the destination position of the opcode."
  (r-code->spec (bit-extract opcode 3 3)))
(define (extract-r-source-spec opcode)
  "Extract the r-code spec from the source position of the opcode."
  (r-code->spec (bit-extract opcode 0 3)))

(define (extract-dd-spec opcode)
  "Extract the dd-code spec from the opcode"
  (dd-code->spec (bit-extract opcode 4 2)))
(define (extract-qq-spec opcode)
  "Extract the qq-code spec from the opcode"
  (qq-code->spec (bit-extract opcode 4 2)))
(define extract-ss-spec
  "Extract the ss-code spec from the opcode"
  #'extract-dd-spec)

(define (extract-cc-spec opcode)
  "Extract the condition code from the opcode"
  (cc-code->spec (bit-extract opcode 3 2)))

(define (extract-t-address opcode)
  "Extract the address for an RST jump."
  (t-code->address (bit-extract opcode 3 3)))



(define (string->list string)
  "Convert a string into a list of chars."
  (coerce string 'list))

(define (list->byte bits)
  "Given a list of bits e.g. (1 0 1 1) produce a byte #b1011"
  (foldl (lambda (bit index result) (bit-value-set result index bit))
	 0
	 (reverse bits)
	 (range 8)))

(define-examples list->byte
  ((list->byte '(1 0 1 1))
   #b1011))


(define (opcodes template)
  "Given a template which is a string of 1s 0s, spaces and other characters, produce a list of all matching bytes.
Spaces are ignored, while any other character is a variable."
  (define (map-cons value list-of-lists)
    (map (lambda (list) (cons value list)) list-of-lists))
  (let rec ((patterns (remove #\space (string->list template)))
	    (results '(())))
    (cond
      ((null? patterns) (map (compose #'list->byte #'reverse) results))
      (t (let ((pattern (first patterns)))
	   (cond
	     ((eq? pattern #\1) (rec (rest patterns) (map-cons 1 results)))
	     ((eq? pattern #\0) (rec (rest patterns) (map-cons 0 results)))
	     (t
	      (rec (rest patterns)
		   (append-map (lambda (result) (list (cons 0 result) (cons 1 result)))
			       results)))))))))

(define-examples opcodes
  ((set=?
    (opcodes "101 10 xx1")
    '(#b10110001
      #b10110011
      #b10110101
      #b10110111))))


(defmacro disassembler ((opcode &optional immediate) &body body)
  "Equivalent to lambda but opcode is ignorable and immediate is discarded if not provided."
  (if immediate
      `(lambda (,opcode ,immediate)
	 (declare (ignorable ,opcode))
	 ,@body)
      `(lambda (,opcode)
	 (declare (ignorable ,opcode))
	 ,@body)))


;; Disassemblers for GameBoy

(define (ld-rr-opcodes)
  "Excludes (LD (HL) (HL)) from the possible (LD r r) opcodes."
  (remove #b01110110 (opcodes "01 rrr rrr")))

;; 8-bit Transfer Instructions
(register!     nil (ld-rr-opcodes)        (disassembler (opcode)   (make-ld (extract-r-dest-spec opcode) (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "00 rrr 110") (disassembler (opcode i) (make-ld (extract-r-dest-spec opcode) i)))

(register!     nil (opcodes "00 001 010") (disassembler (opcode)   (make-ld 'a '(bc))))
(register!     nil (opcodes "00 011 010") (disassembler (opcode)   (make-ld 'a '(de))))
(register!     nil (opcodes "00 000 010") (disassembler (opcode)   (make-ld '(bc) 'a)))
(register!     nil (opcodes "00 010 010") (disassembler (opcode)   (make-ld '(de) 'a)))
(register!     nil (opcodes "00 101 010") (disassembler (opcode)   (make-ld 'a '(hli))))
(register!     nil (opcodes "00 100 010") (disassembler (opcode)   (make-ld '(hli) 'a)))
(register!     nil (opcodes "00 111 010") (disassembler (opcode)   (make-ld 'a '(hld))))
(register!     nil (opcodes "00 110 010") (disassembler (opcode)   (make-ld '(hld) 'a)))

(register!     nil (opcodes "11 110 010") (disassembler (opcode)   (make-ld 'a '(c))))
(register!     nil (opcodes "11 100 010") (disassembler (opcode)   (make-ld '(c) 'a)))
(register-i16! nil (opcodes "11 111 010") (disassembler (opcode i) (make-ld 'a `(,i))))
(register-i16! nil (opcodes "11 101 010") (disassembler (opcode i) (make-ld `(,i) 'a)))

(define (offset address) (+ #xff00 address))
(register-i8!  nil (opcodes "11 110 000") (disassembler (opcode i) (make-ld 'a `(,(offset i)))))
(register-i8!  nil (opcodes "11 100 000") (disassembler (opcode i) (make-ld `(,(offset i)) 'a)))

;; 16-bit transfers
(register-i16! nil (opcodes "00 001 000") (disassembler (opcode i) (make-ld `(,i) 'sp)))
(register-i16! nil (opcodes "00 dd0 001") (disassembler (opcode i) (make-ld (extract-dd-spec opcode) i)))
(register-i8!  nil (opcodes "11 111 000") (disassembler (opcode i) (make-ldhl (u8->s8 i))))
(register!     nil (opcodes "11 111 001") (disassembler (opcode)   (make-ld 'sp 'hl)))

(register!     nil (opcodes "11 qq0 101") (disassembler (opcode)   (make-push (extract-qq-spec opcode))))
(register!     nil (opcodes "11 qq0 001") (disassembler (opcode)   (make-pop  (extract-qq-spec opcode))))

;; 8-bit Arithmetic
(register!     nil (opcodes "10 000 rrr") (disassembler (opcode)   (make-add 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 000 110") (disassembler (opcode i) (make-add 'a i)))
(register!     nil (opcodes "10 001 rrr") (disassembler (opcode)   (make-adc 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 001 110") (disassembler (opcode i) (make-adc 'a i)))
(register!     nil (opcodes "10 010 rrr") (disassembler (opcode)   (make-sub 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 010 110") (disassembler (opcode i) (make-sub 'a i)))
(register!     nil (opcodes "10 011 rrr") (disassembler (opcode)   (make-sbc 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 011 110") (disassembler (opcode i) (make-sbc 'a i)))

;; 8-bit logical
(register!     nil (opcodes "10 100 rrr") (disassembler (opcode)   (make-and 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 100 110") (disassembler (opcode i) (make-and 'a i)))
(register!     nil (opcodes "10 110 rrr") (disassembler (opcode)   (make-or  'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 110 110") (disassembler (opcode i) (make-or  'a i)))
(register!     nil (opcodes "10 101 rrr") (disassembler (opcode)   (make-xor 'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 101 110") (disassembler (opcode i) (make-xor 'a i)))
(register!     nil (opcodes "10 111 rrr") (disassembler (opcode)   (make-cp  'a (extract-r-source-spec opcode))))
(register-i8!  nil (opcodes "11 111 110") (disassembler (opcode i) (make-cp  'a i)))

;; 8-Bit arithmetic update
(register!     nil (opcodes "00 rrr 100") (disassembler (opcode)   (make-inc (extract-r-dest-spec opcode))))
(register!     nil (opcodes "00 rrr 101") (disassembler (opcode)   (make-dec (extract-r-dest-spec opcode))))

;; 16-bit Arithmetic
(register!     nil (opcodes "00 ss1 001") (disassembler (opcode)   (make-add 'hl (extract-ss-spec opcode))))
(register-i8!  nil (opcodes "11 101 000") (disassembler (opcode i) (make-add 'sp (u8->s8 i))))

;; 16-bit Arithmetic update
(register!     nil (opcodes "00 ss0 011") (disassembler (opcode)   (make-inc (extract-ss-spec opcode))))
(register!     nil (opcodes "00 ss1 011") (disassembler (opcode)   (make-dec (extract-ss-spec opcode))))

;; Rotate Instructions
(register!     nil (opcodes "00 000 111") (disassembler (opcode)   (make-rlca)))
(register!     nil (opcodes "00 010 111") (disassembler (opcode)   (make-rla)))
(register!     nil (opcodes "00 001 111") (disassembler (opcode)   (make-rrca)))
(register!     nil (opcodes "00 011 111") (disassembler (opcode)   (make-rra)))
(register!     t   (opcodes "00 000 rrr") (disassembler (opcode)   (make-rlc (extract-r-source-spec opcode))))
(register!     t   (opcodes "00 010 rrr") (disassembler (opcode)   (make-rl  (extract-r-source-spec opcode))))
(register!     t   (opcodes "00 001 rrr") (disassembler (opcode)   (make-rrc (extract-r-source-spec opcode))))
(register!     t   (opcodes "00 011 rrr") (disassembler (opcode)   (make-rr  (extract-r-source-spec opcode))))

;; Shift Instructions
(register!     t   (opcodes "00 100 rrr") (disassembler (opcode)   (make-sla (extract-r-source-spec opcode))))
(register!     t   (opcodes "00 101 rrr") (disassembler (opcode)   (make-sra (extract-r-source-spec opcode))))
(register!     t   (opcodes "00 111 rrr") (disassembler (opcode)   (make-srl (extract-r-source-spec opcode))))

;; Swap
(register!     t   (opcodes "00 110 rrr") (disassembler (opcode)   (make-swap (extract-r-source-spec opcode))))

;; Bit Instructions
(define (extract-bit-index opcode) (bit-extract opcode 3 3))
(register!     t   (opcodes "01 bbb rrr") (disassembler (opcode)   (make-bit (extract-bit-index opcode) (extract-r-source-spec opcode))))
(register!     t   (opcodes "11 bbb rrr") (disassembler (opcode)   (make-set (extract-bit-index opcode) (extract-r-source-spec opcode))))
(register!     t   (opcodes "10 bbb rrr") (disassembler (opcode)   (make-res (extract-bit-index opcode) (extract-r-source-spec opcode))))

;; Jump instructions
(define (relative immediate8) (+ 2 (u8->s8 immediate8)))
(register-i16! nil (opcodes "11 000 011") (disassembler (opcode i) (make-jp i)))
(register-i16! nil (opcodes "11 0cc 010") (disassembler (opcode i) (make-jpc (extract-cc-spec opcode) i)))
(register-i8!  nil (opcodes "00 011 000") (disassembler (opcode i) (make-jr (relative i))))
(register-i8!  nil (opcodes "00 1cc 000") (disassembler (opcode i) (make-jrc (extract-cc-spec opcode) (relative i))))
(register!     nil (opcodes "11 101 001") (disassembler (opcode)   (make-jp '(HL))))

;; Call Instructions 
(register-i16! nil (opcodes "11 001 101") (disassembler (opcode i) (make-call i)))
(register-i16! nil (opcodes "11 0cc 100") (disassembler (opcode i) (make-callc (extract-cc-spec opcode) i)))

;; Return Instructions
(register!     nil (opcodes "11 001 001") (disassembler (opcode)   (make-ret)))
(register!     nil (opcodes "11 011 001") (disassembler (opcode)   (make-reti)))
(register!     nil (opcodes "11 0cc 000") (disassembler (opcode)   (make-retc (extract-cc-spec opcode))))

;; Restart
(register!     nil (opcodes "11 ttt 111") (disassembler (opcode)   (make-rst (extract-t-address opcode))))

;; MISC
(register!     nil (opcodes "00 100 111") (disassembler (opcode)   (make-daa)))
(register!     nil (opcodes "00 101 111") (disassembler (opcode)   (make-cpl)))
(register!     nil (opcodes "00 000 000") (disassembler (opcode)   (make-nop)))
(register!     nil (opcodes "00 111 111") (disassembler (opcode)   (make-ccf)))
(register!     nil (opcodes "00 110 111") (disassembler (opcode)   (make-scf)))
(register!     nil (opcodes "11 110 011") (disassembler (opcode)   (make-di)))
(register!     nil (opcodes "11 111 011") (disassembler (opcode)   (make-ei)))
(register!     nil (opcodes "01 110 110") (disassembler (opcode)   (make-halt)))
(register!     nil (opcodes "00 010 000") (disassembler (opcode)   (make-stop)))

;; Sanity Checks

(defparameter *unused-short-opcodes*
  (cons *dispatch-opcode* '(#xd3 #xe3 #xe4 #xf4 #xdb #xeb #xec #xfc #xdd #xed #xfd))
  "There are 11 single-byte opcodes which are unused + the dispatching opcode.")

(define (all-byte-values)
  "return a list of all byte values from (0..255)"
  (range #x100))
(define (all-short-opcodes)
  "Return a list of all valid single-byte opcodes."
  (set-difference (all-byte-values) *unused-short-opcodes*))
(define (all-extended-opcodes)
  "Return a list of all valid double-byte opcodes."
  (all-byte-values))

(define (missing-short-opcodes)
  "Returns a list of unregistered short-opcodes"
  (filter-not (lambda (opcode) (find-disassembler nil opcode))
	      (all-short-opcodes)))

(define (missing-extended-opcodes)
  "Returns a list of unregistered extended opcodes."
  (filter-not (lambda (opcode) (find-disassembler t opcode))
	      (all-extended-opcodes)))

(define (group-disassembly list-of-opcode-and-disassembly)
  "Return disassemblies grouped by the instruction name."
  (group
   (compose #'first #'second)
   list-of-opcode-and-disassembly))

(define (all-short-opcodes-disassembled (immediate8 42) (immediate16 1337))
  "Return a list of all short opcodes disassembled with the provided immediate values.
List is grouped in the form ((instruction-name (opcode instruction) . more-instructions) . more-groups)"
  (group-disassembly
   (map (lambda (opcode)
	  (list opcode [(find-disassembler nil opcode) opcode immediate8 immediate16]))
	(all-short-opcodes))))

(define (all-extended-opcodes-disassembled (immediate8 42) (immediate16 1337))
  "Return a list of all extended opcodes disassembled with the provided immediate values.
List is grouped in the form ((instruction-name (opcode instruction) . more-instructions) . more-groups)"
  (group-disassembly
   (map (lambda (opcode)
	  (list opcode [(find-disassembler t opcode) opcode immediate8 immediate16]))
	(all-extended-opcodes))))

(define (all-opcodes-disassembled (immediate16 1337))
  "Returns grouped opcodes using (DISASSEMBLE memory address)"
  (let ((low (low-byte immediate16))
	(high (high-byte immediate16)))
    (group-disassembly
     (append
      (map (lambda (opcode) (list opcode (disassemble (vector opcode low high) 0)))
	   (all-short-opcodes))
      (map (lambda (opcode) (list (list *dispatch-opcode* opcode) (disassemble (vector *dispatch-opcode* opcode low high) 0)))
	   (all-byte-values))))))

(define (all-registered-instruction-names)
  "returns a list of all instruction names from disassembling all opcodes."
  (map 'first (all-opcodes-disassembled)))

(defparameter *instruction-names*
  '(ld push pop
    add adc sub sbc
    and xor or cp
    inc dec
    daa
    cpl
    ldhl
    rlca rla rrca rra rlc rl rrc rr
    sla swap sra srl
    bit set res
    ccf scf
    nop halt stop di ei
    jp jr call ret reti rst

    ;; Added by me.
    jpc jrc callc retc)
  "A list of all known instruction names.")

(define (missing-instruction-names)
  "Returns a list of all unregistered instruction names."
  (set-difference *instruction-names* (all-registered-instruction-names)))

;; see that all opcodes are covered.
(assert (null (missing-short-opcodes)))
(assert (null (missing-extended-opcodes)))
;; make sure we can disassemble all opcodes, and we at least have covered all instructions.
(assert (empty? (missing-instruction-names)))
