(unless (find-package :analyze)
  (make-package :analyze :use '(:schemeish.schemeish)))

(in-package #:analyze)

;; TODO: Rename ANALYZE to ENCODE
;; TODO: Rename ANALYZER to ENCODER

(extend-package :analyze
		(document "Provides analyze for converting assembly instructions into machine primitives.")
		(package-use :binary :example :assembly :machine-ops :operations)
		(package-shadow 'drop))

;; An Analyzer is a function from instruction -> machine-op
;; (analyze instr) => machine-op
;; #'analyze => ANALYZER

;; (analyzer . args) => ANALYZER
;; [(analyzer . args) instr] => machine-op

(defvar *table* (make-hash-table)
  "Table of instruction name -> analyzer where [analyzer . instruction] -> (machine-ops ...)")

(define (register! instruction-name analyzer)
  "registers an analyzer for the instruction-name."
  (hash-set! *table* instruction-name analyzer))


(define (find-analyzer instruction-name)
  "Finds analyzer associated with instruction-name."
  (hash-ref *table* instruction-name))

(export
 (define (analyze instruction)
   "Converts assembly instruction into a machine-op."
   [(find-analyzer (first instruction)) instruction]))

(define (analyze-address-spec addr-spec)
  "addr-spec := register-pair
              | C
              | number
If addr-spec is the register-name C, it is added to #xFF00 for relative-addressing. ( -- address )"
  (cond
    ((register-pair? addr-spec)
     (reg addr-spec))
    ((eq? addr-spec 'c)
     (seq (constant #xFF00)
	  (reg addr-spec)
	  (perform 2 '+)))
    ((number? addr-spec)
     (constant addr-spec))
    (t (error "~S is not a valid addr-spec" addr-spec))))

(define-examples analyze-address-spec
  ((stack-effects-of-size? (append-map #'machine-op-stack-effects
				       (list
					(analyze-address-spec 'bc)
					(analyze-address-spec 'c)
					(analyze-address-spec 42)))
			   0 1)))

(define (analyze-dest-spec spec)
  "Dest-spec := (address-spec)
              | register-name
Assigns the value on the stack to dest spec. ( u8 -- )"
  (cond
    ((pair? spec)
     ;; (address-spec) => store memory
     (let ((addr-spec (first spec)))
       (assert (null? (rest spec)))
       (seq
	(analyze-address-spec addr-spec)
	(store))))
    ((register-name? spec) (assign-reg spec))
    (t (error "Invalid dest-spec ~S" spec))))


(define-examples analyze-dest-spec
  ((stack-effects-of-size? (append-map #'machine-op-stack-effects
				       (list (analyze-dest-spec 'a)
					     (analyze-dest-spec '(bc))))
			   1 0)))


(define (boolean->bit bool)
  "NIL is 0, else 1."
  (if bool 1 0))

(define (analyze-source-spec spec)
  "Source-spec := (address-spec)
                | register-name
                | number
Pushes the value of source-spec onto the stack. ( -- u8 )"
  (cond
    ((pair? spec)
     ;; (address-spec) => fetch memory
     (let ((addr-spec (first spec)))
       (assert (null? (rest spec)))
       (seq
	(analyze-address-spec addr-spec)
	(fetch))))
    ((register-name? spec) (reg spec))
    ((number? spec) (constant spec))
    (t (error "Invalid source-spec ~S" spec))))


(define-examples analyze-source-spec
  ((stack-effects-of-size? (append-map #'machine-op-stack-effects
				       (list (analyze-source-spec '(bc))
					     (analyze-source-spec 'a)
					     (analyze-source-spec 42)))
			   0 1)))

(define (analyze-ld instr)
  "Analysis for (LD dest source)"
  (seq
   (analyze-source-spec (assign-source instr))
   (analyze-dest-spec (assign-dest instr))))

(define (analyze-update dest update-op)
  "Analysis for storing [update-op dest] into dest. ( -- )"
  (seq (analyze-source-spec dest)
       (perform 1 update-op)
       (analyze-dest-spec dest)))

(define-examples analyze-update
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-update 'sp '1-))
			   0 0)))

(define (analyze-push-reg register-name)
  "Analysis for pushing register-name onto the stack. ( -- )"
  (define (push-byte byte-op)
    (seq (analyze-update 'sp '1-)
	 (reg register-name)
	 (perform 1 byte-op)
	 (reg 'sp)
	 (store)))
  (seq (push-byte 'high-byte)
       (push-byte 'low-byte)))

(define-examples analyze-push-reg
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-push-reg 'pc))
			   0 0)))

(define (analyze-push instr)
  "Analysis for (PUSH reg)"
  (analyze-push-reg (push-source instr)))

(define (u16-swapped low high) (u16 high low))

(define (analyze-pop-stack)
  "Analysis for popping off the stack. ( -- u16 )"
  (define pop-byte
    (seq (reg 'sp) (fetch)
	 (analyze-update 'sp '1+)))
  (seq pop-byte pop-byte ;; ( -- low high )
       (perform 2 'u16-swapped)))

(define-examples analyze-pop-stack
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-pop-stack))
			   0 1)))

(define (analyze-pop-reg register-name)
  "Analysis for popping off the stack into register. ( -- )"
  (define pop-byte
    (seq (reg 'sp) (fetch)
	 (analyze-update 'sp '1+)))
  (seq (analyze-pop-stack) (assign-reg register-name)))

(define-examples analyze-pop-reg
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-pop-reg 'pc))
			   0 0)))

(define (analyze-pop instr)
  "Analysis for (POP reg)"
  (analyze-pop-reg (pop-dest instr)))

(define (analyze-ldhl instr)
  "Analysis for (LDHL sp offset)"
  (seq
   (reg 'sp)
   (constant (ldhl-offset instr))
   (analyze-alu-op 'add16-op nil)
   (assign-reg 'hl)))

(define analyze-add-hl
  "Analysis for (ADD HL source)"
  (alu-analyzer 
   (seq
    (perform 2 'add16-op 5)
    (drop) ;; zero? is discarded
    (assign-flag 'subtraction?)
    (assign-flag 'half-carry?)
    (assign-flag 'carry?))))

(define (analyze-carry with-carry?)
  "Analysis which, if with-carry? is true, pushes carry-bit on the stack.
If with-carry?: ( -- bit )
Else          : ( -- )"
  (if with-carry?
      (seq (flag 'carry?) (perform 1 'boolean->bit))
      (nop)))

(define-examples analyze-carry
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-carry t))
			   0 1))
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-carry nil))
			   0 0)))


(define (assign-flags)
  "Analysis which assigns all of the flags in order: ZNHC. ( c? h? s? z? -- )"
  (seq
   (assign-flag 'zero?)
   (assign-flag 'subtraction?)
   (assign-flag 'half-carry?)
   (assign-flag 'carry?)))

(define-examples assign-flags
  ((stack-effects-of-size? (machine-op-stack-effects (assign-flags))
			   4 0)))

(define (analyze-alu-op alu-op with-carry?)
  "Analysis that performs [alu-op a b (carry)] => op-result
and assigns flags.
Leaves the result on the stack. ( a b -- result )"
  (seq
   (analyze-carry with-carry?)
   (perform (if with-carry? 3 2) alu-op 5)
   (assign-flags)))

(define-examples analyze-alu-op
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-alu-op 'add16 t))
			   2 1))
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-alu-op 'add16 nil))
			   2 1)))

(define (analyze-alu-sources instr)
  "Analysis which gets ALU dest and source. ( -- dest source )"
  (seq
   (analyze-source-spec (assign-dest instr))
   (analyze-source-spec (assign-source instr))))

(define-examples analyze-alu-sources
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-alu-sources '(add a c)))
			   0 2)))

(define ((alu-analyzer alu-op-analysis) instr)
  "Analyzer which performs analysis on source and dest, and assigns to dest. ( -- )"
  (seq (analyze-alu-sources instr)
       alu-op-analysis
       (analyze-dest-spec (assign-dest instr))))

(define (arithmetic-analyzer arithmetic-op with-carry?)
  "ALU-Analyzer for arithmetic operations. [arithmetic-op a b (carry)] => op-result. ( -- )"
  (alu-analyzer (analyze-alu-op arithmetic-op with-carry?)))

(define (logical-analyzer logical-op)
  "ALU-Analyzer for logical operations. [logical-op a b] => op-result. ( -- )"
  (alu-analyzer (analyze-alu-op logical-op nil)))

(define (drop (n 1))
  "Discards values from the machine stack. ( value -- )"
  (perform n 'values 0))

(define (analyze-cp instr)
  "Analysis for (CP dest source)"
  (seq
   (analyze-alu-sources instr)
   (analyze-alu-op 'sub-op nil)
   ;; discard the result
   (drop)))

(define ((update-analyzer update-op) instr)
  "Analyzer for updating a value: [update-op value] => value. ( -- )"
  (analyze-update (update-dest instr) update-op))

(define (analyze-add op)
  "Analyze 8-bit and 16-bit (ADD dest source) ( -- )"
  (let ((dest (assign-dest op)))
    (cond
      ;; TODO: new instructions for ADDHL and ADDSP
      ((eq? dest 'hl) (analyze-add-hl op))
      ((eq? dest 'sp) [(arithmetic-analyzer 'add16-op nil) op])
      (t [(arithmetic-analyzer 'add8-op nil) op]))))

(define ((shift-analyzer shift-op with-carry?) instr)
  "Analyzer for updating a byte and flags: [shift-op byte (carry)] => (values byte . flags) ( -- )"
  (seq
   (analyze-source-spec (update-dest instr))
   (analyze-carry with-carry?)
   (perform (if with-carry? 2 1) shift-op 5)
   (assign-flags)
   (analyze-dest-spec (update-dest instr))))

(define (analyze-bit instr)
  "Analysis (BIT index source) ( -- )"
  (seq
   (analyze-source-spec (bit-op-source instr))
   (constant (bit-op-index instr))
   (perform 2 'bit-reset? 1) (assign-flag 'zero?)
   (constant t) (assign-flag 'half-carry?)
   (constant nil) (assign-flag 'subtraction?)))

(define ((bit-set-analyzer bit-op) instr)
  "Analyzer which updates a byte: [bit-op byte index] => byte ( -- )"
  (seq
   (analyze-source-spec (bit-op-source instr))
   (constant (bit-op-index instr))
   (perform 2 bit-op)
   (analyze-dest-spec (bit-op-source instr))))

(define (analyze-jp-address instr)
  "Analysis for (JP address) ( -- )"
  (seq (constant (goto-dest instr))
       (goto)))

(define (analyze-jphl instr)
  "Analysis for (JP (HL)) ( -- )"
  (declare (ignore instr))
  (seq (reg 'hl) (assign-reg 'pc)))

(define (analyze-jp instr)
  "Analysis for (JP dest) ( -- )"
  (cond
    ;; TODO: new instruction for jphl
    ((equal? instr '(jp (hl))) (analyze-jp-hl))
    (t (analyze-jp-address instr))))

(define (analyze-condition condition)
  "condition := flag-name
              | (not flag-name).
Analysis which pushes the appropriate boolean value. ( -- flag? )"
  (cond
    ((flag-name? condition) (flag condition))
    ((and (list? condition) (eq? (first condition) 'not))
     (let ((flag-name (second condition)))
       (assert (and (= 2 (length condition))
		    (flag-name? flag-name)))
       (seq (flag flag-name)
	    (perform 1 'not))))
    (t (error "Unrecognized condition ~S" condition))))

(define-examples analyze-condition
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-condition '(not zero?)))
			   0 1))
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-condition 'carry?))
			   0 1)))

(define (analyze-jpc instr)
  "Analysis for (JPC condition address)"
  (seq
   (analyze-condition (branch-condition instr))
   (constant (branch-dest instr))
   (branch)))

(define (analyze-relative-address offset)
  "Analysis which pushes the absolute address given the offset from the PC. ( -- address )"
  (seq (constant offset) (reg 'pc) (perform 2 '+)))

(define-examples analyze-relative-address
  ((stack-effects-of-size? (machine-op-stack-effects (analyze-relative-address 42))
			   0 1)))

(define (analyze-jr instr)
  "Analysis for (JR address)"
  (seq (analyze-relative-address (goto-dest instr))
       (goto)))

(define (analyze-jrc instr)
  "Analysis for (JR condition address)"
  (seq (analyze-relative-address (branch-dest instr))
       (analyze-condition (branch-condition instr))
       (branch)))

(define (analyze-call instr)
  "Analysis for (CALL address)"
  (seq (analyze-push-reg 'pc)
       (constant (goto-dest instr))
       (goto)))
(define (analyze-ret instr)
  "Analysis for (RET)"
  (declare (ignore instr))
  (seq (analyze-pop-reg 'pc)
       (goto)))
(define (analyze-reti instr)
  "Analysis for (RETI)"
  (declare (ignore instr))
  (seq (analyze-pop-reg 'pc)
       (goto)
       (enable-interrupts)))

(define (analyze-callc instr)
  "Analysis for (CALLC condition address)"
  (seq (analyze-condition (branch-condition instr))
       (alt (seq (analyze-push-reg 'pc)
		 (constant (branch-dest instr))
		 (goto))
	    ())))
(define (analyze-retc instr)
  "Analysis for (RETC condition)"
  (seq (analyze-condition (retc-condition instr))
       (alt (seq (analyze-pop-stack)
		 (goto))
	    (nop))))

;; registry

;; Transfer
(register! 'ld   'analyze-ld)
(register! 'ldhl 'analyze-ldhl)
(register! 'push 'analyze-push)
(register! 'pop  'analyze-pop)

;; Arithmetic
(register! 'add 'analyze-add)
(register! 'adc (arithmetic-analyzer 'add8-op t))
(register! 'sub (arithmetic-analyzer 'sub-op nil))
(register! 'sbc (arithmetic-analyzer 'sub-op t))

;; Logical
(register! 'and (logical-analyzer 'and-op))
(register! 'or  (logical-analyzer 'or-op))
(register! 'xor (logical-analyzer 'xor-op))
(register! 'cp  'analyze-cp)

;; Update
(register! 'inc (update-analyzer '1+))
(register! 'dec (update-analyzer '1-))

;; Rotate/Shift
(register! 'rlca (shift-analyzer 'rlca-op nil))
(register! 'rla  (shift-analyzer 'rla-op t))
(register! 'rrca (shift-analyzer 'rrca-op nil))
(register! 'rra  (shift-analyzer 'rra-op t))
(register! 'rl   (shift-analyzer 'rl-op t))
(register! 'rlc  (shift-analyzer 'rlc-op nil))
(register! 'rr   (shift-analyzer 'rr-op t))
(register! 'rr   (shift-analyzer 'rrc-op nil))

(register! 'sla  (shift-analyzer 'sla-op nil))
(register! 'sra  (shift-analyzer 'sra-op nil))
(register! 'srl  (shift-analyzer 'srl-op nil))

(register! 'swap (shift-analyzer 'swap-op nil))

;; Bit Instructions
(register! 'bit 'analyze-bit)
(register! 'set (bit-set-analyzer 'bit-set))
(register! 'res (bit-set-analyzer 'bit-reset))

;; Jump Instructions
(register! 'jp  'analyze-jp)
(register! 'jpc 'analyze-jpc)
(register! 'jr  'analyze-jr)
(register! 'jrc 'analyze-jrc)

;; Call Instructions

;;TODO:...
(register! 'call  'analyze-call)
(register! 'callc 'analyze-callc)
(register! 'ret   'analyze-ret)
(register! 'reti  'analyze-reti)
(register! 'retc  'analyze-retc)
(register! 'rst   'analyze-rst)

;; TODO: HLD HLI
;; TODO: instruction size and cycle counts
