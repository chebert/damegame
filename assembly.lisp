(unless (find-package :assembly)
  (make-package :assembly :use '(:schemeish.schemeish)))

(in-package :assembly)

(extend-package :assembly
		(document "Provides names, constructors and accessors for assembly-language instructions."))

(define (tagged? list name)
  "True if list begins with name."
  (eq? (car list) name))
(define ((make-tagged? name) spec)
  "Return a predicate which returns true if tagged with name"
  (tagged? spec name))

(defmacro def (name constructor)
  (let ((make-name (intern (string-append "MAKE-" (symbol->string name)))))
    `(progn
       (define ,make-name (,constructor ',name))
       (list ',name ',make-name))))

;; (UNIT-INSTRUCTION)
(define ((make-unit name)) `(,name))

;; (assign-INSTRUCTION dest source)
(define ((make-assign name) dest source) `(,name ,dest ,source))
(export (define assign-dest #'second))
(export (define assign-source #'third))

;; (GOTO-INSTRUCTION dest)
(define ((make-goto name) dest) `(,name ,dest))
(export (define goto-dest #'second))

;; (BRANCH-INSTRUCTION condition dest)
(define ((make-branch name) condition dest) `(,name ,condition ,dest))
(export (define branch-condition #'second))
(export (define branch-dest #'third))

;; (UPDATE-INSTRUCTION source/dest)
(define ((make-update name) dest) `(,name ,dest))
(export (define update-dest #'second))

;; (BIT-OP index source)
(define ((make-bit-op name) index source) `(,name ,index ,source))
(export (define bit-op-index #'second))
(export (define bit-op-source #'third))

;; Assign-Instructions (name dest source)
(export (def add make-assign))
(export (def adc make-assign))
(export (def sub make-assign))
(export (def sbc make-assign))
(export (def and make-assign))
(export (def xor make-assign))
(export (def or  make-assign))
(export (def cp  make-assign))
(export (def ld  make-assign))

;; Update Instructions (name source/dest)
(export (def inc  make-update))
(export (def dec  make-update))
(export (def rlc  make-update))
(export (def rl   make-update))
(export (def rrc  make-update))
(export (def rr   make-update))
(export (def sla  make-update))
(export (def sra  make-update))
(export (def srl  make-update))
(export (def swap make-update))

;; Bit instructions (name index source) 
(export (def bit make-bit-op))
(export (def set make-bit-op))
(export (def res make-bit-op))

;; Goto instructions (name dest)
(export (def jp   make-goto))
(export (def jr   make-goto))
(export (def call make-goto))
(export (def rst  make-goto))

;; Branch instructions (name condition dest)
(export (def jpc   make-branch))
(export (def jrc   make-branch))
(export (def callc make-branch))

;; Unit instructions (name)
(export (def ret  make-unit))
(export (def reti make-unit))
(export (def rlca make-unit))
(export (def rla  make-unit))
(export (def rrca make-unit))
(export (def rra  make-unit))
(export (def daa  make-unit))
(export (def cpl  make-unit))
(export (def nop  make-unit))
(export (def ccf  make-unit))
(export (def scf  make-unit))
(export (def di   make-unit))
(export (def ei   make-unit))
(export (def halt make-unit))
(export (def stop make-unit))

;; MISC
;; (PUSH source)
(export (define (make-push source-spec) `(push ,source-spec)))
(export (define push-source #'second))

;; (POP dest)
(export (define (make-pop dest-spec) `(pop ,dest-spec)))
(export (define pop-dest #'second))

;; (LDHL sp offset)
(export (define (make-ldhl offset) `(ldhl sp ,offset)))
(export (define ldhl-offset #'third))

;; (RETC condition)
(export (define (make-retc condition) `(retc ,condition)))
(export (define retc-condition #'second))

(export
 (define (high-register-name register-pair-name)
   "Returns the high-register-name given a register-pair AF, BC, DE, or HL."
   (cond
     ((eq? register-pair-name 'af) 'a)
     ((eq? register-pair-name 'bc) 'b)
     ((eq? register-pair-name 'de) 'd)
     ((eq? register-pair-name 'hl) 'h)
     (t (error "Not a register-pair name ~S" register-pair-name)))))
(export
 (define (low-register-name register-pair-name)
   "Returns the low-register-name given a register-pair AF, BC, DE, or HL."
   (cond
     ((eq? register-pair-name 'af) 'f)
     ((eq? register-pair-name 'bc) 'c)
     ((eq? register-pair-name 'de) 'e)
     ((eq? register-pair-name 'hl) 'l)
     (t (error "Not a register-pair name ~S" register-pair-name)))))

(export
 (define (register16? register-name)
   "True if register-name names a 16-bit register"
   (member register-name '(af bc de hl sp pc))))
(export
 (define (register-pair? register-name)
   "True if register-name names a 16-bit register pair"
   (member register-name '(af bc de hl))))
(export
 (define (register8? register-name)
   "True if register-name names an 8-bit register"
   (member register-name '(a b c d e f h l))))

(export
 (define (register-name? v)
   (member v '(a b c d e f h l af bc de hl sp pc))))
(export
 (define (flag-name? v)
   (member v '(carry? half-carry? subtraction? zero?))))

(export '(push pop ldhl retc))
(export '(a b c d e f h l af bc de hl sp pc carry? half-carry? subtraction? carry))
