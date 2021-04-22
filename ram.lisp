(in-package #:ram)

(define (make-memory-vector length)
  "Returns a vector of u8s with the given length."
  (make-array length :initial-element 0 :element-type '(unsigned-byte 8)))

(for-macros
  (define (region-spec-end-address region-spec)
    (first region-spec))
  (define (region-spec-body region-spec)
    (rest region-spec))

  (define (with-mapped-address-form address-form mapped-address-name region-specs)
    (let ((address-name (gensym "ADDRESS")))
      `(let ((,address-name ,address-form))
	 (cond
	   ,@(map-successive
	      2
	      (lambda (previous-region-spec region-spec)
		(let ((start-address (region-spec-end-address previous-region-spec))
		      (end-address (region-spec-end-address region-spec))
		      (body (region-spec-body region-spec)))
		  (if (eq? t end-address)
		      `(t ,@body)
		      `((< ,address-name ,end-address)
			(let ((,mapped-address-name (- ,address-name ,start-address)))
			  (declare (ignorable ,mapped-address-name))
			  ,@body)))))
	      (cons `(0) region-specs)))))))


(defmacro with-mapped-address (address mapped-address-name &body region-specs)
  "Evaluate the body of the appropriate region-spec with the given address mapped to the region.
region-spec := (end-address . body) | (t . body)
Evaluates body with the provided mapped-address-name bound to the relative address.
Each region starts at the end of the previous region.
The default case is (t . body) and is evaluated if no other region matches."
  (with-mapped-address-form address mapped-address-name region-specs))

(define-examples with-mapped-address
  ((map (lambda (address)
	  (with-mapped-address address mapped-address
	    (100 (list 0 mapped-address))
	    (200 (list 1 mapped-address))
	    (300 (list 2 mapped-address))
	    (t :out-of-bounds)))
	'(50 100 201 300))
   '((0 50) (1 0) (2 1) :OUT-OF-BOUNDS)))

(define internal-ram? (make-bundle-predicate :internal-ram))
(define (make-internal-ram
	 (character-ram (make-memory-vector #x1800))
	 (background-map-data-1 (make-memory-vector #x400))
	 (background-map-data-2 (make-memory-vector #x400))
	 (internal-ram (make-memory-vector #x2000))
	 (object-attribute-memory (make-memory-vector #xa0))
	 (hardware-registers (make-memory-vector #x80))
	 (high-ram (make-memory-vector #x7f))
	 (interrupt-enable-flag (make-memory-vector 1)))
  "Creates the internal ram that's part of the gameboy itself (not the cartridge, and not ROM).
RAM can be accessed using global addresses using byte-getter and byte-setter, or individual regions
of memory can be retrieved."

  (define (memory-map address)
    "Return (values memory-vector relative-address)"
    (with-mapped-address address mapped-address
      (#x8000 (error "RAM: Cannot access ROM at ~S" address))
      (#x9800 (values character-ram mapped-address))
      (#x9C00 (values background-map-data-1 mapped-address))
      (#xA000 (values background-map-data-2 mapped-address))
      (#xC000 (error "RAM: Cannot access cartridge RAM at ~S" address))
      (#xE000 (values internal-ram mapped-address))
      (#xFE00 (error "RAM: Cannot access Echo RAM at ~S" address))
      (#xFEA0 (values object-attribute-memory mapped-address))
      (#xFF00 (error "RAM: Cannot acess unusable memory at ~S" address))
      (#xFF80 (values hardware-registers mapped-address))
      (#xFFFF (values high-ram mapped-address))
      (#x10000 (values interrupt-enable-flag mapped-address))
      (t (error "RAM: Cannot access memory at ~S" address))))

  (define (byte-getter address)
    "Returns a (lambda () byte) which returns the byte at the given global address when called."
    (multiple-value-bind (memory-vector address) (memory-map address)
      (lambda ()
	(aref memory-vector address))))

  (define (byte-setter address)
    "Returns a (lambda (byte)) which sets the byte at the given global address."
    (multiple-value-bind (memory-vector address) (memory-map address)
      (lambda (value)
	(setf (aref memory-vector address) value))))

  (define (get-character-ram) character-ram)
  (define (get-background-map-data-1) background-map-data-1)
  (define (get-background-map-data-2) background-map-data-2)
  (define (get-internal-ram) internal-ram)
  (define (get-object-attribute-memory) object-attribute-memory)
  (define (get-hardware-registers) hardware-registers)
  (define (get-high-ram) high-ram)
  (define (get-interrupt-enable-flag-ram) interrupt-enable-flag)

  (bundle 'internal-ram? (make-internal-ram
			  character-ram
			  background-map-data-1
			  background-map-data-2
			  internal-ram
			  object-attribute-memory
			  hardware-registers
			  high-ram
			  interrupt-enable-flag)
	  byte-getter
	  byte-setter
	  
	  get-interrupt-enable-flag-ram
	  get-high-ram
	  get-hardware-registers
	  get-object-attribute-memory
	  get-internal-ram
	  get-background-map-data-2
	  get-background-map-data-1
	  get-character-ram))
