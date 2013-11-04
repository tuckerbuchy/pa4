#lang plai-typed

(require "typed-lang2.rkt")

;;;;;;;;;; Store Definitions ;;;;;;;;;;;;;;
;; Please consider the interface to the store to be:
;; - the types Location, Cell, and Store
;;   (and associated functions like cell-location)
;; - the variable empty-store
;; - the store-lookup, fresh-loc, update-store,
;;   and alloc-store functions
;;
;; TODO: complete fresh-loc, and alloc-store!

(define-type Cell
 [cell (location : Location) (value : ValueC)])

(define-type-alias Store (listof Cell))
(define empty-store empty)

; find the value stored at the given location
; (with an error on an unallocated location)
(define (store-lookup [locn : Location] [store : Store]) : ValueC
 (cond [(empty? store)
        (interp-error (string-append "Unallocated cell: "
                                     (to-string locn)))]
       [(= locn (cell-location (first store)))
        (cell-value (first store))]
       [else
        (store-lookup locn (rest store))]))

; produce a fresh location not presently in the store
(define (fresh-loc [store : Store]) : Location
  (length store))
 ;; Hint: can you find out what the largest
 ;; location in the store is?  Once you have
 ;; that, can you calculate a fresh location?

; update the store to contain the given value at the given location
; PRESENTLY: no check that the location already exists
(define (update-store [locn : Location]
                     [value : ValueC]
                     [store : Store]) : Store
 (cons (cell locn value) store))

; allocate the given value at a new location in the store,
; producing the location used and the new store.
;
; to get the Location and Store out of the result, use
; code like:
;
; (local [(define-values (locn store) (alloc-store ...))]
;   ...)
;
; PRESENTLY: no check that the location already exists
; We may want to add that in the future.
(define (alloc-store [value : ValueC]
                    [store : Store]) : (Location * Store)
  (let ([new-loc (fresh-loc store)])
    (values new-loc (update-store new-loc value store))))


(module+ test
  (local ((define store0 empty-store)
          (define-values (locn1 store1) (alloc-store (NumV 1) store0))
          (define-values (locn2 store2) (alloc-store (NumV 2) store1))
          (define-values (locn3 store3) (alloc-store (NumV 3) store2)))
    
    ; Sadly, these don't work because of the implementation of interp-error.
    ;(test/exn (store-lookup (fresh-loc store0) store0) "Unallocated cell:")
    ;(test/exn (store-lookup (fresh-loc store1) store1) "Unallocated cell:")
    ;(test/exn (store-lookup (fresh-loc store2) store2) "Unallocated cell:")
    ;(test/exn (store-lookup (fresh-loc store3) store3) "Unallocated cell:")
    
    ; Instead, we'll do this.
    (begin
      (test (member (fresh-loc store1) (list locn1)) false)
      (test (member (fresh-loc store2) (list locn1 locn2)) false)
      (test (member (fresh-loc store3) (list locn1 locn2 locn3)) false)
      
      (test (store-lookup locn1 store1) (NumV 1))
      (test (store-lookup locn1 store2) (NumV 1))
      (test (store-lookup locn2 store2) (NumV 2))
      (test (store-lookup locn1 store3) (NumV 1))
      (test (store-lookup locn2 store3) (NumV 2))
      (test (store-lookup locn3 store3) (NumV 3))
      (test (store-lookup locn1 (update-store locn1 (NumV -1) store1))
            (NumV -1))
      (test (store-lookup locn1 (update-store locn1 (NumV -1) store3))
            (NumV -1))
      (test (store-lookup locn3 (update-store locn1 (NumV -1) store3))
            (NumV 3))
      (test (store-lookup locn1 (update-store locn3 (NumV -3) store3))
            (NumV 1))
      (test (store-lookup locn3 (update-store locn3 (NumV -3) store3))
            (NumV -3)))))

(define-type Result
  [v*s (value : ValueC) (store : Store)])


;;;;;;;;;;;;;;;;;;HELPERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp-addition [v-arg1 : ValueC] [v-arg2 : ValueC]) : ValueC 
  (NumV (+ (NumV-n v-arg1) (NumV-n v-arg2))))
(define (interp-string-addition [v-arg1 : ValueC] [v-arg2 : ValueC]) : ValueC 
  (StrV (string-append (StrV-s v-arg1) (StrV-s v-arg2))))
(define (interp-subtraction [v-arg1 : ValueC] [v-arg2 : ValueC]) : ValueC 
  (NumV (- (NumV-n v-arg1) (NumV-n v-arg2))))

(define (interp-equals v-arg1 v-arg2 s-arg2) : Result
  (cond 
    [(equal? v-arg1 v-arg2) (v*s (TrueV) s-arg2)]
    [else (v*s (FalseV) s-arg2)]))

(define (interp-compare v-arg1 v-arg2 s-arg2 op) : Result
     (cond
       [(op (NumV-n v-arg1) (NumV-n v-arg2)) (v*s (TrueV) s-arg2)]
       [else (v*s (FalseV) s-arg2)]))

; Function to convert a list of FieldC fields to a lits of the identifier strings from each FieldC.
(define (fields_to_stringlist [fields : (listof FieldC)] [acc : (listof string)]) : (listof string)
  (cond
    [(empty? fields) acc]
    [else (fields_to_stringlist (rest fields) (cons (fieldC-name (first fields)) acc))]))

; Function to check if there are duplicates in a list.
(define (check-duplicates [fieldnames : (listof string)]) : ValueC
  (cond
    [(empty? fieldnames) (FalseV)]
    [(member (first fieldnames) (rest fieldnames)) (TrueV)]
    [else (check-duplicates (rest fieldnames))]))

; Function to interp a list of fields.
(define (interp-listof-fields [fields : (listof FieldC)] [env : Env] [store : Store] [acc : (listof FieldV)]): Result
  (cond 
    [(empty? fields) (v*s (ObjectV acc) store)]
    [else (type-case Result (interp-full (fieldC-value (first fields)) env store)
            [v*s (v-f s-f) (interp-listof-fields (rest fields) env s-f (cons (fieldV (fieldC-name (first fields)) v-f) acc))])]))

; Helper function for interpreting getfield.
(define (interp-getfield [fields : (listof FieldV)] [s : string] [store : Store]) : Result
  (cond 
    [(empty? fields) (interp-error (string-append "Field not found: " s))]
    [(string=? (fieldV-name (first fields)) s) (v*s (fieldV-value (first fields)) store)]
    [else (interp-getfield (rest fields) s store)]))

; Helper function for interpreting setfield.
(define (interp-setfield [fields : (listof FieldV)] [s : string] [value : ValueC] [env : Env] [store : Store] [acc : (listof FieldV)]) : Result  
  (cond 
    ; In the case that the field does not exist...
    [(empty? fields) (v*s (ObjectV (cons (fieldV s value) acc)) store)]
    ; If we find the field then...
    [(string=? (fieldV-name (first fields)) s) (v*s (ObjectV (append (rest fields) (cons (fieldV s value) acc))) store)]
    ; Otherwise keep looking...
    [else (interp-setfield (rest fields) s value env store (cons (first fields) acc))]))

; Function to create the desired number of new locations in a given store.
(define (create-locns [numlocns : number] [acc : (listof Location)] [store : Store]) : (listof Location)
                              (cond
                                [(< numlocns 0) empty]
                                [(= numlocns 0) acc]
                                [else (create-locns (- numlocns 1) (cons (fresh-loc store) acc) store)]
                                ))

; Function to extend the given environment with a list of identifiers and locations.
(define (extend-env-bylists [ids : (listof symbol)] [locns : (listof Location)] [env : Env]) : Env
  (cond 
    [(empty? ids) env]
    [else (extend-env-bylists (rest ids) (rest locns) (extend-env (first ids) (first locns) env))]
    ))

; Function to update the given store with a list of locations and values.
(define (update-store-bylists [locns : (listof Location)] [vals : (listof ValueC)] [store : Store]) : Store
  (cond 
    [(empty? locns) store]
    [else (update-store-bylists (rest locns) (rest vals) (update-store (first locns) (first vals) store))]
    ))

; Helper function that evaluates a list of ExprC expressions to ValueC expressions.
(define (map-interp [exprs : (listof ExprC)] [acc : (listof ValueC)] [env : Env] [store : Store]) : (listof ValueC)
  (cond
    [(empty? exprs) acc]
    [else (map-interp (rest exprs) (cons (v*s-value (interp-full (first exprs) env store)) acc) env store)]))

; A helper function that will apply a given function definition to a given list of parameter arguments.
(define (Apply [func : ValueC] [args : (listof ExprC)] [env : Env] [store : Store]) : Result
  (type-case ValueC func
    ; Be sure that a function was passed in.
    [ClosureV (f-args f-body f-env) 
              ; Evaluate the parameter arguments passed in.
              (let ([vals (map-interp args empty env store)])
                (cond
                  ; Check that the number of arguments passed in matches the number of parameters for the function.
                  [(= (length args) (length f-args)) 
                   ; Create n new locations that don't exist in store
                   (let ([locns (create-locns (length args) empty store)])
                     ; Evaluate body to value
                     ; Yield v as the result of the entire expression.
                     ; current environment to env
                     (interp-full f-body
                      ; add bindings to env' -> add bindings to env'
                      ; current environment to env'
                      (extend-env-bylists f-args locns env)
                      ; update store' -> update store'
                      ; current store to store'
                      (update-store-bylists locns vals store)))]
                  ; Throw an arity mismatch error if the number of arguments and parameters does not match.
                  [else (interp-error "Application failed with arity mismatch")]
                  )
                )]
    [else (interp-error (string-append "Applied a non-function: " (pretty-value func)))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
;; TODO: implement all remaining cases of ExprC; you will certainly
;;       want helper functions (like the Lookup metafunction
;;       described in the ParselTongue specifictaion!)    
    [NumC (n) (v*s (NumV n) store)]
    [StrC (s) (v*s (StrV s) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]
    [IdC (id) (v*s (store-lookup (env-lookup id env) store) store)]
    [SeqC (e1 e2) (type-case Result (interp-full e1 env store)
                    [v*s (v-e1 s-e1)
                         (interp-full e2 env s-e1)])]
    [Prim2C (op arg1 arg2) (type-case Result (interp-full arg1 env store)
                             [v*s (v-arg1 s-arg1) 
                                  (type-case Result (interp-full arg2 env s-arg1)
                                    [v*s (v-arg2 s-arg2) 
                                       (cond
                                         [(symbol=? op 'string+)
                                          (cond
                                            [(and (string=? (translate-to-type v-arg1) (translate-to-type v-arg2)) (string=? (translate-to-type v-arg1) "string")) 
                                             (v*s (interp-string-addition v-arg1 v-arg2) s-arg2)]
                                            [else (interp-error (string-append "Bad arguments for " (string-append (symbol->string op) (string-append":\n" (string-append (pretty-value v-arg1) (string-append "\n" (pretty-value v-arg2)))))))])]
                                         [(symbol=? op 'num+) 
                                          (cond
                                            [(and (string=? (translate-to-type v-arg1) (translate-to-type v-arg2)) (string=? (translate-to-type v-arg1) "number")) 
                                             (v*s (interp-addition v-arg1 v-arg2) s-arg2)]
                                            [else (interp-error (string-append "Bad arguments for " (string-append (symbol->string op) (string-append":\n" (string-append (pretty-value v-arg1) (string-append "\n" (pretty-value v-arg2)))))))])]                                         
                                         [(symbol=? op 'num-) 
                                          (cond
                                            [(and (string=? (translate-to-type v-arg1) (translate-to-type v-arg2)) (string=? (translate-to-type v-arg1) "number")) 
                                             (v*s (interp-subtraction v-arg1 v-arg2) s-arg2)]
                                            [else (interp-error (string-append "Bad arguments for " (string-append (symbol->string op) (string-append":\n" (string-append (pretty-value v-arg1) (string-append "\n" (pretty-value v-arg2)))))))])]

                                         [(symbol=? op '==) (interp-equals v-arg1 v-arg2 s-arg2)]
                                         
                                         [(symbol=? op '<) 
                                          (cond
                                            [(and (string=? (translate-to-type v-arg1) (translate-to-type v-arg2)) (string=? (translate-to-type v-arg1) "number")) 
                                             (interp-compare v-arg1 v-arg2 s-arg2 <)]
                                            [else (interp-error (string-append "Bad arguments for " (string-append (symbol->string op) (string-append":\n" (string-append (pretty-value v-arg1) (string-append "\n" (pretty-value v-arg2)))))))])]
                                         [(symbol=? op '>) (cond
                                            [(and (string=? (translate-to-type v-arg1) (translate-to-type v-arg2)) (string=? (translate-to-type v-arg1) "number")) 
                                             (interp-compare v-arg1 v-arg2 s-arg2 >)]
                                            [else (interp-error (string-append "Bad arguments for " (string-append (symbol->string op) (string-append":\n" (string-append (pretty-value v-arg1) (string-append "\n" (pretty-value v-arg2)))))))])]
                                         )])])]
    [Prim1C (op arg) (type-case Result (interp-full arg env store)
                       [v*s (v-arg s-arg)
                            (cond
                              [(symbol=? op 'print) (begin (display (pretty-value v-arg)) (v*s v-arg s-arg))]
                              [(symbol=? op 'tagof) (v*s (StrV (translate-to-type v-arg)) s-arg)]
                              [else (interp-error "error")])])]
    [LetC (s bind body) (type-case Result (interp-full bind env store)
                           [v*s (v-b s-b)
                               (let ([where (fresh-loc s-b)])
                                 (interp-full
                                  body
                                  (extend-env s where env)
                                  (update-store where v-b s-b)))])]
    [IfC (co th el) (type-case Result (interp-full co env store)
                            [v*s (v-cond s-cond)
                                 (type-case ValueC v-cond
                                   [FalseV () (interp-full el env s-cond)]
                                   [else (interp-full th env s-cond)])])]
    [ObjectC (fields) (type-case ValueC (check-duplicates (fields_to_stringlist fields empty))
                        [TrueV () (interp-error "Multiply-defined fields")]
                        [else (interp-listof-fields fields env store empty)])]
    
    [GetFieldC (objid fieldid) 
               (type-case Result (interp-full objid env store) 
                 [v*s (v-obj s-obj)
                      (type-case ValueC v-obj
                        [ObjectV (fields)
                              (type-case Result (interp-full fieldid env s-obj)
                                [v*s (v-field s-field) 
                                     (type-case ValueC v-field
                                       [StrV (s) (interp-getfield fields s s-field)]
                                       [else (interp-error (string-append "Non-string in field lookup: " (pretty-value v-field)))])])]
                        [else (interp-error (string-append "Non-object in field lookup: " (pretty-value v-obj)))])])]
    
    [SetFieldC (objid fieldid value)
               (type-case Result (interp-full objid env store)
                 [v*s (v-obj s-obj) 
                      (type-case ValueC v-obj
                        [ObjectV (fields) 
                                 (type-case Result (interp-full fieldid env store)
                                   [v*s (v-f s-f)
                                        (type-case ValueC v-f
                                          [StrV (s) 
                                                (type-case Result (interp-full value env s-f)
                                                  [v*s (v-val s-val)
                                                       (interp-setfield fields s v-val env store empty)])] 
                                          [else (interp-error (string-append "Non-string in field update: " (pretty-value v-f)))])])]
                        [else (interp-error (string-append "Non-object in field update: " (pretty-value v-obj)))])])]
    
    [Set!C (id value) 
           ; Evaluate the results.
           (type-case Result (interp-full value env store)
             ; Use the newly evaluated value and store.
             [v*s (v s)
                  ; Update the store with the new value using the old enviornment location.
                  (v*s v (update-store (env-lookup id env) v s))])]

    [ErrorC (expr) (type-case Result (interp-full expr env store)
                     [v*s (v s) (interp-error (pretty-value v))])]
    
    ;[FuncC (args : (listof symbol)) (body : ExprC)()]
    [FuncC (args body) (v*s (ClosureV args body env) store)]

    
    ;;[AppC (func : ExprC) (args : (listof ExprC))]
    
    [AppC (func args) 
          ; Evaluate function to a value
          (type-case Result (interp-full func env store)
            [v*s (v s) (Apply v args env store)])]
    
    ;[else (interp-error (string-append "Haven't covered a case yet:" (to-string exprC)))]
    ))






(define (interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))

(define (translate-to-type arg)
  (type-case ValueC arg
    [ObjectV (fs) "object"]
    [ClosureV (a b e) "function"]
    [NumV (n) "number"]
    [StrV (s) "string"]
    [TrueV () "boolean"]
    [FalseV () "boolean"]))
