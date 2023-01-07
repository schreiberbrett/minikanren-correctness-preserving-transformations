(load "~/pmatch.scm")

;; defrel-optimized
;; A macro to be used instead of defrel
(define-syntax defrel-optimized
    (syntax-rules ()
        ((_ (name args ...) body ...)
            (letrec* (
                (arg-list (list 'args ...))
                (body-list (list 'body ...)))

            (eval (perform-cpts `(defrel (name args ...) . (body ...))))))))


;; Correctness-preserving transformations
;; --------------------------------------

;; perform-cpts : Defrel -> Defrel
;; Performs all correctness-preserving transformations in a productive order.
(define (perform-cpts d)
    (optimize-condes
    (single-fresh
        d)))


;; single-fresh : Defrel -> Defrel
;; Consolidate all fresh variables into one top-level fresh declaration.
;; Assumes that no two fresh variables with the same name.
(define (single-fresh d) (pmatch d ((defrel ,args . ,exps)
    (letrec* (

        ;; gather-fresh-vars :: [Exp] -> [Symbol]
        (gather-fresh-vars (lambda (exps)
            (append-map (lambda (exp) (pmatch exp
                ((conde . ,clauses) (append-map gather-fresh-vars clauses))
                ((fresh ,vars . ,fresh-exps) (append vars (gather-fresh-vars fresh-exps)))
                ((,relation . ,args) '()))) exps)))

        ;; remove-fresh-decls :: [Exp] -> [Exp]
        (remove-fresh-decls (lambda (exps)
            (append-map (lambda (exp) (pmatch exp
                ((conde . ,clauses) `((conde . ,(map remove-fresh-decls clauses))))
                ((fresh ,vars . ,fresh-exps) (remove-fresh-decls fresh-exps))
                ((,relation . ,args) `((,relation . ,args))))) exps)))

        (fresh-vars (gather-fresh-vars exps))
        (new-exps (remove-fresh-decls exps)))

    (if (null? fresh-vars)
        `(defrel ,args . ,exps)
        `(defrel ,args . ((fresh ,fresh-vars . ,new-exps))))))))

;; optimize-condes : Defrel -> Defrel
;; Extract any relation that appears in multiple conde clauses and put it after the conde.
;; e.g., (P(x) ∧ Q(y)) ∨ (P(z) ∧ R(w)) ⇒ ∃α.((α≡x ∧ Q(y)) ∨ (α≡z ∧ R(w))) ∧ P(α)
(define (optimize-condes d)
    (pmatch d
        ((defrel ,args . ,exps)
            `(defrel ,args . ,(map optimize-conde exps)))))


;; Helper definitions for `optimize-condes`
;; ----------------------------------------

;; optimize-conde : Exp -> Exp
(define (optimize-conde exp)
    (pmatch exp
        ((fresh ,vars . ,exps)
            `(fresh ,vars . ,(map optimize-conde exps)))
        ((conde . ,exps*)
            `(conde . ,(optimize-conde-exps*
                (map (lambda (exps)
                    (map optimize-conde exps)) exps*))))
        ((,rel . ,args)
            `(,rel . ,args))))


;; maybe-most-frequent-relation : [[Exp]] -> Maybe Symbol
(define (maybe-most-frequent-relation exps*)
    (letrec* (
        ;; contains-relation? : [Exp] -> Symbol -> Bool
        (contains-relation? (lambda (exps rel)
            (and 
                (pair? exps)
                (or
                    (pmatch (car exps)
                        ((conde . ,clauses) #f)
                        ((fresh ,vars . ,fresh-exps) #f)
                        ((,relation . ,args) (equal? rel relation)))

                    (contains-relation?
                        (cdr exps) rel)))))

        ;; frequency : Symbol -> Nat
        (frequency (lambda (rel)
            (length (filter (lambda (exps)
                (contains-relation? exps rel)) exps*))))

        ;; all-relations : [Symbol]
        (all-relations
            (append-map (lambda (exps) (append-map (lambda (exp)
                (pmatch exp
                    ((conde . ,clauses) '())
                    ((fresh ,vars . ,fresh-exps) '())
                    ((== ,x ,y) '())
                    ((,relation . ,args) `(,relation)))) exps)) exps*)))

    (pmatch (max-by frequency all-relations)
        ((nothing) '(nothing))
        ((just ,rel) (if (equal? (frequency rel) 1) '(nothing) `(just ,rel))))))


;; optimize-conde-exps* : [[Exp]] -> [[Exp]]
(define (optimize-conde-exps* exps*)
    (letrec* (
        ;; arity : relation -> nat
        (arity (lambda (relation) (car (append-map
            (lambda (exps)
                (append-map
                    (lambda (exp) (pmatch exp
                        ((conde . ,clauses) '())
                        ((fresh ,vars . ,clauses) '())
                        ((,rel . ,args)
                            (if (equal? relation rel)
                                `(,(length args))
                                '()))))
                    exps))
            exps*)))))

        (pmatch (maybe-most-frequent-relation exps*)
            ((nothing) exps*)
            ((just ,most-frequent-relation)
                (extract-common-relation
                    most-frequent-relation
                    (map (lambda (x) (gensym)) (iota (arity most-frequent-relation)))
                    exps*)))))

;; extract-common-relation :: Symbol -> [Symbol] -> [[Exp]] -> [[Exp]]
(define (extract-common-relation rel fresh-args clauses)
    (letrec* (
        ;; partition : ([([Exp], Exp, [Exp])], [[Exp]])
        (partition (partition-justs
            (lambda (exps)
                (split-on-just
                    (lambda (exp) (pmatch exp
                        ((fresh ,args . ,exps) '(nothing))
                        ((conde . ,conde-clauses) '(nothing))
                        ((,r . ,args) (if (equal? r rel)
                            `(just ,(zip-with (lambda (arg fresh-arg) `(== ,arg ,fresh-arg)) args fresh-args))
                            '(nothing)))))
                    exps)) 
            clauses))

        ;; excludes-rel : [[Exp]]
        (excludes-rel (optimize-conde-exps* (cdr partition)))

        ;; includes-rel : [[Exp]]
        (includes-rel (optimize-conde-exps*
            (map
                (lambda (triple) (pmatch triple
                    ((,before ,just ,after) (append just before after))))
                (car partition)))))

    (if (null? excludes-rel)
        `(
            ((fresh ,fresh-args
                (conde . ,includes-rel)
                (,rel . ,fresh-args)))
        )

        `(
            ((fresh ,fresh-args
                (conde . ,includes-rel)
                (,rel . ,fresh-args)))

            ((conde . ,excludes-rel))
        ))))


;; Miscellaneous helper functions
;; ------------------------------

;; append-map : (a -> [b]) -> [a] -> [b]
(define (append-map f l)
    (cond
        ((null? l) '())
        ((pair? l) (append (f (car l)) (append-map f (cdr l))))))


;; max-by : (a -> Int) -> [a] -> Maybe a
(define (max-by f l)
    (pmatch l
        (() '(nothing))
        ((,h . ,t) (pmatch (max-by f t)
            ((nothing) '(just ,h))
            ((just ,m) `(just ,(if (> (f h) (f m)) h m)))))))


;; zip-with :: (a -> b -> c) -> [a] -> [b] -> [c]
(define (zip-with f l1 l2)
    (pmatch `(,l1 . ,l2)
        ((() . ()) '())
        (((,h1 . ,t1) . (,h2 . ,t2))
            `(,(f h1 h2) . ,(zip-with f t1 t2)))))

;; partition-justs :: (a -> Maybe b) -> [a] -> ([b], [a])
(define (partition-justs f l)
    (pmatch l
        (() `(() . ()))
        ((,h . ,t) (pmatch (partition-justs f t)
            ((,justs . ,nothings) (pmatch (f h)
                ((nothing) `(,justs . (,h . ,nothings)))
                ((just ,just) `((,just . ,justs) . ,nothings))))))))

;; split-on-just :: (a -> Maybe b) -> [a] -> ([a], b, [a])
(define (split-on-just f l)
    (pmatch l
        (() '(nothing))
        ((,h . ,t) (pmatch (f h)
            ((just ,just) `(just (() ,just ,t)))
            ((nothing) (pmatch (split-on-just f t)
                ((nothing) '(nothing))
                ((just (,before ,just ,after)) `(just ((,h . ,before) ,just ,after)))))))))