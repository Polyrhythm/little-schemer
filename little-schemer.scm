(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
		 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons
	      (car lat)
	      (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new lat))
       (else (cons
	      (car lat)
	      (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
	  (else
	   (cond
	    ((eq? (car lat) old) (cons new (cdr lat)))
	    (else (cons
		   (car lat)
		   (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((or
	 (eq? (car lat) o1)
	 (eq? (car lat) o2))
	(cons new (cdr lat)))
       (else
	(cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
	  (else
	   (cond
	    ((eq? (car lat) a) (multirember a (cdr lat)))
	    (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
	  (else
	   (cond
	    ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
	    (else
	     (cons
	      (car lat)
	      (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons
			     new
			     (cons old (multiinsertL new old (cdr lat)))))
       (else
	(cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else
	(cons (car lat) (multisubst new old (cdr lat)))))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons
       (o+ (car tup1) (car tup2))
       (tup+ (cdr tup1) (cdr tup2)))))))

(define lessthan
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (lessthan (sub1 n) (sub1 m))))))

(define greaterthan
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (greaterthan (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((zero? n) (zero? m))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

(define nth
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (x n (nth n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (div (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
	(cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else
	(all-nums (cdr lat))))))))

(define egan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? (car lat) a) (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old (cons new (insertR* new old (cdr l)))))
       (else
	(cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else
	(cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (cons old (insertL* new old (cdr l)))))
       (else
	(cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (or
       (eq? (car l) a)
       (member* a (cdr l))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
    (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and
       (equal? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (egan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp)))))))))

(define o^
  (lambda (x y)
    (cond
     ((zero? y) 1)
     (else
      (* x (o^ x (sub1 y)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) '+)
      (o+ (value (cdr nexp)) (value (cdr (cdr nexp)))))
     ((eq? (car nexp) 'x)
      (x (value (cdr nexp)) (value (cdr (cdr nexp)))))
     (else
      (o^ (value (cdr nexp)) (value (cdr (cdr nexp))))))))
