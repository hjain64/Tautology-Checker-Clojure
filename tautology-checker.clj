(declare substitute)

(def op    
 (fn [l]
  (first l)))

(def left  
 (fn [l]
   (second l)))

(def right
 (fn [l]
   (last l )))

(def make-if
  (fn [& l]
    (if 
      (not (empty? l))
          (cons
             'if
             (cons (first l) (rest l)))
         (recur (rest l)))))
    
(def if-test
  (fn [l] 
    (second l)))

(def if-then
  (fn [l] 
    (first (drop 2 l) )))

(def if-else
  (fn [l]
   (last l)))

(def ifify
  (fn  [p]
     (cond
        (= p 'true)
          'true
        (= p 'false)
          'false
      (symbol? p)
	p
      (= (op p) 'not)
       (list 'if (ifify (left p)) 'false 'true)
      (= (op p) 'and)
	(list 'if (ifify (left p)) (ifify (right p)) 'false)
      (= (op p) 'or)
	(list 'if (ifify (left p)) 'true (ifify (right p))  )
      (= (op p) 'imply)
	(list 'if (ifify (left p)) (ifify (right p)) 'true)
      (= (op p) 'equiv)
	(list 'if (ifify (left p)) (ifify (right p)) (list 'if (ifify (right p)) 'false 'true))
       )))
       
(def normalize 
    (fn [c]
     (cond
        (= c 'true)
          'true
        (= c 'false)
          'false
       (symbol? c)
        c
      (and
        (list? c)
         (list? (if-test c)))  
             (normalize (list 'if (if-test (if-test c))
              (normalize (list 'if (if-then (if-test c)) (if-then c) (if-else c)))
                (normalize (list 'if (if-else (if-test c)) (if-then c) (if-else c))) ))         
       :else
           (list 'if (normalize (if-test c)) (normalize (if-then c)) (normalize (if-else c)) ) ))) 
       

         


(def simplify
  (fn [c]
    (cond
        (= c 'true)
          'true
        (= c 'false)
            'false
        (symbol? c)
            c
        (= (if-test c) 'true)
           (simplify (if-then c))
        (= (if-test c) 'false)
            (simplify (if-else c))
        (and
           (= (if-then c) 'true)
            (= (if-else c) 'false))
             (simplify (if-test c))
         (= (simplify (if-then c))(simplify (if-else c)))
            (simplify (if-else c))                 
          :else
         (simplify (list 'if (simplify (if-test c)) (substitute c (simplify (if-then c)) 'true) (substitute c (simplify (if-else c)) 'false))) )) )       


(def substitute
  (fn [c v b]
   (if
     (not (list? c))
       v
     (list 'if  b (substitute (if-test c) (if-then c) (if-else c)) )
       )))  

(def tautology?
  (fn [p]
   (if
     ( = (simplify (normalize (ifify p))) 'true)
       'true
         'false )))

