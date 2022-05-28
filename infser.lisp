;;; (load (compile-file "infser.lisp"))

(defun s1 (n)
  (if (= n 1.0)
      0.50
    (+ (/ n (+ n 1))
       (s1 (- n 1)))))

(defun s2 (n &optional (m 1))
  (- (s2b n m) 1))
  
(defun s2b (n m)
  (if (= m n) (float m)
    (+ m (/ m (s2 n (+ 1 m))))))

(untrace)
