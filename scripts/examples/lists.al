;; (dump (head '("a" "b" "c")))

;; (dump (last '("a" "b" "c")))

;; (dump (car '("a" "b" "c")))

;; (dump (cons '("a" "b" "c")))

;; (dump (tail '("a" "b" "c")))

;; (dump (init '("a" "b" "c")))

;; (mapc dump (init '("a" "b" "c")))


;; (dump (push '("a" "b" "c") "d" ))
;; (dump (push '("a" "b" "c") 52 ))


;; (dump (delete '("a" "b" "c") "a" ))
;; (dump (delete '("a" "b" "c" 123 ) 123))
;; (dump (delete '(a b c ) 'b))

;; (dump (remove '("a" "b" "c") "a" ))
;; (dump (remove '("a" "b" "c" 123 ) 123))
;; (dump (remove '(a b c ) 'b))


;; (defun list-example()
;;   (let (a b (c 10))
;;     (when 't
;;       (dump (nth '(0 1 2 3 4 5 6 7) 7)))))

;; (list-example)


;; (mapc (lambda (x) (dump x)) (zip (range 1 100) (range 100 200)))
;; (mapc dump (zip (range 1 100) (range 100 200)))
;; (dump (filter (lambda (x) (== (mod x 2) 1)) '(1 2 3 4 5 2 2 3)))
;; (dump (any (lambda (x) (== (mod x 2) 3)) '(1 2 3 4 5 2 2 3)))

(dump (slice '(1 2 3 4 5 6 7 8 9) 1 5))



