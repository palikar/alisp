
(defun even? (arg)
  (== 0 (mod arg 2)))

(defun odd? (arg)
  (!= 0 (mod arg 2)))

(defun square (arg)
  (* arg arg))



(defun d-map (fn list)
  (mapcar fn list))

;; (dump (d-map (lambda (x) (* x 2))'(1 2 3 4 5)))

(defun d-map-when (pred rep l)
  (let ((res (list)))
    (dolist (el l)
      (when (pred el)
        (push res (rep el))))
    res))

;; (dump (d-map-when (lambda (x) (== (mod x 2) 0)) (lambda (x) (* x 2)) '(1 2 3 4 5 6 7 8)))

(defun d-map-first (pred rep l)
  (let ((res (list))
        (found nil))
    (dolist (el l)
      (if (and (pred el) (not found))
          (progn
            (push res (rep el))
            (setq found 't)))
      (push res el))
    res))

(defun d-map-last (pred rep l)
  (let ((res (list))
        (found nil))
    (dolist (el (reverse l))
      (if (and (pred el) (not found))
          (progn
            (push res (rep el))
            (setq found 't))
        (push res el)))
    (reverse res)))

;; (dump (d-map-first even? square '(1 2 3 4 5 6 7 8)))
;; (dump (d-map-last even? square '(1 2 3 4 5 6 7 8)))

(defun d-map-indexed (fn lis)
  (let ((res (list)))
    (dotimes (i (length lis))
      (push res (fn i (nth lis i))))
    res))

;; (dump (d-map-indexed (lambda (index item) (- item index)) '(1 2 3 4))) ;; => '(1 1 1 1)


(defun d-annotate (fn lis)
  (let ((res (list)))
    (dolist (el lis)
      (push res (list (fn el) el)))
    res))

;; (dump (d-annotate square '(1 2 3))) ;; => '((2 . 1) (3 . 2) (4 . 3))

(defun d-splice (pred fun lis)
  (let ((res (list)))
    (dolist (el lis)
      (if (pred el)
          (dolist (new-el (fun el))
            (push res new-el))
        (push res el)))
    res))

;; (dump (d-splice 'even? (lambda (x) (list x x)) '(1 2 3 4))) ;; => '(1 2 2 3 4 4)

(defun d-splice-list (pred new-list lis)
  (let ((res (list)))
    (dolist (el lis)
      (if (pred el)
          (dolist (new-el new-list)
            (push res new-el))
        (push res el)))
    res))

;; (dump (d-splice-list 'even? '(a b c) '(1 2 3 4))) ;; => '(1 a b c 2)

(defun d-mapcat (fn lis)
  (let ((res (list)))
    (dolist (el lis)
      (dolist (new-el (fn el))
        (push res new-el)))
    res))

;; (dump (d-mapcat (lambda (item) (list 0 item)) '(1 2 3))) ;; => '(0 1 0 2 0 3)

(defun d-copy (arg)
  (let ((res (list)))
    (dolist (el arg)
      (push res el))
    res))

;; (dump (d-copy '(1 2 3))) ;; => '(1 2 3)

(defun d-filter (pred lis)
  (filter pred lis))


;; (dump (d-filter (lambda (num) (== 0 (mod num 2))) '(1 2 3 4))) ;; => '(2 4)

(defun d-remove (pred lis)
  (filter (lambda (x) (not (pred x))) lis))

;; (dump (d-remove (lambda (num) (== 0 (mod num 2))) '(1 2 3 4))) ;; => '(2 4)

(defun d-remove-first (pred lis)
  (let ((res (list))
        (found nil))
    (dolist (el lis)
      (if (and (pred el) (not found))
          (setq found 't)
        (push res el)))
    res))

;; (dump (d-remove-first 'even? '(1 3 5 4 7 8 10))) ;; => '(1 3 5 7 8 10)

(defun d-remove-last (pred lis)
  (let ((res (list))
        (found nil))
    (dolist (el (reverse lis))
      (if (and (pred el) (not found))
          (setq found 't)
        (push res el)))
    (reverse res)))

;; (dump (d-remove-last 'even? '(1 3 5 4 7 8 10))) ;; => '(1 3 5 7 8 10)

(defun d-remove-item (item list)
  (delete list item))

(defun d-non-nil (list)
  (filter (lambda (x) (not ( equal x nil))) list))

;; (dump (d-non-nil '(1 nil 2 nil nil 3 4 nil 5 nil))) ;; => '(1 2 3 4 5)

(defun d-slice (lis from &optional to step)
  (let ((res (list)))
    (dolist (i (range from (if to (if (> 0 to) (+ (length lis) to 1) to) (length lis)) (if step step 1)))
      (push res (nth lis i)))    
    res))

;; (dump (d-slice '(1 2 3 4 5) 1)) ;; => '(2 3 4 5)
;; (dump (d-slice '(1 2 3 4 5) 1 -2)) ;; => '(1 2 3)
;; (dump (d-slice '(1 2 3 4 5 6 7 8 9) 1 -1 2)) ;; => '(2 4 6 8)

(defun d-take (n lis)
  (let ((res (list)))
    (dolist (i (range 0 (min (length lis) n)))
      (push res (nth lis i)))
    res))

;; (dump (d-take 3 '(1 2 3 4 5))) ;; => '(1 2 3)
;; (dump (d-take 17 '(1 2 3 4 5))) ;; => '(1 2 3 4 5)

(defun d-take-last (n lis)
  (let ((res (list)))
    (dolist (i (range (max 0 (- (length lis) n)) (length lis)))
      (push res (nth lis i)))
    res))

;; (dump (d-take-last 3 '(1 2 3 4 5))) ;; => '(3 4 5)
;; (dump (d-take-last 17 '(1 2 3 4 5))) ;; => '(1 2 3 4 5)
;; (dump (d-take-last 1 '(1 2 3 4 5))) ;; => '(5)

(defun d-drop (n lis)
  (let ((res (list)))
    (dolist (i (range n (length lis)))
      (push res (nth lis i)))
    res))

;; (dump (d-drop 3 '(1 2 3 4 5))) ;; => '(4 5)
;; (dump (d-drop 17 '(1 2 3 4 5))) ;; => '()

(defun d-drop-last (n lis)
  (let ((res (list)))
    (dolist (i (range 0 (- (length lis) n)))
      (push res (nth lis i)))
    res))

;; (dump (d-drop-last 3 '(1 2 3 4 5))) ;; => '(1 2)
;; (dump (d-drop-last 17 '(1 2 3 4 5))) ;; => '()

(defun d-take-while (pred lis)
  (let ((res (list)))
    (dolist (el lis)
      (if (pred el)
          (push res el)
        (return res)))
    res))

;; (dump (d-take-while 'even? '(1 2 3 4))) ;; => '()
;; (dump (d-take-while 'even? '(2 4 5 6))) ;; => '(2 4)

(defun d-drop-while (pred lis)
  (let ((res (list))
        (found nil))
    (dolist (el lis)
      (unless (and (pred el) (not found))
        (push res el)
        (setq found 't)))
    res))

;; (dump (d-drop-while 'even? '(1 2 3 4))) ;; => '(1 2 3 4)
;; (dump (d-drop-while 'even? '(2 4 5 6))) ;; => '(5 6)

(defun d-select-by-indices (indices lis)
  (let ((res (list)))
    (dolist (i indices)
      (push res (nth lis i)))
    res))

;; (dump (d-select-by-indices '(4 10 2 3 6) '("v" "e" "l" "o" "c" "i" "r" "a" "p" "t" "o" "r"))) ;; => '("c" "o" "l" "o" "r")
;; (dump (d-select-by-indices '(2 1 0) '("a" "b" "c"))) ;; => '("c" "b" "a")
;; (dump (d-select-by-indices '(0 1 2 0 1 3 3 1) '("f" "a" "r" "l"))) ;; => '("f" "a" "r" "f" "a" "l" "l" "a")

(defun d-select-columns (columns table)
  (let ((res (list)))
    (dolist (row table)
      (let ((new-row (list)))
        (when (not columns)  (push res 'nil) (continue))
        (dolist (i columns)
          (push new-row (nth row i)))        
        (push res new-row)))
    res))

;; (dump (d-select-columns '(0 2) '((1 2 3) (a b c) (:a :b :c)))) ;; => '((1 3) (a c) (:a :c))
;; (dump (d-select-columns '(1) '((1 2 3) (a b c) (:a :b :c)))) ;; => '((2) (b) (:b))
;; (dump (d-select-columns nil '((1 2 3) (a b c) (:a :b :c)))) ;; => '(nil nil nil)

(defun d-select-column (column table)
  (let ((res (list)))
    (dolist (row table)
      (when (equal column nil) (push res 'nil) (continue))
      (push res (nth row column)))
    res))

;; (dump (d-select-column 1 '((1 2 3) (a b c) (:a :b :c)))) ;; => '(2 b :b)

(defun d-keep (fn lis)
  (let ((res (list))
        el-res)
    (dolist (el lis)
      (setq el-res (fn el))
      (unless (equal el-res nil) (push res el-res)))
    res))

;; (dump (d-keep (lambda (num) (when (> num 3) (* 10 num))) '(1 2 3 4 5 6))) ;; => '(40 50 60)

(defun d-concat (&rest lists)
  (let ((res (list)))
    (dolist (lis lists)
      (dolist (el lis)
        (push res el)))
    res))

;; (dump (d-concat '(1))) ;; => '(1)
;; (dump (d-concat '(1) '(2))) ;; => '(1 2)
;; (dump (d-concat '(1) '(2 3) '(4))) ;; => '(1 2 3 4)


(defun d-flatten (l)
  (let ((res (list)))
    (dolist (el l)
      (if (plist el)
          (dolist (new-el (d-flatten el)) (push res new-el))
        (push res el)))
    res))

;; (dump (d-flatten '((1)))) ;; => '(1)
;; (dump (d-flatten '((1 (2 3) (((4 (5)))))))) ;; => '(1 2 3 4 5)
;; (dump (d-flatten '(1 2 (3 4)))) ;; => '(1 2 (3 . 4))

(defun d-replace (old new lis)
  (when (equal lis nil) (return nil))
  (let ((res (list)))
    (dolist (el lis)
      (if (equal el old)
          (push res new)
        (push res el)))
    res))

;; (dump (d-replace 1 "1" '(1 2 3 4 3 2 1))) ;; => '("1" 2 3 4 3 2 "1")
;; (dump (d-replace "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo"))) ;; => '("a" "nice" "bar" "sentence" "about" "bar")
;; (dump (d-replace 1 2 nil)) ;; => nil

(defun d-replace-first (old new l)
  (when (equal l nil) (return nil))
  (let ((res (list))
        (found nil))
    (dolist (el l)
      (if (and (equal el old) (not found))
          (progn
            (push res new)
            (setq found 't))
        (push res el)))
    res))

;; (dump (d-replace-first 1 "1" '(1 2 3 4 3 2 1))) ;; => '("1" 2 3 4 3 2 1)
;; (dump (d-replace-first "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo"))) ;; => '("a" "nice" "bar" "sentence" "about" "foo")
;; (dump (d-replace-first 1 2 nil)) ;; => nil

(defun d-replace-last (old new l)
  (when (equal l nil) (return nil))
  (let ((res (list))
        (found nil))
    (dolist (el (reverse l))
      (if (and (equal el old) (not found))
          (progn
            (push res new)
            (setq found 't))
        (push res el)))
    (reverse res)))

;; (dump (d-replace-last 1 "1" '(1 2 3 4 3 2 1))) ;; => '(1 2 3 4 3 2 "1")
;; (dump (d-replace-last "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo"))) ;; => '("a" "nice" "foo" "sentence" "about" "bar")
;; (dump (d-replace-last 1 2 nil)) ;; => nil


(defun d-insert-at (n x lis)
  (let ((res (list)))
    (dolist (el lis)
      (push res el))
    (insert res (min n (length res)) x)))

;; (dump (d-insert-at 1 'x '(a b c))) ;; => '(a x b c)
;; (dump (d-insert-at 12 'x '(a b c))) ;; => '(a b c x)

(defun d-replace-at (n x lis)
  (let ((res (list)))
    (dotimes (i (length lis))
      (if (== i n)
          (push res x)
        (push res (nth lis i))))
    res))

;; (dump (d-replace-at 0 9 '(0 1 2 3 4 5))) ;; => '(9 1 2 3 4 5)
;; (dump (d-replace-at 1 9 '(0 1 2 3 4 5))) ;; => '(0 9 2 3 4 5)
;; (dump (d-replace-at 4 9 '(0 1 2 3 4 5))) ;; => '(0 1 2 3 9 5)

(defun d-update-at (n func lis)
  (let ((res (list)))
    (dotimes (i (length lis))
      (if (== i n)
          (push res (func (nth lis i)))
        (push res (nth lis i))))
    res))

;; (dump (d-update-at 0 (lambda (x) (+ x 9)) '(0 1 2 3 4 5))) ;; => '(9 1 2 3 4 5)
;; (dump (d-update-at 1 (lambda (x) (+ x 8)) '(0 1 2 3 4 5))) ;; => '(0 9 2 3 4 5)

(defun d-remove-at (n lis)
  (let ((res (list)))
    (dotimes (i (length lis))
      (unless (== i n)
        (push res (nth lis i))))
    res))

;; (dump (d-remove-at 0 '("0" "1" "2" "3" "4" "5"))) ;; => '("1" "2" "3" "4" "5")
;; (dump (d-remove-at 1 '("0" "1" "2" "3" "4" "5"))) ;; => '("0" "2" "3" "4" "5")
;; (dump (d-remove-at 2 '("0" "1" "2" "3" "4" "5"))) ;; => '("0" "1" "3" "4" "5")

(defun d-remove-at-indices (indices lis)
  (let ((res (list)))
    (dotimes (i (length lis))
      (unless (contains indices i)
        (push res (nth lis i))))
    res))

;; (dump (d-remove-at-indices '(0) '("0" "1" "2" "3" "4" "5"))) ;; => '("1" "2" "3" "4" "5")
;; (dump (d-remove-at-indices '(0 2 4) '("0" "1" "2" "3" "4" "5"))) ;; => '("1" "3" "5")
;; (dump (d-remove-at-indices '(0 5) '("0" "1" "2" "3" "4" "5"))) ;; => '("1" "2" "3" "4")


(defun d-reduce-from (fn initial-value list))
(defun d-reduce-r-from (fn initial-value list))
(defun d-reduce (fn list))
(defun d-reduce-r (fn list))
(defun d-reductions-from (fn init list))
(defun d-reductions-r-from (fn init list))
(defun d-reductions (fn list))
(defun d-reductions-r (fn list))
(defun d-count (pred list))
(defun d-sum (list))
(defun d-running-sum (list))
(defun d-product (list))
(defun d-running-product (list))
(defun d-inits (list))
(defun d-tails (list))
(defun d-common-prefix (&rest lists))
(defun d-common-suffix (&rest lists))
(defun d-min (list))
(defun d-min-by (comparator list))
(defun d-max (list))
(defun d-max-by (comparator list))


(defun d-iterate (fun init n))
(defun d-unfold (fun seed))

(defun d-any? (pred list))
(defun d-all? (pred list))
(defun d-none? (pred list))
(defun d-only-some? (pred list))
(defun d-contains? (list element))
(defun d-same-items? (list list2))
(defun d-is-prefix? (prefix list))
(defun d-is-suffix? (suffix list))
(defun d-is-infix? (infix list))

(defun d-split-at (n list))
(defun d-split-with (pred list))
(defun d-split-on (item list))
(defun d-split-when (fn list))
(defun d-separate (pred list))
(defun d-partition (n list))
(defun d-partition-all (n list))
(defun d-partition-in-steps (n step list))
(defun d-partition-all-in-steps (n step list))
(defun d-partition-by (fn list))
(defun d-partition-by-header (fn list))
(defun d-partition-after-pred (pred list))
(defun d-partition-before-pred (pred list))
(defun d-partition-before-item (item list))
(defun d-partition-after-item (item list))
(defun d-group-by (fn list))

(defun d-elem-index (elem list))
(defun d-elem-indices (elem list))
(defun d-find-index (pred list))
(defun d-find-last-index (pred list))
(defun d-find-indices (pred list))
(defun d-grade-up (comparator list))
(defun d-grade-down (comparator list))


(defun d-union (list list2))
(defun d-difference (list list2))
(defun d-intersection (list list2))
(defun d-powerset (list))
(defun d-permutations (list))
(defun d-distinct (list))

(defun d-rotate (n list))
(defun d-repeat (n x))
(defun d-cons* (&rest args))
(defun d-snoc (list elem &rest elements))
(defun d-interpose (sep list))
(defun d-interleave (&rest lists))
(defun d-zip-with (fn list1 list2))
(defun d-zip (&rest lists))
(defun d-zip-lists (&rest lists))
(defun d-zip-fill (fill-value &rest lists))
(defun d-unzip (lists))
(defun d-cycle (list))
(defun d-pad (fill-value &rest lists))
(defun d-table (fn &rest lists))
(defun d-table-flat (fn &rest lists))
(defun d-first (pred list))
(defun d-some (pred list))
(defun d-last (pred list))
(defun d-first-item (list))
(defun d-second-item (arg1))
(defun d-third-item (arg1))
(defun d-fourth-item (list))
(defun d-fifth-item (list))
(defun d-last-item (list))
(defun d-butlast (list))
(defun d-sort (comparator list))
(defun d-list (&rest args))
(defun d-fix (fn list))

(defun d-tree-seq (branch children tree))
(defun d-tree-map (fn tree))
(defun d-tree-map-nodes (pred fun tree))
(defun d-tree-reduce (fn tree))
(defun d-tree-reduce-from (fn init-value tree))
(defun d-tree-mapreduce (fn folder tree))
(defun d-tree-mapreduce-from (fn folder init-value tree))
(defun d-clone (list))

(defun d-each (list fn))
(defun d-each-while (list pred fn))
(defun d-each-indexed (list fn))
(defun d-each-r (list fn))
(defun d-each-r-while (list pred fn))
(defun d-dotimes (num fn))
(defun d-doto (eval-initial-value &rest forms))
