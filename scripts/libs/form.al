(import 'fmt :all)

;; (import 'fmt :all)
;; (fmt "this {0}" 8 "b" 1.3)
;; (dolist (i (range 1 50))
;;   (let ((f (file-open (fmt "./file_{}.txt" i) :out)))
;;     ;; (file-close f)
;;     ))
;; (defvar input (read-char))
;; (dump input)

;; (printfln "{:b}" 65)
(println (fmt "{:o}" 65))
(println (fmt "{:b}" 65))
