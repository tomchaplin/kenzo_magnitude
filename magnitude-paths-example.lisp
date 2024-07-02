(load "./magnitude-paths.lisp")
(in-package cat)

(defun dist-circle (n)
  (lambda (x y) (mod (- y x) n)))

(setf 4-circle-ss
  (magnitude-path-flcc-ss
    4
    (dist-circle 4)
    '(4-circle)
  ))

(setf dg1-string
   " 0   1   2  3  1   2
    inf  0   1  2 inf inf
    inf inf  0  1 inf inf
    inf inf inf 0 inf inf
    inf inf inf 2  0   1
    inf inf inf 1 inf  0")

(setf dg2-string
  "0 1 2 2 1 
   inf 0 1 2 inf
   inf inf 0 1 inf
   inf inf inf 0 inf
   inf inf inf 1 0")

(setf dg3-string
   "0 1 2 1
    inf 0 1 2
    inf inf 0 1
    inf inf inf 0")

(setf dg1-ss (matrix-string-to-ss dg1-string))
(setf dg2-ss (matrix-string-to-ss dg2-string))
(setf dg3-ss (matrix-string-to-ss dg3-string))

(write-line "DG1")
(loop for r in '(0 1 2 3 4) do 
  (loop
    for p in '(0 1 2 3)
    do (loop
      for q in '(-3 -2 -1 0)
      do (print-spsq-group dg1-ss r p q))))

(write-line "DG2")
(loop for r in '(0 1 2 3 4) do 
  (loop
    for p in '(0 1 2 3)
    do (loop
      for q in '(-3 -2 -1 0)
      do (print-spsq-group dg2-ss r p q))))

(write-line "DG3")
(loop for r in '(0 1 2 3 4) do 
  (loop
    for p in '(0 1 2 3)
    do (loop
      for q in '(-3 -2 -1 0)
      do (print-spsq-group dg3-ss r p q))))
