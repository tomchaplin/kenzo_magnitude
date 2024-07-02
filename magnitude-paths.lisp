; TODO: Replace mpcc basis elements with integers

(require :asdf)
(push #P"./kenzo" asdf:*central-registry*)
(require :kenzo "./kenzo/kenzo--all-systems.fasb")
(in-package cat)


; === GENERATING BASIS ===

; All the possible regular paths that end in path
; and use the integers 0, ..., n-1
(defun regular-path-extensions (path n)
  (declare
    (type list path)
    (type fixnum n))
  (mapcar
    #'(lambda (ext) (cons ext path))
    (remove-if #'(lambda (v) (= v (first path))) (<a-b< 0 n)))
)

; TODO: Specialise nk-paths to filter out based on length using dist-matrix
; TODO: Also cache?

; Generate all regular k-paths of the integers 0, ..., n-1
(defun nk-paths (n k)
  (declare (type fixnum k n))
  (cond
    ; Base-case : these are all the 0-paths
    ((= k 0) (mapcar #'list (<a-b< 0 n)))
    ; Otherwise : recurse
    (t 
      (mapcan #'(lambda (path) (regular-path-extensions path n)) (nk-paths n (- k 1)))  
    )))

; === BOUNDARY OPERATORS ===

(defun remove-nth (n path)
  (append (subseq path 0 n)
          (subseq path (+ 1 n)) ))

(defun regular-bdry (i path)
  (declare 
    (type list path)
    (type fixnum i))
  (let ((nrb (remove-nth i path)))
    (cond
      ((is-irregular nrb) :degenerate)
      (t nrb))))

(defun index-to-parity (i)
  (declare (type fixnum i))
  (cond ((evenp i) 1)
         (t -1)))

(defun total-non-regular-bdry (path)
  (declare (type list path))
  (mapcar
    #'(lambda (i)
      (pairlis
        '(coeff path)
        (list (index-to-parity i) (remove-nth i path))
      )
    )
    (<a-b< 0 (length path)))
)

(defun is-irregular (path)
  (declare (type list path))
  (cond
    ; 0-path cannot be irregular
    ((= (length path) 1 ) nil)
    (t (or
      ; Check if start of path is irregular
      ( = (first path) (first (rest path)))
      (is-irregular (rest path))))
  )
)

; Probably could be made more efficient by checking regularity during construction
(defun total-regular-bdry (path)
  (declare (type list path))
  (remove-if
    #'(lambda (path-al)
      (is-irregular (rest (assoc 'path path-al))))
    (total-non-regular-bdry path))
  )

(defun path-cmpr-p (x y)
  (eq (l-cmpr x y) :less))

(defun sort-bdry (bdry)
  (sort
    bdry
    #'(lambda (x y) (path-cmpr-p (rest (assoc 'path x)) (rest (assoc 'path y))))))

(defun sort-basis (basis)
  (sort basis #'path-cmpr-p))

(defun total-regular-bdry-dffr (path)
  (let ((new-degree (- (length path) 2)))
    (cond
      ; Empty boundary if going into degree -1
      ((<= new-degree -1) (cmbn -1))
      (t
        ; Otherwise pass arguments to cmbn
        (apply #'cmbn (cons new-degree
          (mapcan 
              #'(lambda (path-al)
                (list
                  (rest (assoc 'coeff path-al))
                  (rest (assoc 'path path-al))
                ))
              (sort-bdry (total-regular-bdry path)))))))))

; === PATH REPRESENTATIONS ===

(defun path-to-int (path n_vertices)
  (declare
    (type list path)
    (type fixnum n_vertices))
  (cond
    ((null path) 0)
    (t (+ (+ (first path) 1)
     (* (path-to-int (rest path) n_vertices) (+ 1 n_vertices))))))

(defun int-to-path (path_int n_vertices)
  (declare (type fixnum n_vertices path_int))
  (cond
    ((= path_int 0) +empty-list+)
    (t
      (cons
        (- (rem path_int (+ 1 n_vertices)) 1)
        (int-to-path (floor path_int (+ 1 n_vertices)) n_vertices)
      )
    )))

; === LENGTH FILTRATION ===

(defun add-with-inf (a b)
  (cond
    ((eq a :inf) :inf)
    ((eq b :inf) :inf)
    (t (+ a b))
))

(defun path-length (dist-matrix path)
  (cond
    ; Empty => t=0
    ((null path) 0)
    ; Vertex => t=0
    ((<= (length path) 1) 0)
    (t 
      ; TODO: Replace with early return upon inf
      (add-with-inf
        (funcall dist-matrix (first path) (first (rest path)))
        (path-length dist-matrix (rest path))
      ))))

; === MAGNITUDE PATH CHAIN COMPLEX + FILTRATION + SPECTRAL SEQUENCE ===
  
; TODO: Switch over to integer representation

; Simplicial set underlying the MPSS
; On vertices 0...(n-1) up to some max k
(defun mp-chain-comp (n k)
  (declare (type fixnum k n))
  (build-chcm
    :cmpr #'l-cmpr
    :basis #'(lambda (dmn)
                (cond
                  ((> dmn k) +empty-list+)
                  ((< dmn 0) +empty-list+)
                  (t 
                    (sort-basis (nk-paths n dmn))
                  )
                ))
    :bsgn '(0)
    :intr-dffr #'(lambda (dmn gnr) (total-regular-bdry-dffr gnr))
    :strt :gnrt
    :orgn '(magnitude-path-chain-comp n k)
  ))

(defun magnitude-path-flcc (n dist-matrix orgn)
  (change-chcm-to-flcc
    (build-chcm
      :cmpr #'l-cmpr
      :basis #'(lambda (dmn)
                  (cond
                    ((< dmn 0) +empty-list+)
                    (t 
                      (sort-basis 
                        (remove-if
                          #'(lambda (path) (eq :inf (path-length dist-matrix path)))
                          (nk-paths n dmn))))))
      :bsgn '(0)
      :intr-dffr #'(lambda (dmn gnr) (total-regular-bdry-dffr gnr))
      :strt :gnrt
      :orgn (list orgn 'magnitude-path-unfl-cc)
    )
    #'(lambda (dgr path)
      ; Can assume path length is finite since path-gen is a basis element
      (path-length dist-matrix path)
    )
  )
)

(defun magnitude-path-flcc-ss (n dist-matrix orgn)
  (build-ss
    (magnitude-path-flcc n dist-matrix orgn)
    (list orgn 'magnitude-path-flcc-ss)
  )
)

; === DISTANCE MATRIX UTILITIES ===

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,) (char= c #\return) (char= c #\linefeed)))

; Shamelessly stolen from
; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun parse-int-or-inf (input)
  (cond
    ((string= "inf" input) :inf)
    (t (parse-integer input))))

(defun matrix-string-to-vector (matrix-string)
  (coerce (mapcar #'parse-int-or-inf (my-split matrix-string)) 'vector)
)

(defun matrix-vector-n-vertices (matrix-vector)
  (isqrt (length matrix-vector)))

(defun matrix-vector-lookup (matrix-vector n i j)
  (elt matrix-vector (+ (* n i) j)))

(defun matrix-vector-to-func (matrix-vector)
  (let
    ((n (matrix-vector-n-vertices matrix-vector)))
    (lambda (x y) (matrix-vector-lookup matrix-vector n x y))
  ))

(defun matrix-string-to-func (matrix-string)
  (matrix-vector-to-func (matrix-string-to-vector matrix-string)))

(defun matrix-string-to-flcc (matrix-string)
  (let
    ((matrix-vector (matrix-string-to-vector matrix-string)))
    (magnitude-path-flcc
      (matrix-vector-n-vertices matrix-vector)
      (matrix-vector-to-func matrix-vector)
      (list 'matrix-string-to-flcc matrix-string)
      )))

(defun matrix-string-to-ss (matrix-string)
  (build-ss
    (matrix-string-to-flcc matrix-string)
    (list 'matrix-string-to-ss matrix-string)))
