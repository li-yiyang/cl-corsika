(in-package :cl-corsika/input)

(defparameter *fuzzy-threshold* 0.8
  "Default warning threshold (should be value within 0~1).
If two different word distance (calculated by `fuzzy-match-levidist')
is greater than `*fuzzy-threshold*'. ")

(defun stringfy (object)
  "Output as string. "
  (format nil "~A" object))

;; Copied from https://github.com/cbaggers/mk-string-metrics/
;; removed x y type check
(deftype array-index (&optional (length (1- array-dimension-limit)))
  (list 'integer 0 length))

(defun levenshtein (x y)
  "Calculate Levenshtein distance between two given strings X and Y."
  (declare (type string x y)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (let* ((x-len (length x))
         (y-len (length y))
         (v0 (make-array (1+ y-len) :element-type 'array-index))
         (v1 (make-array (1+ y-len) :element-type 'array-index)))
    (declare (type (simple-array array-index) v0 v1))
    (dotimes (i (1+ y-len))
      (declare (type array-index i))
      (setf (aref v0 i) i))
    (dotimes (i x-len (aref v0 y-len))
      (declare (type array-index i))
      (setf (aref v1 0) (1+ i))
      (dotimes (j y-len)
        (declare (type array-index j))
        (setf (aref v1 (1+ j))
              (min (1+ (aref v1 j))
                   (1+ (aref v0 (1+ j)))
                   (+  (aref v0 j)
                       (if (char= (char x i)
                                  (char y j))
                           0 1)))))
      (rotatef v0 v1))))

(defun norm-levenshtein (x y)
  "Return normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match."
  (let ((r (levenshtein x y)))
    (if (zerop r)
        1
        (- 1 (/ r
                (max (length x)
                     (length y)))))))

(defun make-fuzzy-matcher (dictionary &key (white-list nil)
					(threshold *fuzzy-threshold*))
  "Make a fuzzy matcher with lambda list (word).
1. Test if `word' is within `white-list', if so, do nothing;
2. Test if `word' matches words in `dictionary' using
   `fuzzy-match-levidist', if distance is less than `threshold',
   throw warning of Did you mean.
"
  (let ((dictionary (mapcar #'string-upcase dictionary))
	(white-list (mapcar #'string-upcase white-list)))
    (lambda (word)
      (let ((word (string-upcase word)))
	(unless (member word white-list :test #'string=)
	  (loop for test in dictionary do
	    (when (< threshold (norm-levenshtein word test) 1.0)
	      (warn (format nil "Did you mean: ~A for ~A" test word)))))))))
