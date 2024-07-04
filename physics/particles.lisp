(in-package :cl-corsika/physics)

;; This file defines how the particle is identificated
;; in Corsika (and other external programs).
;;
;; Note: currently only Corsika particle identification
;; is implemented. and no MULTITHIN, and Cherenkov
;; photons support.
;;
;;
;; Particle Naming Conversion
;; ----------------------------
;; see `*corsika-special-particle-identifications*'. 

;; ========== Periodic Table of Elements ==========
;; In Corsika, only first 1 <= Z <= 56 is used. 
(defparameter *periodic-table-of-elements*
  (make-array 56 :initial-contents
              '(:H                                                                  :He ;; 2
                :Li :Be                                         :B  :C  :N  :O  :F  :Ne ;; 10
                :Na :Mg                                         :Al :Si :P  :S  :Cl :Ar ;; 18
                :K  :Ca :Sc :Ti :V  :Cr :Mn :Fe :Co :Ni :Cu :Zn :Ga :Ge :As :Se :Br :Kr ;; 36
                :Rb :Sr :Y  :Zr :Nb :Mo :Tc :Ru :Rh :Pd :Ag :Cd :In :Sn :Sb :Te :I  :Xe ;; 54
                :Cs :Ba ;; 56
                ))
  "A part of Periodic Table of Elements. ")

(defparameter *periodic-reverse-table-of-elements*
  (let ((table (make-hash-table)))
    (dotimes (i 56 table)
      (setf (gethash (aref *periodic-table-of-elements* i) table)
            (1+ i))))
  "To find element Z reversely. ")

;; In Table 4., the A is constrained within 2 <= A <= 56,
;; not sure if it is ture here... 
(defun %parse-nucleus (particle-id)
  "Parse Corsika `particle-id' like A*100 + Z.
Return (element-name A Z).

The `element-name' is defined in `*periodic-table-of-elements*';
The Z is nucleus of Z protons. "
  (let ((a (floor particle-id 100))
        (z (mod   particle-id 100)))
    (assert (and (<= 2 a 56) (<= z a)))
    (list (aref *periodic-table-of-elements* (1- z)) a z)))

;; About namming conversion:
;; I made my best making the particle name readable. 
(defparameter *corsika-special-particle-identifications*
  (make-array 195 :initial-contents
              '(:gamma                  ; 1
                :e+ :e-                 ; 2, 3
                nil                     ; 4
                :mu+ :mu-               ; 5, 6
                :pi0 :pi+ :pi-          ; 7, 8, 9
                :K0_L :K+  :K-          ; 10, 11, 12
                :n                      ; 13
                :p :anti-p              ; 14, 15
                :K0_S                   ; 16
                :eta                    ; 17
                :Lambda                 ; 18
                :Sigma+ :Sigma0 :Sigma- ; 19, 20, 21
                :Xi0 :Xi-               ; 22, 23
                :Omega-                 ; 24
                :anti-n                 ; 25
                :anti-Lambda            ; 26
                :anti-Sigma-            ; 27
                :anti-Sigma0            ; 28
                :anti-Sigma+            ; 29
                nil nil                 ; 30, 31
                :anti-Omega-            ; 32
                nil nil nil nil nil nil ; 33, 34, 35, 36, 37, 38
                nil nil nil nil nil nil ; 39, 40, 41, 42, 43, 44
                nil nil nil             ; 45, 46, 47
                :eta!                   ; 48
                :phi                    ; 49
                :omega                  ; 50
                :rho0 :rho+ :rho-       ; 51, 52, 53
                :Delta++ :Delta+        ; 54, 55
                :Delta0                 ; 56
                :Delta-                 ; 57
                :anti-Delta--           ; 58
                :anti-Delta-            ; 59
                :anti-Delta0            ; 60
                :anti-Delta+            ; 61
                :K*0 :K*+               ; 62, 63
                :anti-K*- :anti-K*0     ; 64, 65
                :nu_e :anti-nu_e        ; 66, 67
                :nu_mu :anti-nu_mu      ; 68, 69
                nil                     ; 70
                :eta->gamma-gamma       ; 71
                :eta->3-pi0             ; 72
                :eta->pi+-pi--pi0       ; 73
                :eta->pi+-pi--gamma     ; 74
                :mu+-add-info           ; 75
                :mu--add-info           ; 76
                ;; 77 78 79 80  81  82  83  84
                nil nil nil nil nil nil nil nil 
                :decaying-mu+-at-start  ; 85
                :decaying-mu--at-start  ; 86
                ;; 87 88 89 90  91  92  93  94
                nil nil nil nil nil nil nil nil
                ;; 95 96 97 98  99  100 101 102
                nil nil nil nil nil nil nil nil
                ;; 103 104 105 106 107 108 109 110
                nil    nil nil nil nil nil nil nil
                ;; 111 112 113 114 115
                nil    nil nil nil nil
                :D0 :D+                 ; 116, 117
                :anti-D- :anti-D0       ; 118, 119
                :D+_s :anti-D-_s        ; 120, 121
                :eta_c                  ; 122
                :D*0 :D*+               ; 123, 124
                :anti-D*- :anti-D*0     ; 125, 126
                :D*+_s :anti-D*-_s      ; 127, 128
                nil                     ; 129
                :J/phi                  ; 130
                :tau+ :tau-             ; 131, 132
                :nu_tau :anti-nu_tau    ; 133, 134
                nil nil                 ; 135, 136
                :Lambda+_c              ; 137
                :Xi+_c :Xi0_c           ; 138, 139
                :Sigma++_c              ; 140
                :Sigma+_c               ; 141
                :Sigma0_c               ; 142
                :Xi!+_c                 ; 143
                :Xi!0_c                 ; 144
                :Omega0_c               ; 145
                nil nil nil             ; 146 147 148
                :anti-Lambda-_c         ; 149
                :anti-Xi-_c             ; 150
                :anti-Xi0_c             ; 151
                :anti-Sigma--_c         ; 152
                :anti-Sigma-_c          ; 153
                :anti-Sigma0_c          ; 154
                :anti-Xi!-_c            ; 155
                :anti-Xi!0_c            ; 156
                :anti-Omega0_c          ; 157
                nil nil nil             ; 158, 159, 160
                :Sigma*++_c             ; 161
                :Sigma*+_c              ; 162
                :Sigma*0_c              ; 163
                ;; 164 165 166 167 168 169 170
                nil    nil nil nil nil nil nil
                :anti-Sigma*--_c        ; 171
                :anti-Sigma*-_c         ; 172
                :anti-Sigma*0_c         ; 173
                nil nil                 ; 174, 175
                :B0 :B+                 ; 176, 177
                :anti-B- :anti-B0       ; 178, 179
                :B0_s :anti-B0_s        ; 180, 181
                :B+_c :anti-B-_c        ; 182, 183
                :Lambda0_b              ; 184
                :Sigma-_b               ; 185
                :Sigma+_b               ; 186
                :Xi0_b                  ; 187
                :Xi-_b                  ; 188
                :Omega-_b               ; 189
                :anti-Lambda0_b         ; 190
                :anti-Sigma+_b          ; 191
                :anti-Sigma-_b          ; 192
                :anti-Xi0_b             ; 193
                :anti-Xi+_b             ; 194
                :anti-Omega+_b          ; 195
                ))
  "The Corsika special particles, defined in Corsika GUIDE 7.7500 Table 4. ")

(defparameter *corsika-special-particle-reverse*
  (let ((table (make-hash-table)))
    (dotimes (i 195 table)
      (let ((name (aref *corsika-special-particle-identifications* i)))
        (when name (setf (gethash name table) (1+ i))))))
  "To search reverse by name to id. ")

(defun corsika-particle-name (particle-id)
  "Return particle name by `particle-id'.

Example:

    (corsika-particle-name 5626) ;; => (:fe 56 26)
    (corsika-particle-name 14)   ;; => :p
"
  (let ((particle-id (truncate particle-id)))
    (cond ((<= 1 particle-id 195)
           (aref *corsika-special-particle-identifications*
                 (1- particle-id)))
          ((<= 201 particle-id 5656)
           (%parse-nucleus particle-id))
          ((= (floor particle-id 1000) 8888)
           (error "weights of preceding particle (MULTITHIN) not implemented. "))
          ((= particle-id 9900)
           :cherenkov-photon)
          (t (error (format nil "Malformed particle id ~d. " particle-id))))))

(defun corsika-particle-id (particle-name)
  "Return particle id by `particle-name'.

Example:

    (corsika-particle-id :p)     ;; => 14
    (corsika-particle-id :e+)    ;; => 2
    (corsika-particle-id :gamma) ;; => 1

    (corsika-particle-id '(:fe 56))    ;; => 5626
    (corsika-particle-id '(:fe 56 26)) ;; => 5626 (same as above)
    (corsika-particle-id '(:fe 56 27)) ;; => will raise assert error Fe 26 != 27
"
  (cond ((listp particle-name)
         (let ((element (gethash (first particle-name)
                                 *periodic-reverse-table-of-elements*
                                 nil))
               (a (truncate (second particle-name)))
               (z (third  particle-name)))
           (when element
             (when z (assert (= z element)))
             (+ (* a 100) element))))
        ((eq :cherenkov-photon particle-name)
         :cherenkov-photon)
        ((gethash particle-name *corsika-special-particle-reverse* nil)
         (gethash particle-name *corsika-special-particle-reverse*))
        (t (error (format nil "Unkown particle ~a. " particle-name)))))
