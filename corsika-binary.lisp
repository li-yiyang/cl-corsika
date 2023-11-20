(defpackage corsika-binary
  (:use :cl :lisp-binary :lisp-binary-utils)
  (:documentation "This package reads Corsika binary outputs."))

(in-package corsika-binary)

;;; Trival Indent config for SLY/SLIME
;; (trivial-indent:define-indentation defbinary
;;   (4 &lambda &body))
;; (trivial-indent:define-indentation define-enum
;;   (4 4 &lambda &body))

(defbinary corsika-page-spliter ()
  (spliter 0d0 :type single-float))

(defbinary program-info ()
  (run-number 0d0 :type single-float)
  (start-date 0d0 :type single-float)
  (version 0d0 :type single-float))

(defbinary observation-info ()
  (observation-level-number  0d0 :type single-float)
  (observation-level-heights #() :type (simple-array single-float (10))))

(defbinary energy-info ()
  (spectrum-slope 0d0 :type single-float)
  (lower-limit    0d0 :type single-float)
  (upper-limit    0d0 :type single-float))

(defbinary energy-cutoff ()
  (hardron  0d0 :type single-float)
  (muon     0d0 :type single-float)
  (electron 0d0 :type single-float)
  (photon   0d0 :type single-float))

(defbinary inclined-observation-plane ()
  (x 0d0 :type single-float)
  (y 0d0 :type single-float)
  (z 0d0 :type single-float)
  (theta 0d0 :type single-float)
  (phi   0d0 :type single-float)
  (axis-angle-depth 0d0 :type single-float))

(defbinary run-header ()
  (type "RUNH" :type (fixed-length-string 4))
  (program     nil :type program-info)
  (observation nil :type observation-info)
  (energy      nil :type energy-info)

  ;; EGS4 flag
  (EGS4-flag 0d0 :type single-float)
  ;; NKG flag
  (NKG-flag  0d0 :type single-float)

  ;; energy cutoff
  (energy-cutoff nil :type energy-cutoff)

  ;; physical constants and interaction flags
  (constants #() :type (simple-array single-float (50)))

  ;; inclined observation plane info
  (inclined-observation-plane nil :type inclined-observation-plane)

  ;; no longer used
  (no-longer-used #() :type (simple-array single-float (11)))

  ;; rotation between array x direction and earth north
  (rotation-x-to-north 0d0 :type single-float)

  ;; shower number
  (shower-number 0d0 :type single-float)

  ;; SLANT flag
  (SLANT-flag 0d0 :type single-float)

  ;; CKA
  (CKA #() :type (simple-array single-float (40)))
  (CETA #() :type (simple-array single-float (5)))
  (CSTRBA #() :type (simple-array single-float (11)))

  ;; no longer used
  (no-longer-used2 #() :type (simple-array single-float (97)))

  ;; scatter range for cherenkov
  (chenrenkov-scatter-x 0d0 :type single-float)
  (chenrenkov-scatter-y 0d0 :type single-float)

  ;; some meaning less (? doubt) parameters
  (HLAY #() :type (simple-array single-float (5)))
  (AATM #() :type (simple-array single-float (5)))
  (BATM #() :type (simple-array single-float (5)))
  (CATM #() :type (simple-array single-float (5)))
  (NFLAIN 0d0 :type single-float)
  (NFLDIF 0d0 :type single-float)
  (NFLPI0+100xNFLPIF 0d0 :type single-float)
  (NFLCHE+100xNFRAGM 0d0 :type single-float))

(defbinary cherenkov-info ()
  (bunch-size 0d0 :type single-float)
  (detector-x-num 0d0 :type single-float)
  (detector-y-num 0d0 :type single-float)
  (detector-grid-x 0d0 :type single-float)
  (detector-grid-y 0d0 :type single-float)
  (detector-len-x  0d0 :type single-float)
  (detector-len-y  0d0 :type single-float)
  (output-file     0d0 :type single-float))

(defbinary event-header ()
  (type "EVTH" :type (fixed-length-string 4))

  ;; event info
  (event-number    0d0 :type single-float)
  (particle-id     0d0 :type single-float)
  (particle-energy 0d0 :type single-float)
  (altitude        0d0 :type single-float)
  
  (first-target-number 0d0 :type single-float)
  (first-interaction-z 0d0 :type single-float)
  
  (p-x 0d0 :type single-float)
  (p-y 0d0 :type single-float)
  (p-z 0d0 :type single-float)
  (zenith-angle  0d0 :type single-float)
  (azimuth-angle 0d0 :type single-float)

  ;; random number sequence number
  (rand-sequence-length 0d0 :type single-float)
  (rand-sequence #() :type (simple-array
                            (simple-array single-float (3)) (10)))

  ;; run info
  (program nil :type program-info)

  ;; observation levels
  (observation nil :type observation-info)

  ;; energy spectrum
  (energy nil :type energy-info)

  ;; flags
  (NFLAIN 0d0 :type single-float)
  (NFLDIF 0d0 :type single-float)
  (NFLPI0 0d0 :type single-float)
  (NFLCHE 0d0 :type single-float)
  (NFRAGM 0d0 :type single-float)

  ;; earth-magnetic-field
  (earth-magnetic-field-x 0d0 :type single-float)
  (earth-magnetic-field-y 0d0 :type single-float)

  ;; flags
  (EGS4-flag 0d0 :type single-float)
  (NKG-flag  0d0 :type single-float)

  ;; low-energy hadr. model flag
  (low-energy-hadr-flag  0d0 :type single-float)
  (high-energy-hadr-flag 0d0 :type single-float)

  ;; flags
  (CERENKOV-flag 0d0 :type single-float)
  (NEUTRINO-flag 0d0 :type single-float)
  (CURVED-flag   0d0 :type single-float)
  (computer-flag 0d0 :type single-float)

  ;; geometry info
  (theta-lower-edge 0d0 :type single-float)
  (theta-upper-edge 0d0 :type single-float)
  (phi-lower-edge   0d0 :type single-float)
  (phi-upper-edge   0d0 :type single-float)

  ;; cherenkov info
  (cherenkov nil :type cherenkov-info)
  )

