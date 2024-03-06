(in-package :cl-corsika/binary)

;;; Documentation:
;; This part use `defbin' to define the basic Corsika binary OUTPUT
;; sub-block. You could check corsika-77500/doc/CORSIKA_GUIDE7.7500.pdf
;; for specification reference.
 
;;; TODO:
;; 1. DOUBLE CHECK the definition of each sub-block slots
;; 2. Add DOCUMENTATION and COMMENTS for each sub-block
;; 3. Get a user-friendly interface for read out
 
;;; WARNING:
;; Currently the code is WIP, any changes on name and slot would
;; take in place.

(defbin observation-info ()
  number                       ; number of observation levels (max 10)
  (heights :type (:array :float 10)))   ; height of observation level in cm

(defbin energy-spectrum ()
  slope lower-limit upper-limit)

(defbin energy-cutoff ()
  hadron muon electron photon)

(defbin observation-plane ()
  x y z theta phi axis)

(defbin cherenkov-scatter ()
  x y)

(defbin momentum ()
  x y z)

(defbin mag-field ()
  x z)

(defbin cherenkov-cal-info ()
  bunch-size
  detector-number-x detector-number-y
  grid-space-x grid-space-y
  length-x length-y
  output-file-p)

(defbin cherenkov-wavelength ()
  lower upper)

(defbin scattered-core-location ()
  (x :type (:array :float 20))
  (y :type (:array :float 20)))

(defbin CONEX-info ()
  min-vertical-depth
  hadron-high-energy
  muon-high-energy
  em-particle-high-energy
  hadron-low-energy
  muon-low-energy
  em-particle-low-energy
  observation-curvature-flag
  thining-hadronic-whmax
  thining-em-particle-wtmax
  sampling-hadronic-whmax
  sampling-muon-wtmax
  sampling-em-particle-wtmax)

(defbin multi-thin-info ()
  (modes-number                :type (:array :float 6))
  (hadronic-energy-fraction    :type (:array :float 6))
  (hadronic-weight-limit       :type (:array :float 6))
  (em-particle-energy-fraction :type (:array :float 6))
  (rand-int-seed-sequence      :type (:array :float 6))
  (offset-rand-call-mod-1e6    :type (:array :float 6))
  (offset-rand-call-exceed-1e6 :type (:array :float 6))
  interest-particle-threshold
  gzip-output-flag
  pipe-buffer-output-flag
  active-output-flag)

(defbin shower-stastics ()
  weighted-photon-number
  weighted-electrons-number
  weighted-muon-number
  weighted-particle-number)

(defbin lateral-distribution ()
  (x    :type (:array :float 21))
  (y    :type (:array :float 21))
  (xy   :type (:array :float 21))
  (yx   :type (:array :float 21)))

(defbin NKG-output ()
  (level1-lateral-dis :type lateral-distribution)
  (level2-lateral-dis :type lateral-distribution)

  (electron-number              :type (:array :float 10))
  (pseudo-age                   :type (:array :float 10))
  (electron-distribution-dis    :type (:array :float 10))
  (local-pseudo-age-level1      :type (:array :float 10))
  (electron-number-height-g/cm2 :type (:array :float 10))
  (electron-number-height-cm    :type (:array :float 10))
  (local-pseudo-age-level2      :type (:array :float 10)))

(defbin run-header ()
  (type :type :str)                     ; "RUNH"
  
  run-number                            ; run number
  start-date                            ; date of begin run (yymmdd)
  program-version                       ; version of program
  
  (observation-level-info :type observation-info)
  (energy-spectrum        :type energy-spectrum) ; 
  
  EGS4-flag
  NKG-flag
  
  (energy-cutoff          :type energy-cutoff) ; kin. energy cutoff in GeV
  (phisical-constants     :type (:array :float 50))
  (observation-plane      :type observation-plane)
  (:no-use                :offset 11)
  array-angle                           ; rotation angle between array x and north
  shower-number                         ; number of shower
  SLANT-flag
  (CKA                    :type (:array :float 40))
  (CETA                   :type (:array :float 5))
  (CSTRBA                 :type (:array :float 11))
  (:no-use                :offset 97)
  (cherenkov-scatter      :type cherenkov-scatter)
  (HLAY                   :type (:array :float 5))
  (AATM                   :type (:array :float 5))
  (BATM                   :type (:array :float 5))
  (CATM                   :type (:array :float 5))
  NFLAIN
  NFLDIF
  NFLPI0+100NFLPIF
  NFLCHE+100NFRAGM)

(defbin event-header ()
  (type                  :type :str)    ; "EVTH"
  event-number
  particle-id                           ; particle code or A * 100 + Z for nuclei
  energy                                ; GeV
  start-altitude                        ; g/cm^2
  first-target-number                   ; number of first target if fixed
  first-target-z                        ; height (z) of first interaction in cm
                                        ; negative if tracking starts at margin of atmosphere
  (momentum              :type momentum)
  zenith-angle
  azimuth-angle
  (integer-seed-sequence :type (:array :float 10))
  (offset-rand-call-mod-1e6 :type (:array :float 10))
  (offset-rand-call-div-1e6 :type (:array :float 10))
  run-number
  start-date
  program-version
  (observation-info       :type observation-info)
  (energy-spectrum        :type energy-spectrum) ;
  (energy-cutoff          :type energy-cutoff) ; kin. energy cutoff in GeV
  NFLAIN
  NFLDIF
  NFLPI0
  NFLPIF
  NFLCHE
  NFRAGM
  (earth-mag-field        :type mag-field)
  EGS4-flag
  NKG-flag
  low-energy-hadr-model
  high-energy-hadr-model
  CERENKOV-flag
  NEUTRINO-flag
  CURVED-flag
  computer-flag
  theta-lower
  theta-upper
  phi-lower
  phi-upper
  (cherenkov-cal-info    :type cherenkov-cal-info)
  array-angle
  muon-info-output-p
  EGS4-step-length-factor
  (cherenkov-wavelength  :type cherenkov-wavelength)
  cherenkov-rsp-auger-num
  (scattered-core-location :type scattered-core-location)
  SIBYLL-interaction-flag
  SIBYLL-cross-section-flag
  QGSJET-interaction-flag
  QGSJET-cross-section-flag
  DPMJET-interaction-flag
  DPMJET-cross-section-flag
  VENUS-NEXUS-EPOS-cross-section-flag
  muon-multiple-scatter-flag
  NKG-radial-distribution-range
  thin-level-hadronic
  thin-level-em-particle
  thin-em-particle-weigh-max
  max-radius-thin-to-corecut
  viewcone-inner-angle
  viewcone-outer-angle
  transition-height/low-energy
  skim-incidence-flag
  horizontal-axis-altitude
  starting-height
  charm-generation-flag
  hadron-origin-flag
  (CONEX-info                 :type CONEX-info)
  strip-parallel-half-width
  detector-distance
  AUGERHIT-reserved
  (multi-thin-info            :type multi-thin-info)
  (inclined-plane             :type observation-plane)
  (:no-use                    :offset 42))

(defbin event-end ()
  (type :type :str)
  event-number
  (shower-statistics :type shower-statistics)
  (NKG-output        :type NKG-output)
  xi2-per-degree
  weighted-photon-number
  weighted-electron-number
  weighted-hadron-number
  weighted-muon-number
  emerged-em-particle-number
  (:no-use           :offset 6))

(defbin run-end ()
  (type :type :str)
  run-number
  event-number
  :no-use
  parallel-run-number
  parallel-core-number
  (:no-use :offset 269))

;;; Particle Data SubBlock
(defbin particle ()
  description
  momentum
  x y time)

(defbin cherenkov-photon ()
  number x y u v time height)

(defbin mother-particle ()
  description
  momentum
  x y z)

(defbin grandmother-particle ()
  description
  momentum
  ext-electromagn-gen-counter
  depth
  z)

(defbin multithin-weight ()
  id
  (weight :type (:array :float 6)))

(defbin particle-data-sub-block ()
  (particles :type (:array particle 39)))

(defbin cherenkov-photon-data-sub-block ()
  (cherenkov-photon :type (:array cherenkov-photon 39)))

(defbin mother-particle-data-sub-block ()
  (particles :type (:array mother-particle 39)))

(defbin grandmother-particle-data-sub-block ()
  (particles :type (:array grandmother-particle 39)))

(defbin multithin-weight-data-sub-block ()
  (weights :type (:array multithin-weight 39)))

;;; Longitudinal SubBlock
(defbin long-hist ()
  vertical-depth
  gamma e+ e- mu+ mu-
  hadronic charged nuclei cherenkov)

(defbin longitudinal-sub-block ()
  (type :type :str)
  event-number
  particle-id
  energy
  total-blocks
  block-id
  first-interaction-altitude
  zenith-angle
  azimuth-angle
  (energy-cutoff :type energy-cutoff)
  (long-hist     :type (:array long-hist 21)))

;;; The following is Corsika output structure that is not defined by
;;; `defbin'. The following defination declares the structure of
;;; corsika binary output.

(defstruct run
  header events end)

(defstruct event
  header datablocks long-blocks end)
