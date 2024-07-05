(in-package :cl-corsika/binary.lowlevel)

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
  ;; number of observation levels (maximum 10)
  number
  ;; height of observation level i(1..10) in cm
  (heights :type (:array :float 10)))

(defbin energy-spectrum ()
  ;; slope of energy spectrum
  slope
  ;; lower limit of energy range
  lower-limit
  ;; upper limit of energy range
  upper-limit)

(defbin energy-cutoff ()
  ;; kin. energy cutoff for hadrons in GeV
  hadron
  ;; kin. energy cutoff for muons in GeV
  muon
  ;; kin. energy cutoff for electrons in GeV
  electron
  ;; energy cutoff for photons in GeV
  photon)

(defbin observation-plane ()
  ;; XPINCL X-displacement of inclined observation plane
  x
  ;; YPINCL Y-displacement of inclined observation plane
  y
  ;; ZPINCL Z-displacement of inclined observation plane
  z
  ;; THINCL θ angle of normal vector of inclined observation plane
  theta
  ;; PHINCL φ angle of normal vector of inclined observation plane
  phi
  ;; TDINCL Depth on the axis angle for perpendicular observation plane
  depth)

(defbin cherenkov-scatter-range ()
  ;; XSCATT scatter range in x direction for Cherenkov
  x
  ;; YSCATT scatter range in y direction for Cherenkov
  y)

(defbin momentum ()
  ;; px momentum in GeV/c
  x
  ;; py momentum in GeV/c
  y
  ;; pz momentum in GeV/c
  z)

(defbin mag-field ()
  ;; x component of Earth's magnetic field in μT
  x
  ;; y component of Earth's magnetic field in μT
  z)

(defbin cherenkov-cal-info ()
  ;; Cherenkov bunch size in the case of Cherenkov calculations
  bunch-size
  ;; number of Cherenkov detectors in x-direction
  detector-number-x
  ;; number of Cherenkov detectors in y-direction
  detector-number-y
  ;; grid spacing of Cherenkov detectors in x-direction in cm
  grid-space-x
  ;; grid spacing of Cherenkov detectors in y-direction in cm
  grid-space-y
  ;; length of each Cherenkov detector in x-direction in cm
  length-x
  ;; length of each Cherenkov detector in x-direction in cm
  length-y
  ;; Cherenkov output directed to particle output file (= 0.)
  ;; or Cherenkov output file (= 1.)
  output-file-p)

(defbin cherenkov-wavelength ()
  ;; Cherenkov wavelength lower end (in nm)
  lower
  ;; Cherenkov wavelength upper end (in nm)
  upper)

(defbin scattered-core-location ()
  ;; x coordinate of ith core location for scattered events (in cm)
  (x :type (:array :float 20))
  ;; y coordinate of ith core location for scattered events (in cm)
  (y :type (:array :float 20)))

(defbin CONEX-info ()
  ;; minimal vertical depth for transfer of particles from CONEX to CORSIKA
  min-vertical-depth
  ;; high-energy threshold for treatment of hadrons by cascade equations in CONEX
  hadron-high-energy
  ;; high-energy threshold for treatment of muons by cascade equations in CONEX
  muon-high-energy
  ;; high-energy threshold for treatment of em-particles by cascade equations in CONEX
  em-particle-high-energy
  ;; low-energy threshold for treatment of hadrons by cascade equations in CONEX
  hadron-low-energy
  ;; low-energy threshold for treatment of muons by cascade equations in CONEX
  muon-low-energy
  ;; high-energy threshold for treatment of em-particles by cascade equations in CONEX
  em-particle-low-energy
  ;; flag for observation level curvature (CURVOUT)
  ;; (0.=flat, 1.=curved)
  observation-curvature-flag
  ;; actual weight limit whmax for thinning hadronic in CONEX
  thining-hadronic-whmax
  ;; actual weight limit whmax for thinning em-particles in CONEX
  thining-em-particle-wtmax
  ;; actual weight limit whmax for sampling hadronic in CONEX
  sampling-hadronic-whmax
  ;; actual weight limit whmax for sampling muons in CONEX
  sampling-muon-wtmax
  ;; actual weight limit whmax for sampling em-particles in CONEX
  sampling-em-particle-wtmax)

(defbin multi-thin-info ()
  ;; number of MULTITHIN modes (max. 6)
  modes-number
  ;; energy fraction of hadronic thinning for jth MULTITHIN mode
  (hadronic-energy-fraction :type (:array :float 6))
  ;; actual weight limit of hadronic thinning for jth MULTITHIN mode
  (hadronic-weight-limit :type (:array :float 6))
  ;; energy fraction of em thinning for jth MULTIHIN mode
  (em-particle-energy-fraction :type (:array :float 6))
  ;; actual weight limit of em thinning for jth MULTITHIN mode
  (em-particle-weight-limit :type (:array :float 6))
  
  ;; NOTE: not sure for 199 + 3*j, 205 + 3*j, 211 + 3*j
  ;; integer seed of random sequence for jth MULTITHIN mode
  (rand-int-seed-sequence :type (:array :float 6))
  ;; # of offset random calls (mod 10^6) for jth MULTITHIN mode
  (offset-rand-call-mod-1e6 :type (:array :float 6))
  ;; # of offset random calls (exceeding 10^6) for jth MULTITHIN mode
  (offset-rand-call-exceed-1e6 :type (:array :float 6))
  ;; threshold energy above which particles are interesting (ICECUBE)
  interest-particle-threshold
  ;; flag indicating that output is compressed by gzip (ICECUBE)
  gzip-output-flag
  ;; flag indicating that output is written to pipe buffer (ICECUBE)
  pipe-buffer-output-flag
  ;; index of MULTITHIN weight used in COAST
  weighted-index
  ;; flag indicating MULTITHIN output is active
  active-output-flag)

(defbin shower-statistics ()
  ;; weighted number of photons arriving at observation level(s)
  weighted-photon-number
  ;; weighted number of electrons arriving at observation level(s)
  weighted-electrons-number
  ;; weighted number of hadrons arriving at observation level(s)
  weighted-hadrons-number
  ;; weighted number of muons arriving at observation level(s)
  weighted-muon-number
  
  ;; number of weighted particles written to particle output file MPATAP.
  ;; (This number includes also Cherenkov bunches, if Cherenkov output is
  ;; directed to MPATAP, but excludes additional muon information. )
  weighted-particle-number)

(defbin lateral-distribution ()
  ;; lateral distribution in x direction for i. level in cm^-2
  (x :type (:array :float 21))
  ;; lateral distribution in y direction for i. level in cm^-2
  (y :type (:array :float 21))
  ;; lateral distribution in xy direction for i. level in cm^-2
  (xy :type (:array :float 21))
  ;; lateral distribution in yx direction for i. level in cm^-2
  (yx :type (:array :float 21)))

(defbin NKG-output ()
  ;; lateral distribution in x, y, xy, yx direction for 1. level in cm^-2
  (level1-lateral-dis :type lateral-distribution)
  ;; lateral distribution in x, y, xy, yx direction for 2. level in cm^-2
  (level2-lateral-dis :type lateral-distribution)

  ;; i(1..10): electron number in steps of 100 g/cm^2
  (electron-number           :type (:array :float 10))
  ;; i(1..10): pseudo-age in steps of 100 g/cm^2
  (pseudo-age                :type (:array :float 10))
  ;; i(1..10): distances for electron distribution in cm
  (electron-distribution-dis :type (:array :float 10))
  ;; i(1..10): local pseudo-age 1. level
  (local-pseudo-age-level1   :type (:array :float 10))

  ;; i(1..10): height of levels for electron numbers in g/cm^2
  (electron-number-height-g/cm2 :type (:array :float 10))
  ;; i(1..10): height of levels for electron numbers in cm
  (electron-number-height-cm    :type (:array :float 10))
  ;; i(1..10): distance bins for local pseudo-age in cm
  (electron-distribution-bin    :type (:array :float 10))
  ;; i(1..10): local pseudo-age 2. level
  (local-pseudo-age-level2      :type (:array :float 10)))

;; Structure data sub-block defination
(defbin run-header (:size-check 273)
  ;; 1: "RUNH"
  (type :type :str)
  ;; 2: run number
  run-number
  ;; 3: date of begin run (yymmdd)
  start-date
  ;; 4: version of program
  program-version
  
  ;; 5, 5+i(1..10): see `obervation-info'
  (observation-level-info :type observation-info)
  
  ;; 16, 17, 18: see `energy-spectrum'
  (energy-spectrum :type energy-spectrum)

  ;; 19: flag for EGS4 treatment of em.component
  EGS4-flag
  ;; 20: flag for NKG treatment of em.component
  NKG-flag

  ;; 21..24: see `energy-cutoff'
  (energy-cutoff :type energy-cutoff)
  
  ;; 24+i(1..50): physical constants and interaction flags
  (phisical-constants :type (:array :float 50))
  
  ;; 75..80: see `observation-plane'
  (observation-plane :type observation-plane)

  ;; 80 + i(1..11): no longer used
  (:no-use :offset 11)
  ;; 92: ARRANG rotation angle (deg) between array x-direction and North
  array-angle
  ;; 93: NSHOW number of showers to be generated
  shower-number
  ;; 94: flag for SLANT option
  SLANT-flag
  ;; 94 + i(1..40): CKA(i)
  (CKA    :type (:array :float 40))
  ;; 134 + i(1..5): CETA(i)
  (CETA   :type (:array :float 5))
  ;; 139 + i(1..11): CSTRBA(i)
  (CSTRBA :type (:array :float 11))
  
  ;; 150 + i(1..97): no longer used
  (:no-use :offset 97)

  ;; 248, 249: see `cherenkov-scatter-range'
  (cherenkov-scatter-range :type cherenkov-scatter-range)

  ;; 249 + i(1..5): HLAY(i)
  (HLAY :type (:array :float 5))
  ;; 254 + i(1..5): AATM(i)
  (AATM :type (:array :float 5))
  ;; 259 + i(1..5): BATM(i)
  (BATM :type (:array :float 5))
  ;; 264 + i(1..5): CATM(i)
  (CATM :type (:array :float 5))

  ;; 270: NFLAIN
  NFLAIN
  ;; 271: NFLDIF
  NFLDIF
  ;; 272: NFLPI0 + 100 * NFLPIF
  NFLPI0+100NFLPIF
  ;; 273: NFLCHE + 100 * NFRAGM
  NFLCHE+100NFRAGM)

(defbin event-header (:size-check 273)
  ;; 1: "EVTH"
  (type :type :str)
  ;; 2: event number
  event-number
  ;; 3: particle id (particle code or A * 100 + Z for nuclei)
  particle-id
  ;; 4: total energy in GeV
  energy
  ;; 5: starting altitude in g/cm^2
  start-altitude
  ;; 6: number of first target if fixed
  first-target-number
  ;; 7: z coordinate (height) of first interaction in cm
  ;; (negative, if tracking starts at margin of atmosphere, see TSTART)
  first-target-z

  ;; 8, 9, 10: p momentum in x, y, -z in GeV/c
  ;; (pz is positive for downward going particles)
  (momentum :type momentum)

  ;; 11: zenith angle θ in radian
  zenith-angle
  ;; 12: azimuth angle φ in radian
  azimuth-angle

  ;; 13: number of different random number sequences (max 10)
  rand-sequences-number
  ;; Note: Not know the definition in CORSIKA GUIDE
  ;; 11 + 3 * i: integer seed of sequence i
  (integer-seed-sequence    :type (:array :float 10))
  ;; 21 + 3 * i: number of offset random calls (mod 10^6) of sequence i
  (offset-rand-call-mod-1e6 :type (:array :float 10))
  ;; 31 + 3 * i: number of offset random calls (/ 10^6) of sequence i
  (offset-rand-call-div-1e6 :type (:array :float 10))
  
  ;; 44: run number
  run-number
  ;; 45: date of begin run (yymmdd)
  start-date
  ;; 46: version of program
  program-version
  
  ;; 47, 47 + i(1..10): see `observation-info'
  (observation-info :type observation-info)
  ;; 58, 59, 60: see `energy-spectrum'
  (energy-spectrum  :type energy-spectrum)
  ;; 60..64: see `energy-cutoff'
  (energy-cutoff    :type energy-cutoff)
  ;; 65: NFLAIN
  NFLAIN
  ;; 66: NFLDIF
  NFLDIF
  ;; 67: NFLPI0
  NFLPI0
  ;; 68: NFLPIF
  NFLPIF
  ;; 69: NFLCHE
  NFLCHE
  ;; 70: NFRAGM
  NFRAGM
  
  ;; 71, 62: see `mag-field'
  ;; x, z component of Earth's magnetic field in μT
  (earth-mag-field :type mag-field)

  ;; 73: flag for activating EGS4
  EGS4-flag
  ;; 74: flag for activating NKG
  NKG-flag

  ;; 75: low-energy hadr. model flag
  ;; (1.=GHEISHA, 2.=UrQMD, 3.=FLUKA)
  low-energy-hadr-model
  ;; 76: high-energy hadr. model flag
  ;; (0.=HDPM, 1.=VENUS, 2.=SIBYLL, 3.=QGSJET, 4.=DPMJET, 5.=NEXUS, 6.=EPOS)
  high-energy-hadr-model
  
  ;; 77: CERENKOV flag
  CERENKOV-flag
  ;; 78: NEUTRINO flag
  NEUTRINO-flag
  ;; 79: CURVED flag
  ;; (0=standard, 2=CURVED)
  CURVED-flag
  ;; 80: computer flag
  ;; (3=UNIX, 4=Macintosh)
  computer-flag

  ;; 81: lower edge of θ interval (in ◦)
  theta-lower
  ;; 82: upper edge of θ interval (in ◦)
  theta-upper
  ;; 83: lower edge of φ interval (in ◦)
  phi-lower
  ;; 84: upper edge of φ interval (in ◦)
  phi-upper

  ;; 85..92: see `cherenkov-cal-info'
  (cherenkov-cal-info :type cherenkov-cal-info)
  
  ;; 93: angle (in rad) between array x-direction and magnetic north
  array-angle
  ;; 94: flag for additional muon information on particle output file
  muon-info-output-p
  ;; 95: step length factor for multiple scattering step length in EGS4
  EGS4-step-length-factor
  ;; 96, 97: Cherenkov wavelength (in nm), see `cherenkov-wavelength'
  (cherenkov-wavelength :type cherenkov-wavelength)
  ;; 98: number i of uses of each Cherenkov rsp. Auger event
  cherenkov-rsp-auger-num
  ;; 98 + i, 118 + i: see `scattered-core-location'
  (scattered-core-location :type scattered-core-location)

  ;; 139: SIBYLL interaction flag
  ;; (0.= no SIBYLL, 1.=vers.1.6; 2.=vers.2.1; 3.=vers.2.3c; 4.=vers.2.3d)
  SIBYLL-interaction-flag
  ;; 140: SIBYLL cross-section flag
  ;; (0.= no SIBYLL, 1.=vers.1.6; 2.=vers.2.1; 3.=vers.2.3c; 4.=vers.2.3d)
  SIBYLL-cross-section-flag
  ;; 141: QGSJET interaction flag
  ;; (0.=no QGSJET; 1.=QGSJETOLD; 2.=QGSJET01d; 3.=QGSJET-II)
  QGSJET-interaction-flag
  ;; 142: QGSJET cross-section flag
  ;; (0.=no QGSJET; 1.=QGSJETOLD; 2.=QGSJET01d; 3.=QGSJET-II)
  QGSJET-cross-section-flag
  ;; 143: DPMJET interaction flag
  ;; (0.=no DPMJET, 1.=DPMJET)
  DPMJET-interaction-flag
  ;; 144: DPMJET cross-section flag
  ;; (0.=no DPMJET, 1.=DPMJET)
  DPMJET-cross-section-flag
  ;; 145: VENUS/NEXUS/EPOS cross-section flag
  ;; (0=neither, 1.=VENUSSIG, 2./3.=NEXUSSIG, 4.=EPOSSIG)
  VENUS-NEXUS-EPOS-cross-section-flag
  ;; 146: muon multiple scattering flag
  ;; (1.=Molie ́re; 0.=Gauss)
  muon-multiple-scatter-flag
  ;; 147: NKG radial distribution range in cm
  NKG-radial-distribution-range

  ;; 148: EFRCTHN energy fraction of thinning level hadronic
  thin-level-hadronic
  ;; 149: EFRCTHN THINRAT energy fraction of thinning level em-particles
  thin-level-em-particle
  ;; 150: actual weight limit WMAX for thinning hadronic
  thin-hadronic-weight-max
  ;; 151: actual weight limit WMAX WEITRAT for thinning em-particles
  thin-em-particle-weigh-max

  ;; 152: max. radius (in cm) for radial thinning (THIN) rsp. cutting (CORECUT)
  max-radius-thin-to-corecut
  ;; 153: inner angle of viewing cone VIEWCONE (in ◦)
  viewcone-inner-angle
  ;; 154: outer angle of viewing cone VIEWCONE (in ◦)
  viewcone-outer-angle
  ;; 155: transition energy high-energy/low-energy model (in GeV)
  transition-height/low-energy
  ;; 156: skimming incidence flag
  ;; (0.=standard, 1.=skimming)
  skim-incidence-flag
  ;; 157: altitude (cm) of horizontal shower axis (skimming incidence)
  horizontal-axis-altitude
  ;; 158: starting height (cm)
  starting-height
  ;; 159: flag indicatinng that explicite charm generation is switched on
  charm-generation-flag
  ;; 160: flag for hadron origin of electromagnetic subshower on particle tape
  hadron-origin-flag

  ;; 161..173: see `CONEX-info'
  (CONEX-info :type CONEX-info)

  ;; 174: half width of stripes parallel to detector rows in AUGERHIT option (cm)
  strip-parallel-half-width
  ;; 175: detector distance between neighbouring detectors (AUGERHIT) (cm)
  detector-distance
  ;; 176: (reserved for AUGERHIT)
  AUGERHIT-reserved
  
  ;; 177 + j, 183 + j, 189 + j, 195 + j, 199 + 3 * j, 205 + 3 * j, 211 + 3 * j
  ;; see `multi-thin-info'.
  (multi-thin-info :type multi-thin-info)
  ;; 225..230: see `observation-plane'
  (inclined-plane  :type observation-plane)
  
  ;; 231..273: not used
  (:no-use :offset 43))

(defbin event-end (:size-check 273) ;; TODO ?
  ;; 1: "EVTE"
  (type :type :str)
  ;; 2: event number
  event-number
  
  ;; statistics for one shower
  ;; 3..7: see `shower-statistics'
  (shower-statistics :type shower-statistics)
  
  ;; NKG output (if selected):
  ;; 8..255: see `NKG-output'
  (NKG-output :type NKG-output)
  
  ;; 256 + i(1..6): parameters of longitudinal distribution of charged particles
  (longitudinal-distribution-parameters :type (:array :float 6))
  
  ;; 262: χ^2 per degree of freedom of fit to longitudinal distribution
  xi2-per-degree
  
  ;; 263: weighted number of photons written to particle output file
  weighted-photon-number
  ;; 264: weighted number of electrons written to particle output file
  weighted-electron-number
  ;; 265: weighted number of hadrons written to particle output file
  weighted-hadron-number
  ;; 266: weighted number of muons written to particle output file
  weighted-muon-number
  ;; 267: number of em-particles emerging from pre-shower
  emerged-em-particle-number
  
  ;; 268..273: not used
  (:no-use :offset 6))

(defbin run-end (:size-check 273)
  ;; 1: "RUNE"
  (type :type :str)
  ;; 2: run number
  run-number
  ;; 3: number of events processed
  event-number
  ;; 4: not used
  :no-use
  
  ;; in case of PARALLEL
  ;; 3: run number
  ;; parallel-run-number
  ;; 4: number of cores used in this parallel run
  ;; parallel-core-number
  
  ;; 5..273: not used yet
  (:no-use :offset 269))

;;; Particle Data SubBlock
(defbin particle (:size-check 7)
  ;; 1: particle description encoded as:
  ;; part. id * 1000 + hadr. generation * 10 + no. of obs. level
  ;; [for additional muon information or id 95/96:
  ;; part. id * 1000 + hadr. generation]
  description
  ;; 2, 3, 4: px, py, pz (-z) momentum in GeV/c
  (momentum :type momentum)
  ;; 5: x position coordinate in cm
  x
  ;; 6: y position coordinate in cm
  y
  ;; 7: t time since first interaction
  ;; (or since entrance into atmosphere) in nsec
  ;; [for additional muon information or id 95/96: z coordinate in cm]
  time)

(defbin cherenkov-photon (:size-check 7)
  ;; 1: number of cherenkov photons in bunch
  number
  ;; 2: x position coordinate in cm
  x
  ;; 3: y position coordinate in cm
  y
  ;; 4: u direction cosine to x-axis
  u
  ;; 5: v direction cosine to y-axis
  v
  ;; 6: t time since first interaction (or since entrance into atmosphere) in nsec
  time
  ;; 7: height of production of bunch in cm
  height)

(defbin mother-particle (:size-check 7)
  ;; 1: particle description encoded as:
  ;; -(part. id * 100 + hadronic generation of mother)
  description
  ;; 2, 3, 4: px, py, pz (-z) momentum in GeV/c
  (momentum :type momentum)
  ;; 5: x position coordinate in cm
  x
  ;; 6: y position coordinate in cm
  y
  ;; 7: z position coordinate in cm at creation point
  z)

(defbin grandmother-particle (:size-check 7)
  ;; 1: particle description encoded as:
  ;; -(part.id * 1000 + electromagnetic generation of mother)
  description
  ;; 2, 3, 4: px, py, pz (-z) momentum in GeV/c
  (momentum :type momentum)
  ;; 5: ext. electromagn. gen counter of final particle
  ext-electromagn-gen-counter
  ;; 6: penetration depth in g/cm^2
  depth
  ;; 7: -z position coordinate in cm at interaction point
  ;; (negative to make a difference with mother! )
  z)

(defbin multithin-weight ()
  ;; NOTE: not know what is "8888jjj"?
  ;; 1: identification word 8888jjj
  id
  ;; i(2..7): weight of ith thinning mode
  (weight :type (:array :float 6)))

(defbin particle-data-sub-block (:size-check 273)
  (particles :type (:array particle 39)))

(defbin cherenkov-photon-data-sub-block (:size-check 273)
  (cherenkov-photon :type (:array cherenkov-photon 39)))

(defbin mother-particle-data-sub-block (:size-check 273)
  (particles :type (:array mother-particle 39)))

(defbin grandmother-particle-data-sub-block (:size-check 273)
  (particles :type (:array grandmother-particle 39)))

(defbin multithin-weight-data-sub-block (:size-check 273)
  (weights :type (:array multithin-weight 39)))

;;; Longitudinal SubBlock
(defbin long-hist ()
  ;; 4: vertical (rsp. slant) depth of step j in g/cm^2
  vertical-depth
  ;; 5: number of γ-rays at step j
  gamma
  ;; 6: number of e+ at step j
  e+
  ;; 7: number of e- at step j
  e-
  ;; 8: number of mu+ at step j
  mu+
  ;; 9: number of mu- at step j
  mu-
  ;; 10: number of hadronic particles at step j
  hadronic
  ;; 11: number of all charged particles at step j
  charged
  ;; 12: number of nuclei at step j
  nuclei
  ;; 13: number of Cherenkov photons at step j
  cherenkov)

(defbin longitudinal-sub-block (:size-check 273)
  ;; 1: "LONG"
  (type :type :str)
  ;; 2: event number
  event-number
  ;; 3: particle id (particle code or A * 100 + Z for nuclei)
  particle-id
  ;; 4: total energy in GeV
  energy
  ;; 5: (total number of longitudinal steps) * 100 +
  ;; number of longitudinal blocks/shower
  total-blocks
  ;; 6: current number m of longitudinal block
  block-id
  ;; 7: altitude of first interaction in g/cm^2
  first-interaction-altitude
  ;; 8: zenith angle θ in radian
  zenith-angle
  ;; 9: azimuth angle φ in radian
  azimuth-angle
  ;; 10..13: see `energy-cutoff'
  (energy-cutoff :type energy-cutoff)
  ;; 10 * n + (4..13): see `long-hist'
  (long-hist :type (:array long-hist 26)))

;;; The following is Corsika output structure that is not defined by
;;; `defbin'. The following defination declares the structure of
;;; corsika binary output.

(defstruct run
  header events end)

(defstruct event
  header datablocks long-blocks end)
