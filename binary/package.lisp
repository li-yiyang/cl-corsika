(defpackage #:cl-corsika/binary
  (:use :cl :cl-corsika/physics)
  (:import-from #:ieee-floats
                #:decode-float32)
  (:import-from #:alexandria
                #:make-keyword
                #:with-gensyms)
  (:documentation "This package reads Corsika binary outputs.")
  (:export
   ;; Corsika Binary Interfaces
   ;; These functions/macros are recommanded to use
   #:with-open-corsika
   
   ;; Corsika Binary readers
   ;; Note that it's recommand not to use them directly...
   
   ;; data structure
   #:run
   #:run-header
   #:run-events
   #:run-end
   #:event
   #:event-header
   #:event-datablocks
   #:event-long-blocks
   #:event-end

   ;; exported by `defbin'
   #:cherenkov-cal-info
   #:cherenkov-cal-info-bunch-size
   #:cherenkov-cal-info-detector-number-x
   #:cherenkov-cal-info-detector-number-y
   #:cherenkov-cal-info-grid-space-x
   #:cherenkov-cal-info-grid-space-y
   #:cherenkov-cal-info-length-x
   #:cherenkov-cal-info-length-y
   #:cherenkov-cal-info-output-file-p
   #:cherenkov-photon
   #:cherenkov-photon-data-sub-block
   #:cherenkov-photon-data-sub-block-cherenkov-photon
   #:cherenkov-photon-height
   #:cherenkov-photon-number
   #:cherenkov-photon-time
   #:cherenkov-photon-u
   #:cherenkov-photon-v
   #:cherenkov-photon-x
   #:cherenkov-photon-y
   #:cherenkov-scatter-range
   #:cherenkov-scatter-range-x
   #:cherenkov-scatter-range-y
   #:cherenkov-wavelength
   #:cherenkov-wavelength-lower
   #:cherenkov-wavelength-upper
   #:conex-info
   #:conex-info-em-particle-high-energy
   #:conex-info-em-particle-low-energy
   #:conex-info-hadron-high-energy
   #:conex-info-hadron-low-energy
   #:conex-info-min-vertical-depth
   #:conex-info-muon-high-energy
   #:conex-info-muon-low-energy
   #:conex-info-observation-curvature-flag
   #:conex-info-sampling-em-particle-wtmax
   #:conex-info-sampling-hadronic-whmax
   #:conex-info-sampling-muon-wtmax
   #:conex-info-thining-em-particle-wtmax
   #:conex-info-thining-hadronic-whmax
   #:energy-cutoff
   #:energy-cutoff-electron
   #:energy-cutoff-hadron
   #:energy-cutoff-muon
   #:energy-cutoff-photon
   #:energy-spectrum
   #:energy-spectrum-lower-limit
   #:energy-spectrum-slope
   #:energy-spectrum-upper-limit
   #:event-end-emerged-em-particle-number
   #:event-end-event-number
   #:event-end-longitudinal-distribution-parameters
   #:event-end-nkg-output
   #:event-end-shower-statistics
   #:event-end-type
   #:event-end-weighted-electron-number
   #:event-end-weighted-hadron-number
   #:event-end-weighted-muon-number
   #:event-end-weighted-photon-number
   #:event-end-xi2-per-degree
   #:event-header-array-angle
   #:event-header-augerhit-reserved
   #:event-header-azimuth-angle
   #:event-header-cerenkov-flag
   #:event-header-charm-generation-flag
   #:event-header-cherenkov-cal-info
   #:event-header-cherenkov-rsp-auger-num
   #:event-header-cherenkov-wavelength
   #:event-header-computer-flag
   #:event-header-conex-info
   #:event-header-curved-flag
   #:event-header-detector-distance
   #:event-header-dpmjet-cross-section-flag
   #:event-header-dpmjet-interaction-flag
   #:event-header-earth-mag-field
   #:event-header-egs4-flag
   #:event-header-egs4-step-length-factor
   #:event-header-energy
   #:event-header-energy-cutoff
   #:event-header-energy-spectrum
   #:event-header-event-number
   #:event-header-first-target-number
   #:event-header-first-target-z
   #:event-header-hadron-origin-flag
   #:event-header-high-energy-hadr-model
   #:event-header-horizontal-axis-altitude
   #:event-header-inclined-plane
   #:event-header-integer-seed-sequence
   #:event-header-low-energy-hadr-model
   #:event-header-max-radius-thin-to-corecut
   #:event-header-momentum
   #:event-header-multi-thin-info
   #:event-header-muon-info-output-p
   #:event-header-muon-multiple-scatter-flag
   #:event-header-neutrino-flag
   #:event-header-nflain
   #:event-header-nflche
   #:event-header-nfldif
   #:event-header-nflpi0
   #:event-header-nflpif
   #:event-header-nfragm
   #:event-header-nkg-flag
   #:event-header-nkg-radial-distribution-range
   #:event-header-observation-info
   #:event-header-offset-rand-call-div-1e6
   #:event-header-offset-rand-call-mod-1e6
   #:event-header-particle-id
   #:event-header-phi-lower
   #:event-header-phi-upper
   #:event-header-program-version
   #:event-header-qgsjet-cross-section-flag
   #:event-header-qgsjet-interaction-flag
   #:event-header-rand-sequences-number
   #:event-header-run-number
   #:event-header-scattered-core-location
   #:event-header-sibyll-cross-section-flag
   #:event-header-sibyll-interaction-flag
   #:event-header-skim-incidence-flag
   #:event-header-start-altitude
   #:event-header-start-date
   #:event-header-starting-height
   #:event-header-strip-parallel-half-width
   #:event-header-theta-lower
   #:event-header-theta-upper
   #:event-header-thin-em-particle-weigh-max
   #:event-header-thin-hadronic-weight-max
   #:event-header-thin-level-em-particle
   #:event-header-thin-level-hadronic
   #:event-header-transition-height/low-energy
   #:event-header-type
   #:event-header-venus-nexus-epos-cross-section-flag
   #:event-header-viewcone-inner-angle
   #:event-header-viewcone-outer-angle
   #:event-header-zenith-angle
   #:grandmother-particle
   #:grandmother-particle-data-sub-block
   #:grandmother-particle-data-sub-block-particles
   #:grandmother-particle-depth
   #:grandmother-particle-description
   #:grandmother-particle-ext-electromagn-gen-counter
   #:grandmother-particle-momentum
   #:grandmother-particle-z
   #:lateral-distribution
   #:lateral-distribution-x
   #:lateral-distribution-xy
   #:lateral-distribution-y
   #:lateral-distribution-yx
   #:long-hist
   #:long-hist-charged
   #:long-hist-cherenkov
   #:long-hist-e+
   #:long-hist-e-
   #:long-hist-gamma
   #:long-hist-hadronic
   #:long-hist-mu+
   #:long-hist-mu-
   #:long-hist-nuclei
   #:long-hist-vertical-depth
   #:longitudinal-sub-block
   #:longitudinal-sub-block-azimuth-angle
   #:longitudinal-sub-block-block-id
   #:longitudinal-sub-block-energy
   #:longitudinal-sub-block-energy-cutoff
   #:longitudinal-sub-block-event-number
   #:longitudinal-sub-block-first-interaction-altitude
   #:longitudinal-sub-block-long-hist
   #:longitudinal-sub-block-particle-id
   #:longitudinal-sub-block-total-blocks
   #:longitudinal-sub-block-type
   #:longitudinal-sub-block-zenith-angle
   #:mag-field
   #:mag-field-x
   #:mag-field-z
   #:momentum
   #:momentum-x
   #:momentum-y
   #:momentum-z
   #:mother-particle
   #:mother-particle-data-sub-block
   #:mother-particle-data-sub-block-particles
   #:mother-particle-description
   #:mother-particle-momentum
   #:mother-particle-x
   #:mother-particle-y
   #:mother-particle-z
   #:multi-thin-info
   #:multi-thin-info-active-output-flag
   #:multi-thin-info-em-particle-energy-fraction
   #:multi-thin-info-em-particle-weight-limit
   #:multi-thin-info-gzip-output-flag
   #:multi-thin-info-hadronic-energy-fraction
   #:multi-thin-info-hadronic-weight-limit
   #:multi-thin-info-interest-particle-threshold
   #:multi-thin-info-modes-number
   #:multi-thin-info-offset-rand-call-exceed-1e6
   #:multi-thin-info-offset-rand-call-mod-1e6
   #:multi-thin-info-pipe-buffer-output-flag
   #:multi-thin-info-rand-int-seed-sequence
   #:multi-thin-info-weighted-index
   #:multithin-weight
   #:multithin-weight-data-sub-block
   #:multithin-weight-data-sub-block-weights
   #:multithin-weight-id
   #:multithin-weight-weight
   #:nkg-output
   #:nkg-output-electron-distribution-bin
   #:nkg-output-electron-distribution-dis
   #:nkg-output-electron-number
   #:nkg-output-electron-number-height-cm
   #:nkg-output-electron-number-height-g/cm2
   #:nkg-output-level1-lateral-dis
   #:nkg-output-level2-lateral-dis
   #:nkg-output-local-pseudo-age-level1
   #:nkg-output-local-pseudo-age-level2
   #:nkg-output-pseudo-age
   #:observation-info
   #:observation-info-heights
   #:observation-info-number
   #:observation-plane
   #:observation-plane-depth
   #:observation-plane-phi
   #:observation-plane-theta
   #:observation-plane-x
   #:observation-plane-y
   #:observation-plane-z
   #:particle
   #:particle-data-sub-block
   #:particle-data-sub-block-particles
   #:particle-description
   #:particle-momentum
   #:particle-time
   #:particle-x
   #:particle-y
   #:run-end-event-number
   #:run-end-run-number
   #:run-end-type
   #:run-header-aatm
   #:run-header-array-angle
   #:run-header-batm
   #:run-header-catm
   #:run-header-ceta
   #:run-header-cherenkov-scatter-range
   #:run-header-cka
   #:run-header-cstrba
   #:run-header-egs4-flag
   #:run-header-energy-cutoff
   #:run-header-energy-spectrum
   #:run-header-hlay
   #:run-header-nflain
   #:run-header-nflche+100nfragm
   #:run-header-nfldif
   #:run-header-nflpi0+100nflpif
   #:run-header-nkg-flag
   #:run-header-observation-level-info
   #:run-header-observation-plane
   #:run-header-phisical-constants
   #:run-header-program-version
   #:run-header-run-number
   #:run-header-shower-number
   #:run-header-slant-flag
   #:run-header-start-date
   #:run-header-type
   #:scattered-core-location
   #:scattered-core-location-x
   #:scattered-core-location-y
   #:shower-statistics
   #:shower-statistics-weighted-electrons-number
   #:shower-statistics-weighted-hadrons-number
   #:shower-statistics-weighted-muon-number
   #:shower-statistics-weighted-particle-number
   #:shower-statistics-weighted-photon-number
   ))

(in-package :cl-corsika/binary)
