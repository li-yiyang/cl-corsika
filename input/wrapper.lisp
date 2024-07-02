(in-package :cl-corsika/input)

;; This is the wrapper for low-level commands defined in
;; corsika-input.lisp. These functions should be used to
;; construct the Corsika output.
;;
;; Example:
;;
;;    (with-output-to-string (*standard-output*)
;;
;;      ;; Physics Parts
;;      (setup-primary-particle 5626 1e5)            ;; -----|-----
;;      (setup-first-interaction :target :Nitrogen)  ;;      *
;;      (setup-model :EPOS :GHEISHA                  ;;     /|\
;;                   :)                              ;;    /\|/\
;;      (setup-observation-pane :sea-level 0)        ;;  =========
;;
;;      ;; Simulation Configurations
;;      (setup-utils)
;;      (exit))

;; ========== Primary Particle Input Controls ==========

(defun setup-primary-particle (particle-id energy
                               &key (showers 1)
                                 (slope -2.7 slop-set?)
                                 (theta 0.0)
                                 (phi   '(-180 . 180)))
  "Set up the primary particle simulation infomations

Parameters:
+ `particle-id' e.g. 5626 for Fe;

  (see CORSIKA_GUIDE7.7500.pdf Appendix D for details)
+ `energy' e.g. `1e5' for fixed energy,
  or `(1e5 . 2e5)' for energy range;

Keywords:
+ `slope' for energy spectrum slope, only availbe with
  energy range input (warn if misused);
+ `showers' for shower number generated per run;
+ `theta' for zenith angle (degree), `0.0' for single angle,
  or `(0.0 45.0)' for range;
+ `phi' for azimuth angle (degree), same like `theta';
+ 

See:
`cl-corsika/input::prmpar', `cl-corsika/input::prmpar',
`cl-corsika/input::erange', `cl-corsika/input::eslope',
`cl-corsika/input::thetap', `cl-corsika/input::phip'. "
  (let* ((particle-id   (truncate particle-id))
         (slope         (float slope))
         (energy-range? (consp energy))
         (energy-low    (float (if energy-range? (car energy) energy)))
         (energy-high   (float (if energy-range? (cdr energy) energy)))
         (theta-range?  (consp theta))
         (theta-low     (float (if theta-range? (car theta) theta)))
         (theta-high    (float (if theta-range? (cdr theta) theta)))
         (phi-range?    (consp phi))
         (phi-low       (float (if phi-range?   (car phi)   phi)))
         (phi-high      (float (if phi-range?   (cdr phi)   phi))))
    ;; warn if set slope for single energy range
    (when (and (not energy-range?) slop-set?)
      (warn (format nil "Energy spectrum slope is meaningless for primary energy ~F. "
                    energy)))
    ;; generate the Corsika Input
    (nshow showers)
    (prmpar particle-id)
    (when energy-range?
      (eslope slope))
    (erange energy-low energy-high)
    (thetap theta-low  theta-high)
    (phip   phi-low    phi-high)))

;; ========== First Interaction ==========

(defun setup-first-interaction (&key (height 0.0) (target :random) (depth 0.0))
  "Setup first interaction position.

Sets the first interaction infomation:
+ `height' for first interaction height (cm), or `:random' (0.0) for
  random determined by mean free path of the particle;
+ `target' to be one of `:Nitrogen' (1), `:Oxygen' (2), `:Argon' (3),
  or `:random' for random select, or integer number for direct
  specify;
+ `depth' for starting altitude (in g/cm^2) for atmosphere depth

See: `cl-corsika/input::fixhei', `cl-corsika/input::fixchi'. "
  (let ((height (cond ((numberp height) (float height))
                      ((eq height :random) 0.0)
                      (t (error "Height should be either number or `:random'. "))))
        (target (cond ((numberp target) (truncate target))
                      (t (ecase target
                           (:Nitrogen 1)
                           (:Oxygen   2)
                           (:Argon    3)
                           (:random   0))))))
    (fixhei height target)
    (when (> height 0.0)
      (fixchi (float depth)))))

;; ========== Simulation Model Controls ==========
;; CORSIKA_GUIDE7.7500
;; 3 Program Options

(defparameter *corsika-source-path*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "CORSIKA_PATH") "corsika-77500"))
  "Where the Corsika source directory is placed.

Example:

    \"/Users/ryo/corsika-77500/\"
")

;; 3.1 High-Energy Hadronic Interaction Models
;; 3.1.1 DPMJET Option

;; 3.1.2 EPOS Option
;; for using EPOS you first have to select EPOS option when extracting the
;; FORTRAN code from the source file. 

;; Dump from all-inputs-epos in run/ subdirectory in corsika-77500
(defun setup-epos (&key (input "epos.param")
                     (inics "epos.inics")
                     (iniev "epos.iniev")
                     (initl "epos.initl")
                     (inirj "epos.inirj")
                     (inihy "epos.ini1b")
                     (debug-level 0)
                     (check nil)
                     (histo nil)
                     (data  nil)
                     (copy  nil)
                     (cross-section t)
                   &allow-other-keys)
  "Setup EPOS (Energy conserving quantum mechanical multi-scattering approach,
based on Partons, Off-shell remnants and Splitting parton ladders) options.

Keywords: are all EPOS parameters.

See: `cl-corsika/input::epos', `cl-corsika/input::epopar'. "
  ;; In your input file you may supply the keyword.
  ;; (Use EPOS routine for high-energy hadronic interactions.)
  (let ((epos-path (truename
                    (uiop:ensure-directory-pathname
                     (merge-pathnames "epos" *corsika-source-path*)))))
    (epos t debug-level)
    (epopar (format nil "input ~A" (merge-pathnames input epos-path)))
    (macrolet ((set-epopar (var)
                 `(epopar (format nil "fname ~(~A~) ~A" ',var
                                  (if ,var (merge-pathnames ,var epos-path)
                                      "none")))))
      ;; The standard parameters for EPOS are set in subroutine aaset of the
      ;; epos-bas-lhc.f file. If in your calling directory the data sets
      ;; epos.inics.lhc, epos.iniev, epos.ini1b, epos.inirj.lhc, and epos.initl
      ;; are not existent or not compatible with the selected parameters, they
      ;; will be calculated at the first call of subroutine psaini of
      ;; epos-sem-xxx.f (which takes some 100 h on a DEC 3000/600 AXP with 175 MHz).
      (set-epopar inics)
      (set-epopar iniev)
      (set-epopar initl)
      (set-epopar inirj)
      (set-epopar inihy)
      (set-epopar check)
      (set-epopar histo)
      (set-epopar data)
      (set-epopar copy)))
  
  ;; The EPOS cross-sections are selected automati- cally when the EPOS option
  ;; has been used for extracting the FORTRAN code from the source file. 
  (eposig cross-section))

;; 3.1.3 HDPM Routines
;; 3.1.4 NEXUS Option
;; 3.1.5 QGSJET and QGSII Options
;; 3.1.6 SIBYLL Option
;; 3.1.7 VENUS Option
;; 3.2 Low-Energy Hadronic Interaction Models
;; 3.2.1 FLUKA Options

;; 3.2.2 GHEISHA Option

(defun setup-gheisha (&key &allow-other-keys)
  "Setup GHEISHA (Gamma Hadron Electron Interaction SHower code) options."
  t)

;; 3.2.3 URQMD Options
;; 3.3 Electromagnetic Interactions (NKG/EGS4 Option)
;; 3.3.1 NKG Treatment
;; 3.3.2 EGS4 Treatment

;; Setup model
(defun setup-model (high-energy low-energy
                    &rest args
                    &key
                      ;; hadron model                      
                      (hadron-ecut        0.3)
                      (muon-ecut          0.3)
                      (electrons-ecut     0.003)
                      (photons-ecut       0.003)
                      (hadflg '(0 0 0 0 0 2))
                      ;; gamma
                      (gamma-factor-cut   1e11)
                      ;; scattering
                      (muon-scattering    :Moliere)
                      (scatter-step       1.0)
                      ;; addition info
                      (addition-info      :all)
                      ;; electronmagnet model
                      (use-NKG            t)
                      (NKG-outer-radius   200e2)
                      (use-EGS4           t)
                      ;; longitudinal simulation
                      (longitudinal '(t 10.0 nil nil))
                    &allow-other-keys)
  "Set the hadronic and eletronic model parameters.

Parameters:
+ `high-energy' sets the hadron model for high energy;
+ `low-energy' sets the hadron model for low energy;

Keywords:
+ `muon-ecut', `hadron-ecut', `electrons-ecut', `photons-ecut'
  are used for energy cut switching high/low level model
+ `gamma-factor-cut' set the gamma factor cut
+ `scatter-step' for multi scattering step length fact
+ `muon-scattering': set the scattering model for muon,
  can be `:Moliere' (default) or `:Gauss';
+ `addition-info': set the addition info for
  + `:mu' or `:muon': additional infomation for muon
  + `:em' or `:electromagnetic': additional infomation for
    electromagnetic particles;
  + `:nu' or `:neutrino': additional neutrino information;
  + a list containing above options, like (:muon :em);
  + `:all' for all of the above, equal to (:muon :em :neutrino);
  
  see Corsika GUIDE 7.7500 Table 10,
  or `cl-corsika/binary:particle-data-sub-block';
"
  ;; hadronic model for high energy
  (ecase high-energy
    (:EPOS    (apply #'setup-epos args)))
  ;; hadronic model for low energy
  (ecase low-energy
    (:GHEISHA (apply #'setup-gheisha args)))

  ;; hadron interaction and fragmentation flags
  (apply #'hadflg hadflg)

  ;; Energy cuts for high/low energy
  (ecuts (float hadron-ecut)
         (float muon-ecut)
         (float electrons-ecut)
         (float photons-ecut))
  (ectmap gamma-factor-cut)

  (let* ((flags (cond ((listp addition-info) addition-info)
                      ((eq addition-info :all) '(:mu :em :nu))
                      (t (list addition-info))))
         (mu? (or (find :muon flags)
                  (find :mu   flags)))
         ;; (em? (or (find :electromagnetic flags)
         ;;          (find :em              flags)))
         ;; (nu? (or (find :neutrino flags)
         ;;          (find :nu       flags)))
         )
    ;; additional info for muons
    (muaddi (if mu? t nil))
    ;; additional info for em.
    ;; (emaddi (if em? t nil))
    ;; additional info for nu
    ;; (nuaddi (if nu? t nil))
    )

  ;; scatter step length
  (stepfc scatter-step)

  ;; muon scatter model
  (cond ((eq muon-scattering :Moliere) (mumult t))
        ((eq muon-scattering :Gauss)   (mumult nil))
        ((eq muon-scattering nil)      (mumult nil))
        (t                             (mumult t)))

  ;; em. interaction flags
  (elmflg use-NKG use-EGS4)
  ;; for NKG
  (radnkg NKG-outer-radius)

  (apply #'longi longitudinal))

;; ========== Set Observation Pane (Geo, Mag, ATM) ==========

(defun setup-observation-pane (&key
                                 (altitude       0.0)
                                 (array-rotation 0.0)
                                 (magnet '(20.4 . 43.23))
                                 (horizontal-mag 20.4  horizontal-mag-set?)
                                 (vertical-mag   43.23 vertical-mag-set?)
                                 (atmosphere     :US-standard))
  "Setup the observation pane.

Keywords:
+ `altitude': observation altitude (in cm);
+ `array-rotation': rotation of observation array (in degree to North);
+ `magnet', `horizontal-mag', `vertical-mag': the earch magnet, μT
+ `atmosphere':
  + can be keywords like `:default', `:us-standard' and so on;
  + or a integer for atmosphere model;
  + or the full form:

    (atmod
     (...)  ;; ATMA
     (...)  ;; ATMB
     (...)  ;; ATMC
     (...)) ;; ATMLAY

See:
`cl-corsika/input::obslev', `cl-corsika/input::arrang',
`cl-corsika/input::magnet', `cl-corsika/input::atmod',
`cl-corsika/input::atma', `cl-corsika/input::atmb',
`cl-corsika/input::atmc', `cl-corsika/input::atmlay'. "
  (obslev (float altitude))
  (arrang (float array-rotation))
  
  ;; setup the earth magnet
  (let ((h-mag (if horizontal-mag-set? horizontal-mag (car magnet)))
        (v-mag (if vertical-mag-set?   vertical-mag   (cdr magnet))))
    (magnet h-mag v-mag))

  ;; setup the atmosphere model
  (cond ((keywordp atmosphere)
         (case atmosphere
           ((:US-standard :default) (atmod 1))
           (:AT115  (atmod 2))
           (:AT223  (atmod 3))
           (:AT511  (atmod 4))
           (:AT616  (atmod 5))
           (:AT822  (atmod 6))
           (:AT1014 (atmod 7))
           (:AT1224 (atmod 8))
           (t (error "Not implemented yet. Contribution is needed. "))))
        ((and (integerp atmosphere)) ;; is is safe?
         (atmod atmosphere))
        ((listp atmosphere)
         (atmod (car atmosphere))
         (mapcar (lambda (fn args) (apply fn args))
                 (list #'atma #'atmb #'atmc #'atmlay)
                 (cdr atmosphere)))
        (t (error "Malformed `atomsphere' input. "))))

;; ========== Random Number Sequence Set Up ==========
;; this should be called within the `setup-utils'. 

(defun setup-random-seed (&key
                            (init-calls    0)
                            (hadron-seed   1)
                            (EGS4-seed     2)
                            (CERENKOV-seed 3 cerenkov-set?)
                            (IACT-seed     4 IACT-set?)
                            (HERWIG-seed   5 HERWIG-set?)
                            (PARALLEL-seed 6 PARALLEL-set?)
                            (CONEX-seed    7 CONEX-set?)
                          &allow-other-keys)
  "Set up random number seeds.

Keywords:
+ `init-calls': Contains the number of calls N_{in} to the generator
  that are performed for initialization.

      ISEED(2, k) = N_{in} % 1,000,000,000
      ISEED(3, k) = floor(N_{in}, 1,000,000,000)
+ `hadron', `EGS4', `CERENKOV', `IACT', `HERWIG', `PARALLEL'
  and `CONEX' are seed number for ISEED(1, k). 

See: `cl-corsika/input::seed'. "
  (let ((iseed2 (mod   init-calls 1000000000))
        (iseed3 (floor init-calls 1000000000)))
    (seed hadron-seed iseed2 iseed3)
    (seed EGS4-seed   iseed2 iseed3)
    (when (or CERENKOV-set? IACT-set? HERWIG-set? PARALLEL-set? CONEX-set?)
      (seed CERENKOV-seed iseed2 iseed3))
    (when (or IACT-set? HERWIG-set? PARALLEL-set? CONEX-set?)
      (seed IACT-seed iseed2 iseed3))
    (when (or HERWIG-set? PARALLEL-set? CONEX-set?)
      (seed HERWIG-seed iseed2 iseed3))
    (when (or PARALLEL-set? CONEX-set?)
      (seed PARALLEL-seed iseed2 iseed3))
    (when CONEX-set?
      (seed CONEX-seed iseed2 iseed3))))

;; ========== Debug Infomation ==========
;; this should be called within the `setup-utils'.

(defun setup-debug (&key
                      (debug nil)
                    &allow-other-keys)
  (when debug (warn "Debug is not available right now. "))
  ;; copied from all-inputs-epos
  (debugging nil 6 nil 1000000))

;; ========== Simulation Utilities ==========

;; `*run-id*' should be automatically increased when
;; starting a new simulation. 
(defparameter *run-id* 1
  "The number of run. ")

(defun incf-run-id ()
  "Increase `*run-id*'. "
  (incf *run-id*))

(defun reset-run-id ()
  "Reset `*run-id*' to 1. "
  (setf *run-id* 1))

(defun setup-utils (&rest args
                    &key
                      (run-id      *run-id*)
                      (start-event 1)
                      (print-event 1)
                      (debug       nil)
                      (data-base   nil)
                      (dir         (uiop:getcwd))
                      (user (or (uiop:getenv "USER")
                                "CL-CORSIKA")))
  "Setup simulation utils.

Keywords:
+ `run-id': see `cl-corsika/input::RUNNR';
+ `start-event': number of first shower event, count from
  `start-event' (increasing by 1);
+ `print-event': max number of printed events;
+ `data-base': whether or not write data base file;
+ `debug': see `cl-corsika/input::setup-debug';
+ `dir': output directory, default to current working directory;
+ `user': if $USER is not found, using `CL-CORSIKA' instead;
"
  (runnr  run-id)
  (evtnr  start-event)
  
  ;; setup random number seeds
  (apply  #'setup-random-seed args)

  ;; data base file
  (datbas data-base)
  
  ;; setup debug info
  (when debug (apply #'setup-debug args))
  
  (maxprt print-event)
  (direct (format nil "~A" (truename (uiop:ensure-directory-pathname dir))))
  (user   user))

(defun %with-corsika-input (output input)
  "Output the Corsika `input' to `output'.

Output could be:
+ `:stream' for make a string input stream;
+ `nil' for just return `input';
+ a stream that will format `input' to stream;
+ otherwise, format `input' to `*standard-output*'. "
  (cond ((eq output :stream) (make-string-input-stream input))
        ((streamp output)    (format output "~A" input))
        ((eq output nil)     input)
        ((or (stringp output)
             (pathnamep output))
         (with-open-file (stream output :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
           (format stream "~A" input)))
        (t                   (format t "~A" input))))

(defmacro with-corsika-input ((output &key debug) &body body)
  "With Corsika Input write to `output'.

Keywords:
+ `debug': if debug is set to non-nil, format input to standard-output.

Example:

    (with-corsika-input (*input-file-path* :debug t)
      (setup-primary-particle 5626 1e5)
      (setup-first-interaction)
      (setup-model :EPOS :GHEISHA)
      (setup-observation-pane :altitude 0.0)
      (setup-utils :dir *output-dir*))
"
  `(%with-corsika-input ,output
                        (,(if debug 'print 'progn)
                         (with-output-to-string (*standard-output*)
                           ,@body
                           (exit)))))
