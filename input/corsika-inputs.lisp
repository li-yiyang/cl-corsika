(in-package :cl-corsika/input)

;; This file is first dump from CORSIKA_GUIDE_7500 with the following code:
;; 
;; (defun scanner (fstream output)
;;   (let ((line (read-line fstream nil "")))
;;     (when (ppcre:scan "^4\\.\\d+$" line) ; /^4.\d+$/ => match for input chapter
;;       (let ((title (read-line fstream nil nil)))
;;         (when (not (ppcre:scan "^4\\.\\d+$" title))
;;           (format output "~&~%;; ~a ~a~%" line title)
;;           ;; scan parameters
;;           (loop for line = (read-line fstream nil nil)
;;                 while line                ; not EOF
;;                 if (ppcre:scan "^Format" line)
;;                   return (progn
;;        ;; def-input
;;        (format output
;;          "~%(def-input (~(~A~) \"~@:(~A~)\") (~{~A~^ ~})~%  (:documentation \"~A"
;;          (first define) (first define) (rest define) line))
;;                 else if (not (string= line ""))
;;                        collect line into define)
;;           ;; scan documentation
;;           (loop for line = (read-line fstream nil nil)
;;                 while line
;;                 if (ppcre:scan "^4\\.\\d+$" line) ; found next define
;;                   return (progn
;;        ;; unread Format line
;;        (file-position fstream (- (file-position fstream) (1+ (length line))))
;;        ;; add documentation
;;        (format output "~&~{~A~^~%~}\"))" lines))
;;                 else if (not (string= line ""))
;;                        collect line into lines))))))
;; 
;; (with-open-file (fstream "corsika_guide.txt")
;;   (with-open-file (output "corsika-inputs.lisp" :direction :output)
;;     ;; drop previous lines (change the dotimes)
;;     (dotimes (i 4000) (read-line))
;;     ;; scan the spec doc (change the dotimes)
;;     (dotimes (i 4000) (scanner fstream output))))
;; 
;; But I have to admit that the plain result is not so good...
;; So I spent a lot of time manually refine the results.
;;
;; It was a time consuming work, and currently, I only done some formatting
;; works, the inner logic and the correctness of the documentation (I mean,
;; the assertions may not so properly constructed).
;;
;; P.S.: if only I have the origin TeX source of the CORSIKA_GUIDE...
;; I could done this work better and more automatically... 

;; 4.1 Run Number

(def-input (runnr "RUNNR") (&optional (id 1))
  (:documentation "Run number of this simulation.
This number is used to form part of the name of the various output files.")
  (:formats (integer id))
  (:asserts (<= 0 id 999999)))

;; 4.2 First Event Number

(def-input (evtnr "EVTNR") (&optional (id 1))
  (:documentation "Event number of first shower.
The second shower will get number SHOWNO+1 and so on.")
  (:formats (integer id))
  (:asserts (<= 1 id 999999)))

;; 4.3 Random Number Generator Initialization

(def-input (seed "SEED")
    (&optional (seed1 1) (seed2 0) (seed3 0))
  (:documentation "Random Number Generator Initialization.

    SEED    ISEED(i,k),i=1... 3

Format = (A4, 3I), Defaults = k, 0, 0
+ `k' is the seed of the random number sequence k.
+ `seed1' contains the seed of the random number sequence k.
+ `seed2' and `seed3' contain the number of calls N_{in} to the generator
  that are performed for initialization such that

      N_{in} = ISEED(2, k) + 10^9 * ISEED(3, k)

At present at most k = 7 sequences are used:
+ 1 for the hadron shower;
+ 2 for the EGS4 part;
+ 3 for the simulation of Cherenkov photons (only for CERENKOV option);
+ 4 for the random offset of Cherenkov telescope systems with respect
  of their nominal positions (only for IACT option) rsp. for the offset
  scattering of the core relative to the Auger detector (AUGER-HIT option);
+ 5 for the HERWIG routines in the NUPRIM option;
+ 6 for the PARALLEL option;
+ 7 for the CONEX option.

Their activation follows the sequence of occurrence of the keyword lines.
The sequences 9 . . . 14 are activated by the keyword MSEED.
(see Sect. 4.59 page 92).

At minimum 2 seeds should be activated. If not sufficient seeds are
activated, the default values are taken.

The use of ISEED(2, k) > 0 and especially of ISEED(3, k) > 0 should be
avoided as presetting the random number generator by billions of calls
 needs considerable computing time. To get different random sequences
it is sufficient to modify ISEED(1, k).

When the eventio and other separate functions are enabled in the IACT
option, an external random generator may be used.")
  (:formats (integer seed1 seed2 seed3))
  (:asserts (<= 1 seed1 900000000)))

;; 4.4 Number of Showers

(def-input (nshow "NSHOW") (&optional (counts 10))
  (:documentation "Number of showers to be generated in a run.")
  (:formats (integer counts))
  (:asserts (>= counts 1)))

;; 4.5 Primary Particle Definition

(def-input (prmpar "PRMPAR") (&optional (particle-id 14))
  (:documentation "Particle type of the primary particle.
See Table 4 (page 123) for the particle codes.")
  (:formats (integer particle-id))
  (:asserts (<= 1 particle-id 5656)))

;; 4.6 Energy Range

(def-input (erange "ERANGE") (&optional (low 1e4) (high low))
  (:documentation "Energy range.
+ `low' : Lower limit of the primary particle energy range (in GeV);
+ `high': Upper limit of the primary particle energy range (in GeV).

The primary energy is selected at random out of this interval.
If `low' = `high', the primary energy is fixed at this value.
The energies are total energies and include the particle rest mass.

For primary photons and electrons ULIMIT ≤ 1.E11 GeV (but keep in mind
that no LPM-effect is included in NKG!). For primary hadrons and nuclei
no upper limit is recommended, but the user should take care not to
over-stretch the selected hadronic interaction model. See also Ref. [26].

It is recommended for HDPM: `high' ≤ 1 * 108 GeV and for VENUS:
`high' ≤ 2 · 107 GeV. For the DPMJET option above ULIMIT > 1010 GeV
this model becomes uncertain.

This keyword is not available in the STACKIN option.")
  (:formats (float low high))
  (:asserts (>= low high)))

;; 4.7 Slope of Energy Spectrum

(def-input (eslope "ESLOPE") (&optional (slope 0.0))
  (:documentation "Exponent γ of differential primary energy spectrum.
The primary energy is taken at random from an exponential energy spectrum
of the form dN/dE0 ∝ E0^γ.

`slope' has no meaning in case of fixed primary energy.

The energies are total energies and include the particle rest mass.

This keyword is not available in the STACKIN option.
")
  (:formats (float slope))
  (:asserts t))

;; 4.8 Zenith Angle Definition

(def-input (thetap "THETAP") (&optional (low 0.0) (high low))
  (:documentation "Zenith Angle Definition of theta. 
+ `low' : Low edge of zenith angle range of primary particle (in degree);
+ `high': High edge of zenith angle range of primary particle (in degree).

The zenith angle is selected at random out of this interval in a manner which
respects equal particle fluxes from all solid angle elements of the sky and a
registration by a horizontal flat detector arrangement. THETPR is the angle of
incidence at a horizontal detector. THETPR(i) = 0. is vertical.

If THETPR(1) = THETPR(2), the zenith angle is fixed at this value.")
  (:formats (float low high))
  (:asserts (and (<= 0.0 low high 70.0))))

;; 4.9 Azimuth Angle Definition

(def-input (phip "PHIP") (&optional (low 0.0) (high low))
  (:documentation "Azimuth Angle Definition.
+ `low' : Low edge of azimuth angle range of primary particle (in degree);
+ `high': High edge of azimuth angle range of primary particle (in degree).

The azimuth angle is selected at random out of this interval.
If PHIPR(1) = PHIPR(2), the azimuth angle is fixed at this value.
For φ = 0 degree the shower axis points to magnetic North,
for φ = 90 degree it points to West, see Fig. 1 (page 120).")
  (:formats (float low high))
  (:asserts (and (<= -360.0 low high 360))))

;; 4.10 Viewing Cone Specifications

(def-input (viewcone "VIEWCONE") (&optional (inner 0.0) (outer inner))
  (:documentation "Viewing Cone Specifications
+ `inner': Inner limiting angle of viewing cone (in degree);
+ `outer': Outer limiting angle of viewing cone (in degree).

The VIEWCONE option (see Sect. 3.5.40 page 66) selects the direction of primaries
in a circular cone around the fixed primary direction THETPR(1) and PHIPR(1)
(page 72) with the inner opening VUECON(1) and the outer opening VUECON(2).
The zenith angular dependence of the selected detector geometry is maintained
for flat horizontal rsp. spherical detectors (see Sect. 3.5.41 page 66).

The generation of showers with angles beyond the range of the program validity
is skipped.

This keyword is only available in the VIEWCONE option.")
  (:formats (float inner outer))
  (:asserts (<= 0.0 inner outer 90.0)))

;; 4.11 Starting Grammage

(def-input (fixchi "FIXCHI") (&optional (thick 0.0))
  (:documentation "Starting Grammage.
+ `thick': The vertical starting altitude (in g/cm2 mass overburden) of the
  primary particle is set for all showers. This choice is not effective if the
  height of the first interaction is set by FIXHEI > 0. (see Sect. 4.13 below).
  With this keyword the development of sub-showers starting at the chosen
  altitude within the atmosphere may be followed. It will define the starting (0)
  point of the longitudinal profile with the SLANT option. including when the
  FIXHEI option is selected. The starting altitude must be above the lowest
  observation level.

This keyword is not available in the COASTUSERLIB or STACKIN option.")
  (:formats (float thick))
  (:asserts (<= 0.0 thick)))

;; 4.12 Starting Point of Arrival Timing

(def-input (tstart "TSTART")
    (&optional (by-atmosphere-entrance? nil))
  (:documentation "Starting Point of Arrival Timing
+ `by-atmosphere-entrance?': Flag indicating the starting point of the
  arrival time scale.

  If `by-atmosphere-entrance?' = nil, the first interaction starts the clock.

  If `by-atmosphere-entrance?' = t, the entrance into the atmosphere (rsp. THICK0,
  see above) is taken for starting the internal clock. Additionally, the
  ionization energy loss, the deflection within the Earth’s magnetic field, and
  the generation of Cherenkov photons is enabled for charged hadronic or muonic
  primaries on their path between entering the atmosphere and the first
  interaction, which otherwise is disabled in the standard version (for the
  CERENKOV, CURVED, and SLANT options see page 38, 49, rsp. 61).

  For `by-atmosphere-entrance?' = t the height of the first interaction is written
  negative to element 7 of the event header block.

  This keyword is not available in the CONEX, CURVED, SLANT, or STACKIN options,
  but is set and used by default as .true.; TMARGIN is set .true. by default
  in the CERENKOV and IACT options, but it may be overwritten using the keyword.

  Note that THICK0 is always defined as vertical depth even witht the SLANT option.")
  (:formats (boolean by-atmosphere-entrance?))
  (:asserts t))

;; 4.13 First Interaction Definition

(def-input (fixhei "FIXHEI")
    (&optional (height 0.0) (target 0))
  (:documentation "First Interaction Definition.
+ `height': Fixes the height (in cm) of the first interaction of hadronic
  primaries (rsp. the starting altitude for em-particles) for all showers in
  a run. If `height' = 0., the height of the first interaction is varied
  at random according to the appropriate mean free path. In case of unstable
  hadronic primaries and fixed height the first interaction will not be a decay.

  The fixed height must be above the lowest observation level.
  If `height' > 0. is set, the `fixhei' of the primary is not
  effective (see Sect. 4.11 above) for the propagation but will define the
  starting point of the longitudinal profile unless the STACKIN and SLANT
  options are selected too. In that case, and only in that case,
  `fixhei' also define the starting point of the shower profile in slant depth.

  In the STACKIN option FIXHEI is needed to specify the altitude of the first,
  externally treated interaction.

  In the CURVED option the keyword FIXHEI cannot be used for em-primary particles.
+ `target': Fixes the target of the first interaction:
  + 1 = Nitrogen
  + 2 = Oxygen
  + 3 = Argon
  + else = random selection according to the atmospheric abundances.

  This option is only applicable for high-energy hadronic primaries, i.e.
  primaries with an energy per nucleon of Elab ≥ HILOW (see page 86). Also
  in the NUPRIM option it may be used.
  
  With the STACKIN option one should select `target' = 0 as in this case it
  acts on that secondary particle which is treated first.

  In case of unstable hadronic primaries and predetermined target the first
  interaction will not be a decay.

Limits are: 0. ≤ FIXHEI < border of atmosphere (at 112.8E5 cm for atmospheric
models 1 ≤ MODATM ≤ 9 or MODATM ≥ 17). ")
  (:formats (float height) (integer target))
  (:asserts (<= 0.0 height 112.8e5)))

;; 4.14 Skimming Incidence

(def-input (impact "IMPACT")
    (&optional (low 0.0) (high low))
  (:documentation "Skimming Incidence.
+ `low' : Lower value (in cm) for minimum altitude of horizontal shower axis;
+ `high': Upper value (in cm) for minimum altitude of horizontal shower axis.

The actual minimum altitude is selected at random out of this interval with
uniform distribution. Zenith angles given by the keyword THETAP (page 72) are
overridden by a calculation from the actual minimum altitude. See UPWARD
option page 65.

Limits are: OBSLEV(1) ≤ HIMPACT(i) ≤ min( FIXHEI, FIXCHI, border of atmosphere
[at 112.8E5 cm for atmospheric models 1 ≤ MODATM ≤ 9 or MODATM ≥ 17]).

This keyword is only available in the combination of the CURVED option with the
UPWARD option.")
  (:formats (float low high))
  (:asserts t))

;; 4.15 Stack Input File Name

(def-input (infile "INFILE") (file-name)
  (:documentation "Stack Input File Name.
+ `file-name': File name to define the name and directory of the input file
  containing the parameters of secondary particles (see Sect. 3.5.34 page 61).

  Lower case characters of FILINP are not converted to capitals. Please keep in
  mind that in FORTRAN an automatic expansion of UNIX names like ’$HOME’ is
  not possible, rather you should give the fully expanded name of the directory
  and file.

Limit is: FILINP must not begin with a ∼ (tilde) character.

This keyword is only available in the STACKIN option.")
  (:formats (string file-name))
  (:asserts (<= (length file-name) 132)))

;; 4.16 Stack Output File Name

(def-input (outfile "OUTFILE") (file-name)
  (:documentation "Stack Output File Name.
+ `file-name': File name to define the name and directory of the output file
  which will contain the parameters of the secondary particles produced in
  the first interaction. These particles are written only if the file name
  is defined. If more than one shower is simulated in one run, only the
  secondary particles of the first interaction coming from the first shower
  are written, those from the second and further showers are skipped.

These particles may be read in in a later CORSIKA-run established with the
STACKIN option (see Sect. 3.5.34 page 61). Lower case characters of FILINP
are not converted to capitals.

Please keep in mind that in FORTRAN an automatic expansion of UNIX names
like ’$HOME’ is not possible, rather you should give the fully expanded
name of the directory and file.

Limit is: FILOUT must not begin with a ∼ (tilde) character.

This keyword is not available in the CONEX or STACKIN options.")
  (:formats (string file-name))
  (:asserts (<= (length file-name) 132)))

;; 4.17 Atmospheric Model Selection

(def-input (atmod "ATMOD") (&optional (model-id 1))
  (:documentation "Atmosphere Model Selection. 
+ `model-id': Gives the number of the atmospheric parameterization.

MODATM = 0: Atmosphere as read in by keywords ATMA, ATMB, ATMC, and ATMLAY
(uppermost layer unchanged).
MODATM = 1: U.S. standard atmosphere as parameterized by Linsley.
MODATM = 2: AT115 Central European atmosphere for Jan. 15, 1993.
MODATM = 3: AT223 Central European atmosphere for Feb. 23, 1993.
MODATM = 4: AT511 Central European atmosphere for May 11, 1993.
MODATM = 5: AT616 Central European atmosphere for June 16, 1993.
MODATM = 6: AT822 Central European atmosphere for Aug. 22, 1993.
MODATM = 7: AT1014 Central European atmosphere for Oct. 14, 1993.
MODATM = 8: AT1224 Central European atmosphere for Dec. 24, 1993.
MODATM = 9: Atmosphere as read in by keywords ATMA, ATMB, ATMC.
(Layers as in MODATM = 1 . . . 8.)
MODATM = 10: Atmosphere as read in by keywords ATMA, ATMB, ATMC, and ATMLAY
(uppermost layer also read in).
MODATM = 11: South pole atmosphere for March 31, 1997 (MSIS-90-E).
MODATM = 12: South pole atmosphere for July 01, 1997 (MSIS-90-E).
MODATM = 13: South pole atmosphere for Oct. 01, 1997 (MSIS-90-E).
MODATM = 14: South pole atmosphere for Dec. 31, 1997 (MSIS-90-E).
MODATM = 15: South pole atmosphere for January after Lipari.
MODATM = 16: South pole atmosphere for August after Lipari.
MODATM = 17: U.S. standard atmosphere as parameterized by Keilhauer.
MODATM = 18: Malarg¨ue GDAS model for January after Will/Keilhauer.
MODATM = 19: Malarg¨ue GDAS model for February after Will/Keilhauer.
MODATM = 20: Malarg¨ue GDAS model for March after Will/Keilhauer.
MODATM = 21: Malarg¨ue GDAS model for April after Will/Keilhauer.
MODATM = 22: Malarg¨ue GDAS model for May after Will/Keilhauer.
MODATM = 23: Malarg¨ue GDAS model for June after Will/Keilhauer.
MODATM = 24: Malarg¨ue GDAS model for July after Will/Keilhauer.
MODATM = 25: Malarg¨ue GDAS model for August after Will/Keilhauer.
MODATM = 26: Malarg¨ue GDAS model for September after Will/Keilhauer.
MODATM = 27: Malarg¨ue GDAS model for October after Will/Keilhauer.
MODATM = 28: Malarg¨ue GDAS model for November after Will/Keilhauer.
MODATM = 29: Malarg¨ue GDAS model for December after Will/Keilhauer.
MODATM = 30: South Pole for IceTop for January after S. de Ridder.
MODATM = 31: South Pole for IceTop for February after S. de Ridder.
MODATM = 32: South Pole for IceTop for March after S. de Ridder.
MODATM = 33: South Pole for IceTop for April after S. de Ridder.
MODATM = 34: South Pole for IceTop for May after S. de Ridder.
MODATM = 35: South Pole for IceTop for June after S. de Ridder.
MODATM = 36: South Pole for IceTop for July after S. de Ridder.
MODATM = 37: South Pole for IceTop for August after S. de Ridder.
MODATM = 38: South Pole for IceTop for September after S. de Ridder.
MODATM = 39: South Pole for IceTop for October after S. de Ridder.
MODATM = 40: South Pole for IceTop for November after S. de Ridder.
MODATM = 41: South Pole for IceTop for December after S. de Ridder.
The various atmospheric models are described in Appendix F (page 166 ff.).
")
  (:formats (integer model-id))
  (:asserts (<= 0 model-id 41)))

;; 4.18 Atmospheric Parameters A(i)

(def-input (atma "ATMA")
    (&optional (AATM1 0.0) (AATM2 0.0) (AATM3 0.0) (AATM4 0.0) (AATM5 0.0))
  (:documentation "Atmospheric Parameters A(i)

Format = (A4, 4F), Defaults = 0., 0., 0., 0. (for ATMOD 0)
Format = (A4, 5F), Defaults = 0., 0., 0., 0., 0. (for ATMOD 10)

AATMi : A-parameters for 4 layers of atmospheric model # 0 (or for 5 layers
of atmospheric model # 10). For the 5th layer a linear decrease is assumed,
which in case of ATMOD = 0 uses the same parameters as the U.S. standard
atmosphere. To be used with ATMOD = 0 or 10.

Limit is: 0. < AATM5")
  (:formats (float aatm1 aatm2 aatm3 aatm4 aatm5))
  (:asserts (>= aatm5 0.0)))

;; 4.19 Atmospheric Parameters B(i)

(def-input (atmb "ATMB") (BATM1 BATM2 BATM3 BATM4)
  (:documentation "Atmospheric Parameters B(i). 
Format = (A4, 4F), Defaults = 0., 0., 0., 0.
BATMi : B-parameters for 4 layers of atmospheric model # 0. For the 5th layer
a linear decrease is assumed with the same parameters as for the U.S. standard
atmosphere. To be used with ATMOD = 0 or 10.

Limits are: BATMi != 0.")
  (:formats (float batm1 batm2 batm3 batm4))
  (:asserts (every (lambda (batmi) (not (zerop batmi)))
                   (list BATM1 BATM2 BATM3 BATM4))))

;; 4.20 Atmospheric Parameters C(i)

(def-input (atmc "ATMC") (CATM1 CATM2 CATM3 CATM4 CATM5)
  (:documentation "4.20 Atmospheric Parameters C(i)

Format = (A4, 4F), Defaults = 0., 0., 0., 0. (for ATMOD 0)
Format = (A4, 5F), Defaults = 0., 0., 0., 0., 0. (for ATMOD 10)

CATMi : C-parameters for 4 layers of atmospheric model # 0 (or for 5 layers of
atmospheric model # 10). For the 5th layer a linear decrease is assumed, which
in case of ATMOD = 0 uses the same parameters as the U.S. standard atmosphere.
To be used with ATMOD = 0 or 10.

Limits are: CATMi > 0.")
  (:formats (float catm1 catm2 catm3 catm4 catm5))
  (:asserts (and (> catm1 0) (> catm2 0) (> catm3 0)
                 (> catm4 0) (> catm5 0))))

;; 4.21 Atmospheric Layer Boundaries

(def-input (atmlay "ATMLAY")
    (HLAY2 HLAY3 HLAY4 HLAY5)
  (:documentation "Atmospheric Layer Boundaries. 
Format = (A6, 4F), Defaults = 4.D5, 10.D5, 40.D5, 100.D5
HLAYi : Layer lower boundaries (in cm) for the layers of atmospheric model # 0
and # 10. A value of 0. is adopted for the HLAY1. If not specified, the default
values of MODATM = 1 are used for MODATM = 0 and 10. For other models (MODATM != 0
and != 10), the default values correspond with the selected model MODATM. Should
only be used with ATMOD = 0 or 10. Limits are: 0. < HLAYi.")
  (:formats (float HLAY2 HLAY3 HLAY4 HLAY5))
  (:asserts (every (lambda (hlayi) (> hlayi))
                   (list HLAY2 HLAY3 HLAY4 HLAY5))))

;; 4.22 External Tabulated Atmosphere

(def-input (atmosphere "ATMOSPHERE") (IATMOX FREFRX)
  (:documentation "External Tabulated Atmosphere. 
Format = (A10, I, L), Defaults = 0, F

IATMOX : Use MODTRAN [42] atmospheric model IATMOX = i (in terms of density and
refractive index) instead of CORSIKA built-in model. This requires a file named
atmprofi.dat.

MODTRAN model atmospheres supplied with the ’bernlohr’ package include tropical
(i = 1), mid-latitude summer (2), mid-latitude winter (3), sub-arctic summer (4),
sub-arctic winter (5), and U.S. standard atmosphere 1976 (6). Additionally for
the MAGIC Cherenkov telescope on La Palma the summer (7) and winter (8)
atmospheres[69] are supplied. The atmosphere (9) gives the winter atmosphere at
the South pole. User supplied models are possible (i> 9).

FREFRX : If .true., the atmospheric refraction for Cherenkov photons is taken
into account (for plane-parallel atmosphere); if .false., refraction is ignored.
The value of this second argument is ignored if the CERENKOV option is not
selected.

This keyword is only available in the ATMEXT option and needs linking with the
(compiled) atmo.c routines of the ’bernlohr’ package.")
  (:formats (integer IATMOX) (boolean FREFRX))
  (:asserts t))

;; 4.23 Atmospheric profile file

(def-input (atmfile "ATMFILE") (FILEATM)
  (:documentation "Format = (A7, A1024), Defaults = ' '

FILEATM: File name to define the name and directory where to read ATMLAY, ATMA,
ATMB and ATMC parameters, as well as a tabulated altitude profile of the
atmospheric refractive index.

The file FILEATM needs to be produced with the gdastool script included with
CORSIKA (in src/utils/), or to have the same format. See Appendix G page 178
for how to use gdastool. This allows to use atmospheric profiles for density
and refractive index as compiled from the GDAS (Global Data Assimilation
System) database. This database contains atmospheric parameters as used in
e.g. weather forecasting models. It features atmospheric parameters at 24
altitudes in the atmosphere, at a 1 x 1 degree grid on Earth, at a 3-hour time
resolution.

Limit is: FILEATM must not begin with a ∼ (tilde) character and should contain
the full path of the file if it is not located where CORSIKA is run (DATDIR
not used).

This keyword cannot be used together with a manual specification of ATMLAY,
ATMA, ATMB and/or ATMC.")
  (:formats (string fileatm))
  (:asserts (<= (length fileatm) 1024)))

;; 4.24 Earth’s Magnetic Field

(def-input (magnet "MAGNET") (&optional (BX 20.40) (BZ 43.23))
  (:documentation "Earth’s Magnetic Field
Format = (A6, 2F), Defaults = 20.40, 43.23

BX : Is the horizontal component of the Earth’s magnetic field (in µT) to the
x-direction of the coordinate system (North) (see Fig. 1 page 120)

BZ : Is the vertical component of the Earth’s magnetic field (in µT) downwards.
The default values represent the magnetic field for the Karlsruhe location.
The values of other locations may be obtained from the program Geomag which is
available on-line in the world wide web [70]. The value H of Geomag corresponds
with our BX, the value Z with our BZ.

For the orientation of the CORSIKA coordinate system see also Fig. 1 page 120.
Limits are: BX, BZ != 0. .")
  (:formats (float BX BZ))
  (:asserts (and (not (zerop BX))
                 (not (zerop BZ)))))

;; 4.25 Experiment Coordinates for Pre-Showering and CONEX

(def-input (gcoord "GCOORD")
    (&optional (GLONG -69.585) (GLATI -35.463) (GRFYEAR 2003.0)
               (IPREPR 1) (IPRSTP 0))
  (:documentation "Experiment Coordinates for Pre-Showering and CONEX
Format = (A6, 3F, 2I), Defaults = -69.585, -35.463, 2003., 1, 0

GLONG : Gives the geographical longitude (in ◦, West length is negative) of
the experiment.

GLATI : Gives the geographical latitude (in ◦, South latitude is negative) of
the experiment.

GRFYEAR : Gives the year of the experiment (the magnetic field is varying with
time). These coordinates are used to calculate the magnetic dipole field of
the Earth’s globe above the atmosphere of the experiment’s position in the case
of a pre-shower induced by ultra-high energetic primary photons. The default
values give the position coordinates of the southern Pierre Auger Observatory
at Malarg¨ue (Argentina) for the year 2003.

IPREPR : Print indicator72: IPREPR ≤ 0 disables pre-shower printing;
IPREPR = 1 prints details of pre-shower in case of MAXPRT (page 101) or
DEBUG (page 113); IPREPR ≥ 2 always prints details of pre-shower.

IPRSTP : If IPRSTP != 0 events without pre-showering are skipped.

Limits are: -180. ≤ GLONG ≤ 180.; -90. ≤ GLATI ≤ 90.; 1965. ≤ GRFYEAR ≤ 2015.

This keyword is only available in the CONEX or PRESHOWER options.")
  (:formats (float GLONG GLATI GRFYEAR)
            (integer IPREPR IPRSTP))
  (:asserts (and (<= -180.0 GLONG 180.0)
                 (<= -90.0  GLATI 90.0)
                 (<= 1965.0 GRFYEAR 2015.0))))

;; 4.26 DPMJET Selection Flag

(def-input (dpmjet "DPMJET") (FDPMJT LEVLDB)
  (:documentation "DPMJET Selection Flag. 

Format = (A6, L, I), Defaults = T, 0

FDPMJT : If .true., the DPMJET routines are used to treat the high-energy
hadronic interactions. If .false., the HDPM routines are used to treat the
high-energy hadronic interactions.

LEVLDB : Gives amount of debug output for the DPMJET code in case of DEBUG.
With increasing value up to 8 the debug output becomes more and more detailed.
This output cannot be redirected and always appears on unit 6.

Limits are: 0 ≤ LEVLDB ≤ 8 .
This keyword is only available in the DPMJET option.")
  (:formats (boolean FDPMJT) (integer LEVLDB))
  (:asserts (<= 0 LEVLDB 8)))

;; 4.27 DPJSIG Selection Flag

(def-input (dpjsig "DPJSIG") (FDPJSG)
  (:documentation "DPJSIG Selection Flag

Format = (A6, L), Default = T

FDPJSG : If .true, the DPMJET high-energy hadronic cross-sections are used.
If .false., the default cross-sections as described in Ref. [3] are used.

This keyword is only available in the DPMJET option.")
  (:formats (boolean FDPJSG))
  (:asserts t))

;; 4.28 EPOS Selection Flag

(def-input (epos "EPOS") (FNEXUS ISH0N)
  (:documentation "EPOS Selection Flag.

Format = (A5, L, I), Defaults = T, 0

FNEXUS : If .true., the EPOS routines are used to treat the high-energy
hadronic interactions. If .false., the HDPM routines are used to treat
the high-energy hadronic interactions.

ISH0N : Determines amount of debug output for the EPOS routines. With
increasing number ISH0N > 0 the output becomes more and more detailed.
This output appears on the unit MDEBUG.

For more information look into the EPOS documentation. Additional debugging
is effective by setting print parameters using EPOPAR print .... This debug
output is written to the ifch file (see Table 1 page 25).

Limits are: 0 ≤ ISH0N ≤ 9.

This keyword is only available in the EPOS option.")
  (:formats (boolean FNEXUS) (integer ISH0N))
  (:asserts (<= 0 ISH0N 9)))

;; 4.29 EPOS Parameters

(def-input (epopar "EPOPAR") (&optional (parcha " "))
  (:documentation "EPOS Parameters.

Format = (A6, A74), Defaults = ’ ’

parcha : Command line to be read by subroutine aread of program block
epos-bas-lhc.f. The possible command lines are described in the EPOS
documentation. Use lower case characters. Lower case characters of parcha
are not converted to capitals. Do not use the commands application ...,
set nevent ..., run, or stop within your input parameters, these will
cause unpredictable results or crashes. Only epos.inixx names might be
changed by standard users (note that .lhc is automatically added at the
end of relevant file names).

This keyword is only available in the EPOS option.

A typical EPOPAR input looks like:
EPOPAR input ˜corsika-77500/epos/epos.param
EPOPAR fname inics ˜corsika-77500/epos/epos.inics
EPOPAR fname iniev ˜corsika-77500/epos/epos.iniev
EPOPAR fname inihy ˜corsika-77500/epos/epos.ini1b
EPOPAR fname inirj ˜corsika-77500/epos/epos.inirj
EPOPAR fname initl ˜corsika-77500/epos/epos.initl
EPOPAR fname check ˜corsika-77500/epos/epos.check
EPOPAR fname histo ˜corsika-77500/epos/epos.histo ! for interaction test only
EPOPAR fname data ˜corsika-77500/epos/epos.data ! for debugging only
EPOPAR fname copy ˜corsika-77500/epos/epos.copy ! for debugging only
EPOPAR fname log ˜corsika-77500/epos/epos.log ! for debugging only
EPOPAR printcheck screen ! for debugging only.
")
  (:formats (string parcha))
  (:asserts t))

;; 4.30 EPOSIG Selection Flag

(def-input (eposig "EPOSIG") (FNEXSG)
  (:documentation "EPOSIG Selection FlagFormat = (A6, L), Default = T
FNEXSG : If .true., the EPOS high-energy hadronic cross-sections are used.
If .false., the default cross-sections as described in Ref. [3] are used.
This keyword is only available in the EPOS option.")
  (:formats (boolean FNEXSG))
  (:asserts t))

;; 4.31 NEXUS Selection Flag

(def-input (nexus "NEXUS") (FNEXUS ISH0N)
  (:documentation "NEXUS Selection Flag.

Format = (A5, L, I), Defaults = T, 0

FNEXUS : If .true., the NEXUS routines are used to treat the high-energy
hadronic interactions. If .false., the HDPM routines are used to treat
the high-energy hadronic interactions.

ISH0N : Determines amount of debug output for the NEXUS routines. With
increasing number ISH0N > 0 the output becomes more and more detailed.
This output appears on the unit MDEBUG.

For more information look into the NEXUS documentation. Additional
debugging is effective by setting print parameters using NEXPAR print
.... This debug output is written to the ifch file (see Table 1 page 25).

Limits are: 0 ≤ ISH0N ≤ 9 .

This keyword is only available in the NEXUS option.")
  (:formats (boolean FNEXUS) (integer ISH0N))
  (:asserts (<= 0 ISH0N 9)))

;; 4.32 NEXUS Parameters

(def-input (nexus-param "NEXPAR") (parcha)
  (:documentation "NEXUS Parameters.

Format = (A6, A74), Defaults = ’ ’

parcha : Command line to be read by subroutine aread of program block
nexus-bas.f. The possible command lines are described in the NEXUS
documentation. Use lower case characters. Lower case characters of
parcha are not converted to capitals. Do not use the commands application
..., set nevent ..., run, or stop within your input parameters, these
will cause unpredictable results or crashes. Only nexus.inixx names might
be changed by standard users.

This keyword is only available in the NEXUS option.

A typical NEXPAR input looks like:
NEXPAR fname inics ˜corsika-77500/nexus/nexus.inics
NEXPAR fname iniev ˜corsika-77500/nexus/nexus.iniev
NEXPAR fname inirj ˜corsika-77500/nexus/nexus.inirj
NEXPAR fname initl ˜corsika-77500/nexus/nexus.initl
NEXPAR fname check ˜corsika-77500/nexus/nexus.check
NEXPAR fname histo ˜corsika-77500/nexus/nexus.histo ! for interaction test only
NEXPAR fname data ˜corsika-77500/nexus/nexus.data ! for debugging only
NEXPAR fname copy ˜corsika-77500/nexus/nexus.copy ! for debugging only
NEXPAR fname log ˜corsika-77500/nexus/nexus.log ! for debugging only
NEXPAR printcheck screen ! for debugging only.")
  (:formats (string parcha))
  (:asserts t))

;; 4.33 NEXSIG Selection Flag

(def-input (nexsig "NEXSIG") (FNEXSG)
  (:documentation "NEXSIG Selection Flag.

Format = (A6, L), Default = T

FNEXSG : If .true., the NEXUS high-energy hadronic cross-sections are used.
If .false., the default cross-sections as described in Ref. [3] are used.

This keyword is only available in the NEXUS option.")
  (:formats (string FNEXSG))
  (:asserts t))

;; 4.34 QGSJET Selection Flag

(def-input (qgsjet "QGSJET") (FQGS LEVLDQ)
  (:documentation "QGSJET Selection Flag.

Format = (A6, L, I), Defaults = T, 0

FQGS : If .true., the qgsjet-II-04 (rsp. QGSJET01d) routines are used to
treat the high-energy hadronic interactions. If .false., the HDPM routines
are used to treat the high-energy hadronic interactions.

LEVLDQ : Gives amount of debug output for the qgsjet-II-04 (rsp. QGSJET01d)
code in case of DEBUG. With increasing value up to 4 the debug output
becomes more and more detailed.

This output cannot be redirected and always appears on unit 6.

Limits are: 0 ≤ LEVLDQ ≤ 4 .

This keyword is only available in the QGSJET option.")
  (:formats (boolean FQGS) (integer LEVLDQ))
  (:asserts (<= 0 LEVLDQ 4)))

;; 4.35 QGSSIG Selection Flag

(def-input (qgssig "QGSSIG") (FQGSSG)
  (:documentation "QGSSIG Selection Flag. 

Format = (A6, L), Default = T

FQGSSG : If .true., the qgsjet-II-04 (rsp. QGSJET01d) high-energy
hadronic cross-sections are used. If .false., the default
cross-sections as described in Ref. [3] are used.

This keyword is only available in the QGSJET option.")
  (:formats (boolean FQGSSG))
  (:asserts t))

;; 4.36 SIBYLL Selection Flag

(def-input (sibyll "SIBYLL") (FSIBYL ISDEBUG)
  (:documentation "SIBYLL Selection Flag.

Format = (A6, L), Default = T, 0

FSIBYL : If .true., the SIBYLL 2.3d routines are used to treat the
high-energy hadronic interactions. If .false., the HDPM routines are
used to treat the high-energy hadronic interactions.

ISDEBUG : Debug level; with increasing level the SIBYLL debug output
becomes more and more detailed. This output cannot be redirected and
always appears on unit 6. This debugging becomes only actively, if
CORSIKA debugging is activated by the DEBUG flag (Sect. 4.114 page 113).

This keyword is only available in the SIBYLL option.")
  (:formats (boolean FSIBYL) (integer ISDEBUG))
  (:asserts t))

;; 4.37 SIBSIG Selection Flag

(def-input (sibsig "SIBSIG") (FSIBSG)
  (:documentation "SIBSIG Selection Flag.

Format = (A6, L), Default = T

FSIBSG : If .true., the SIBYLL high-energy hadronic cross-sections
are used. If .false., the default cross-sections as described in
Ref. [3] are used.

This keyword is only available in the SIBYLL option.")
  (:formats (boolean FSIBSG))
  (:asserts t))

;; 4.38 SIBCHM Selection Flag

(def-input (sibchm "SIBCHM") (FSIBCH)
  (:documentation "SIBCHM Selection Flag.

Format = (A6, L), Default = T

FSIBCH : If .true., charm production is active in SIBYLL (default
tune in the model). It may be propagated or not in CORSIKA using
the CHARM (page 46) option. If .false., no charm particles are
produced by SIBYLL.

This keyword is only available in the SIBYLL option.")
  (:formats (boolean FSIBCH))
  (:asserts t))

;; 4.39 VENUS Selection Flag

(def-input (venus "VENUS") (FVENUS ISH00)
  (:documentation "VENUS Selection Flag.

Format = (A5, L, I), Defaults = T, 0

FVENUS : If .true., the VENUS routines are used to treat the
high-energy hadronic interactions. If .false., the HDPM routines
are used to treat the high-energy hadronic interactions.

ISH00 : Determines the amount of debug output for VENUS routines.
With increasing number ISH00 ≥ 90 the output becomes more and more
detailed. This output appears on the unit MDEBUG. For more
information look into the listing of subroutine venini.

Limits are: 0 ≤ ISH00 ≤ 98 .

This keyword is only available in the VENUS option.")
  (:formats (boolean FVENUS) (integer ISH00))
  (:asserts (<= 0 ISH00 98)))

;; 4.40 VENUS Parameters

(def-input (venpar "VENPAR") (PARCHA PARVAL)
  (:documentation "VENUS Parameters.

Format = (A6, A6, F), Defaults = ’ ’, 0.

PARCHA(i) : Name of VENUS parameter to be changed.
PARVAL(i) : New value of VENUS parameter to be changed.

A maximum of i = 100 VENUS parameters may be set by the user in
arbitrary sequence. The available names and their meaning may be
taken from the listing of subroutine venini.

The VENUS parameters should not be changed by standard users.

This keyword is only available in the VENUS option.")
  (:formats (string parcha) (float parval))
  (:asserts t))

;; 4.41 VENSIG Selection Flag

(def-input (vensig "VENSIG") (FVENSG)
  (:documentation "VENSIG Selection Flag.

Format = (A6, L), Default = T

FVENSG : If .true., the VENUS high-energy hadronic cross-sections
are used. If .false., the default cross-sections as described in
Ref. [3] are used.

This keyword is only available in the VENUS option.")
  (:formats (boolean FVENSG))
  (:asserts t))

;; 4.42 HDPM Interaction Parameters & Fragmentation

(def-input (hadflg "HADFLG")
    (NFLAIN NFLDIF NFLPI0 NFLPIF NFLCHE NFRAGM)
  (:documentation "HDPM Interaction Parameters & Fragmentation.

Format = (A6, 6I), Defaults = 0, 0, 0, 0, 0, 2

Steering flags of the high-energy hadronic interaction model HDPM and
of the projectile nucleus fragmentation of all hadronic interaction models.

NFLAIN : The number of interactions of a projectile in a target nucleus
may fluctuate (NFLAIN = 0) or is calculated as an average value (NFLAIN != 0).

NFLDIF : No diffractive interactions are allowed in case of more than 1
interaction in the target (NFLDIF = 0) or diffractive interactions are
possible (NFLDIF != 0).

NFLPI0 : The rapidity distribution of π◦ is taken different from that of
charged pions as indicated by collider data (NFLPI0 = 0) or is taken as for
charged pions (NFLPI0 != 0).

NFLPIF : The number of π◦ fluctuates in the same way as the number of charged
pions (NFLPIF = 0) or fluctuates independently as parameterized from collider
data (NFLPIF != 0).

NFLCHE : Charge exchange reactions for the proj. and target particles are
allowed (NFLCHE = 0) or inhibited (NFLCHE != 0).

NFRAGM : A primary nucleus fragments at the first interaction completely into
free nucleons (NFRAGM = 0) or successively by assuming that the non-interacting
nucleons proceed as one new nucleus (NFRAGM = 1). This new nucleus may
evaporate nucleons or alpha-particles with a transverse momentum distribution
according to experimental data [71] (NFRAGM = 2, default) or with a
transverse momentum distribution according to Goldhaber’s theory [72] using
0.090 GeV/nucleon as the average transverse momentum (NFRAGM = 3). NFRAGM = 4
gives identical fragments as NFRAGM = 2 or 3, but without transverse momenta.

The NFRAGM flag is used also to steer the fragmentation in the various
interaction models as described for the HDPM routines. EPOS, NEXUS, and VENUS
use the same evaporation model as HDPM with the same meaning of NFRAGM, while
SIBYLL and QGSJET deliver themselves realistic nuclear fragments with
according transverse momenta; they are selected by NFRAGM ≥ 2. Therefore the
nuclear evaporation as used for HDPM, EPOS, NEXUS, and VENUS is coupled with
DPMJET and the meaning of NFRAGM follows HDPM.

Limits are: 0 ≤ all flags < 100")
  (:formats (integer NFLAIN NFLDIF NFLPI0 NFLPIF NFLCHE NFRAGM))
  (:asserts (every (lambda (flag) (<= 0 flag (1- 100)))
                   (list NFLAIN NFLDIF NFLPI0 NFLPIF NFLCHE NFRAGM))))

;; 4.43 Neutrino Interaction Type Selection

(def-input (nuslct "NUSLCT") (NUSLCT)
  (:documentation "Neutrino Interaction Type Selection.

Format = (A6, I), Default = 2

NUSLCT : Selects the type of the primary neutrino interaction:
+ 0 = neutral current interaction,
+ 1 = charged current interaction,
+ else = type of interaction is selected at random according to the
  interaction cross sections for the two processes.

This keyword is only available in the NUPRIM option.")
  (:formats (integer NUSLCT))
  (:asserts t))

;; 4.44 Charm Interaction Cross Section

(def-input (sigmaq "SIGMAQ") (SIGMAQ1 SIGMAQ2 SIGMAQ3 SIGMAQ4)
  (:documentation "Charm Interaction Cross Section.

Format = (A6, F4), Defaults = 0., 0., 0., 0.

SIGMAQ(i) : The interaction cross-sections for charmed mesons (i = 1),
charmed baryons (i = 2), bottom mesons (i = 3), and bottom baryons
(i = 4) are specified. For SIGMAQ(i) = 0., the parameterizations of
Ref. [45] are used; for SIGMAQ(i) ̸= 0., the energy independent
cross-section of SIGMAQ(i) is used for the corresponding projectile.

Limits are: 0. ≤ SIGMAQ(i).

This keyword is only available in the CHARM option.")
  (:formats (float SIGMAQ1 SIGMAQ2 SIGMAQ3 SIGMAQ4))
  (:asserts (every (lambda (sigmaqi) (<= 0 sigmaqi))
                   (list SIGMAQ1 SIGMAQ2 SIGMAQ3 SIGMAQ4))))

;; 4.45 Charm Interaction Selection

(def-input (propaq "PROPAQ") (PROPMOD)
  (:documentation "Charm Interaction Selection.

Format = (A6, I), Default = 1

PROPMOD : If set to 1, the PYTHIA extension [45] is used to treat the
interactions of charmed projectiles. If set to 0, the QGSJET01d
interaction model is used to treat the interactions of charmed
projectiles.

Limits are: 0 ≤ PROPMOD ≤ 1.

This keyword is only available in the CHARM option in combination with
the QGSJET option.")
  (:formats (integer PROPMOD))
  (:asserts (<= 0 PROPMOD 1)))

;; 4.46 Transition Energy between Models

(def-input (hilow "HILOW") (HILOELB)
  (:documentation "Transition Energy between Models.

Format = (A5, F), Default = 80.

HILOELB : Allows to define the transition energy75 (Elab in GeV)
between high and low-energy hadronic interaction model.

Limits depend on the used interaction model, for most high-energy
hadronic interaction models the low-energy limit is in the range
of ≈ 80 GeV, for SIBYLL ≈ 60 GeV, while most low energy models enable
a limit as high as several 100 GeV. With the INTTEST option the default
value of this border is at 49 GeV rsp. 101 GeV, depending whether a
high- or low-energy hadronic interaction model should be tested. For
testing of DPMJET, EPOS, NEXUS, QGSJET, and VENUS the default value is
set to 49 GeV, for SIBYLL to 60 GeV. If none of those models is selected,
the default value is set to 101 GeV to test the models FLUKA, GHEISHA,
or UrQMD.")
  (:formats (float HILOELB))
  (:asserts t))

;; 4.47 Electromagnetic Interaction Steering Flags

(def-input (elmflg "ELMFLG") (FNKG FEGS)
  (:documentation "Electromagnetic Interaction Steering Flags.

Format = (A6, 2L), Defaults = T, T

FNKG : If .true., the NKG option is switched on for calculating the
electromagnetic subcascades analytically. For the electron kinetic
energy threshold the value of ELCUT(3) is taken (keyword ECUTS page 94).
If .false., the NKG option is disabled76.

FEGS : If .true., the EGS4 option is selected to calculate all
interactions of e+, e−, and photons in the atmosphere explicitly.
(The second random number sequence should be initialized for use in
the EGS4 part. Otherwise the default initialization is taken.) If .false.,
the EGS4 option is disabled.

In the CERENKOV and PRESHOWER options this flag is obsolete as EGS4 is
selected automatically.

The two options may be selected or disabled independently at the same time.")
  (:formats (boolean FNKG FEGS))
  (:asserts t))

;; 4.48 Electron Multiple Scattering Length Factor

(def-input (stepfc "STEPFC") (STEPFC)
  (:documentation "Electron Multiple Scattering Length Factor.

Format = (A6, F), Default = 1.

STEPFC : Factor by which the multiple scattering length for electrons
and positrons in EGS4 simulations is elongated relative to the value
given in [17]. A detailed discussion on the use of the step length is
given in [35]. An enlargement of this factor may be tolerated to reduce
computing time, but simultaneously the electron lateral distribution on
ground becomes slightly narrower. With STEPFC = 10. the CPU-time is
reduced by a factor of ≈ 1.7 (relative to the default value). A reduction
of STEPFC will increase the computing time considerably, e.g with
STEPFC = 0.1 by a factor of ≈ 5.

Limits are: 0. < STEPFC ≤ 10.0

This keyword is not available in the CERENKOV option.")
  (:formats (float STEPFC))
  (:asserts (< 0.0 STEPFC 10.0)))

;; 4.49 Radius of NKG Lateral Range

(def-input (radnkg "RADNKG") (RADNKG)
  (:documentation "Radius of NKG Lateral Range.

Format = (A6, F), Default = 200.E2

RADNKG : Gives the outer range radius (in cm) within which the lateral
NKG distribution is calculated for 10 radii equidistant in logarithmic
scale. The inner radius is always kept at 100 cm. In the CURVED option
the NKG formulas are no longer valid, therefore the NKG flag is disabled
automatically in this option. The NKG flag should be disabled in the
COMPACT option, as the resulting NKG parameters cannot be written out
onto the particle output file.

Limit is: RADNKG > 100.")
  (:formats (float RADNKG))
  (:asserts (> RADNKG 100.0)))

;; 4.50 Flags for Simplified CONEX Threshold Management

(def-input (cascade "CASCADE") (FCXCAS FCXLCE FCXGHE)
  (:documentation "Flags for Simplified CONEX Threshold Management.

Format = (A7, 3L), Defaults = T, F, F

FCXCAS : If .true. optimized parameters are used to run in hybrid mode
(MC+CE+MC) with full 3D information. Muons are fully tracked in MC.
If .false. the cascade equations are disabled below the high-energy
thresholds and only MC is used.

FCXLCE : If .true. default parameters are used to run in hybrid mode
(MC+CE) with full 3D information for muons only. For hadrons and e/m
particles, only 1D informations are relevant (longitudinal profile and
energy distribution of particles at ground). Muons are fully tracked
in 3D in MC. If .false. optimized sampling weights are used to run MC
below the default low-energy thresholds. Weights defined by THIN keyword
are used for thinning in CONEX and sampling weights are used as thinning
weights in CORSIKA (see FCXCE in Sect. 4.53 page 89).

FCXGHE : If .true. default parameters are used to run in hybrid mode
(MC+CE) for all particles. Only 1D informations are relevant (longitudinal
profile and energy distribution of particles at ground). If .false.
optimized sampling weights are used to run MC for muons below the default
low-energy thresholds. Weights defined by THIN keyword are used for
thinning in CONEX and sampling weights are used as thinning weights in
CORSIKA (see FCXCE in Sect. 4.53 page 89).

Limits are: Only the combinations of the three flags T T T, T T F, T F F,
or F F F are allowed.

Notes: This keyword should be used if the user wants to select easily
different hybrid mode. In particulars the first MC part of the shower
is not changed by changing the value of CASCADE (or even by introducing
this command), thus a shower with the same longitudinal profile can
be calculated with different speed and precision (1D or 3D) if the 7th
SEED sequence is not changed of course. The sampling weights are adjusted
to get a good compromise between precision and speed. If ECUTS are defined
below CONEX limits (1 GeV for hadrons and muons and 1 MeV for e-m particles),
low energy MC (T F F but with larger weight and lower threshold) is
automatically switched on for the relevant part of the shower and to get
reasonable precision in 1D, but it will be necessary slower than default
T T F or T T T mode. To be able to reproduce a given shower, ECUTS should
not be changed while changing CASCADE configuration. To get the fastest
simulation for any ECUTS see the keyword CORSIKA in Sect. 4.54 page 90.

It is recommended not to use this keyword together with the CX2COR or CXWMX
keywords since it redefines some of the parameters.

This keyword must not be called after the energy cuts are defined (see
keyword ECUTS Sect. 4.63 page 94).

This keyword is only available in the CONEX option.")
  (:formats (boolean FCXCAS FCXLCE FCXGHE))
  (:asserts (flet ((mask (a b c)
                     (and (eq a FCXCAS) (eq b FCXLCE) (eq c FCXGHE))))
              (or (mask t t   t)   (mask t   t   nil)
                  (mask t nil nil) (mask nil nil nil)))))

;; 4.51 Thresholds for CONEX Cascade Equations

(def-input (conex "CONEX") (CXTHR1 CXTHR2 CXTHR3)
  (:documentation "Thresholds for CONEX Cascade Equations.

Format = (A5, 3F), Defaults = 1.E-3, 1., 1.E-4

CXTHR(i) : Fractions of primary energy above which the shower particles
are treated individually by Monte Carlo methods. Below these thresholds
cascade equations treat the particles.

These thresholds apply to hadrons (i = 1), muons (i = 2), and
electrons/photons (i = 3).

Limits are: 0. ≤ CXTHR(i) ≤ 1.

This keyword is only available in the CONEX option.")
  (:formats (float CXTHR1 CXTHR2 CXTHR3))
  (:asserts (every (lambda (cxthri) (<= 0.0 cxthri 1.0))
                   (list CXTHR1 CXTHR2 CXTHR3))))

;; 4.52 Thresholds for Transition from CONEX to CORSIKA

(def-input (cx2cor "CX2COR") (CXMCT1 CXMCT2 CXMCT3 CXMCS)
  (:documentation "Thresholds for Transition from CONEX to CORSIKA.

Format = (A6, 4F), Defaults = 3.E2, 1.E20, 1.E1, 4.E2

CXMCT(i) : Thresholds (in GeV) below which the particles are transferred
from CONEX back to the CORSIKA stack. These thresholds apply to hadrons
(i = 1), muons (i = 2), and electrons/photons (i = 3).

CXMCS : Minimal vertical depth (g/cm2) above observation level below
which the transfer of particles from CONEX to CORSIKA treatment is started.

Limits are: 0. ≤ CXMCT(i) ≤ 1.E20; 0. ≤ CXMCS ≤ 1.E20

In the CONEX option this keyword has to be called after the keyword CASCADE
T F F (see Sect. 4.50 page 88) is used to be effective, but it is better not
to use it with CASCADE keyword.

This keyword is only available in the CONEX option.")
  (:formats (float CXMCT1 CXMCT2 CXMCT3 CXMCS))
  (:asserts (and (every (lambda (cxmcti) (<= 0.0 cxmcti 1e20))
                        (list CXMCT1 CXMCT2 CXMCT3))
                 (<= 0.0 CXMCS 1e20))))

;; 4.53 Weight Sampling for CONEX

(def-input (cxwmx "CXWMX") (CXWMT1 CXWMT2 CXWMT3 FCXWMX FCXCE)
  (:documentation "Weight Sampling for CONEX.

Format = (A5, 3F, L), Defaults = -1., -1., -1., F, F

CXWMT(i) : Factors (GeV−1) to be multiplied with the primary energy to get
maximal sampling weights for particles transferred from CONEX to CORSIKA.
These factors apply to hadrons (i = 1), muons (i = 2), and electrons/photons
(i = 3). For negative or zero values of CXWMT(i) the CORSIKA thinning weight
limits (see Sects. 4.55, 4.56, and 4.57) are taken as sampling weights too.

FCXWMX : If .true. forces the usage of CONEX sampling weight limits also in
thinning (in CONEX and CORSIKA) and these weight limits are determined with
the CXWMT(i) factors. If .false. and FCXCE = .false. the CORSIKA weight
limits are used as thinning weights for both CONEX and CORSIKA (see Sect.
4.50) and forces the usage of the CONEX weight limits also in CORSIKA.

FCXCE : If .true. forces the usage of CONEX sampling weight limits also in
thinning for CORSIKA only and these weight limits are determined with the
CXWMT(i) factors. The CORSIKA weight limits are used as thinning weight for
CONEX only. If FCXCAS = .true. (see Sect. 4.50) forces FCXCE to be .true.

Limits are: -1. ≤ CXWMT(i) ≤ 1.

In the CONEX option this keyword has to be called after the keyword CASCADE
(see page 88) is used to be effective, but it is better not to use it with
CASCADE keyword.

This keyword is only available in the CONEX option.")
  (:formats (float CXWMT1 CXWMT2 CXWMT3) (boolean FCXWMX FCXCE))
  (:asserts (every (lambda (cxwmti) (<= -1.0 cxwmti 1.0))
                   (list CXWMT1 CXWMT2 CXWMT3))))

;; 4.54 CONEX without CORSIKA

(def-input (corsika "CORSIKA") (FCORS)
  (:documentation "CONEX without CORSIKA.

Format = (A7, L), Defaults = T

FCORS : If .true. allows CORSIKA to run low energy MC. If .false. all
particles which are supposed to go into the CORSIKA stack are lost and
only CONEX is run with CE going to the minimum energy. This is equivalent
to CASCADE T T T (see Sect. 4.50) but with no influence of ECUTS.

This keyword should be used with care because only the longitudinal energy
deposit profile is valid. Even the Xmax fit based on the number of electrons
could be biased because ECUTS below CONEX limits are not taken into account.
This keyword is useful to get very fast 1D energy deposit profile. The change
of FCORS doesn’t change the first interactions of the shower so with the
same SEED the same Xmax for the energy deposit will be obtained.

This keyword is only available in the CONEX option.")
  (:formats (boolean FCORS))
  (:asserts t))

;; 4.55 Thinning Definition

(def-input (thin "THIN") (EFRCTHN WMAX RMAX)
  (:documentation "Thinning Definition.

Format = (A4, 3F), Defaults = 1.E-4, 1.E30, 0.E0

EFRCTHN : Factor εth which defines the energy fraction of the primary energy
below which the thinning algorithm becomes active. If the fraction is
selected in a manner that this energy is below the lowest energy threshold
of ELCUT(i), i = 1...4 (keyword ECUTS page 94), thinning will not become
active, but the particle output data structure will contain the weight (= 1.)
for each particle.

WMAX : Weight limit for thinning. If the weight of a particle exceeds WMAX,
no further thinning is performed.

RMAX : Maximum radius (in cm) at observation level within which all particles
are subject to inner radius thinning. Particles are selected with probability
(r/rmax)^4. The weight of surviving particles is multiplied by the appropriate
factor (inverse of probability). This thinning (See footnote page 63.)
neither affects the shower development nor the table output nor the histogram
output of the ANAHIST, AUGERHIST, or MUONHIST options, rather only the
particle output file written onto MPATAP (and the Cherenkov output file
written onto MCETAB). For RMAX ≤ 0. no radial thinning is applied.

If the keyword CORECUT (Sect. 4.62 page 93) is used with RCUT > 0. the radial
thinning will not be effective in favour of the radial cutting.

Limits are: ULIMIT·EFRCTHN ≤ 1 · 107 GeV (for ULIMIT see keyword ERANGE
Sect. 4.6 page 71); 0.1 ≤ WMAX ≤ 1 * 10^20 .

This keyword is only available in the THIN option.")
  (:formats (float EFRCTHN WMAX RMAX))
  (:asserts (<= 0.1 WMAX 1e20)))

;; 4.56 Hadronic Thinning Definition

(def-input (thinh "THINH") (THINRAT WEITRAT)
  (:documentation "Hadronic Thinning Definition.

Format = (A5, 2F), Defaults = 1., 1.

THINRAT : Defines hadronic thinning limit differing from em-thinning limit
EFRCTHN by the ratio of εthem/εthhadr which gives the ratio between the energy
of the em-particles (specified by keyword THIN, see above) and the energy of
the hadronic particles below which the thinning algorithm becomes active for
these particle species (see also Sect. 4.55 above).

WEITRAT : Defines hadronic weight limit differing from em-weight limit WMAX by
the ratio of weight limit of em-particles to weight limit of hadronic particles
in case of thinning (see also Sect. 4.55 above).

A simultaneous use of the keyword THINH together with THINEM is not tolerated
and will lead to an error stop.

Limits are: ULIMIT·EFRCTHN/THINRAT ≤ 1·107 GeV (for ULIMIT see keyword ERANGE
Sect. 4.6 page 71); 1 · 10^−4 ≤ WEITRAT ≤ 1 · 10^6 .

This keyword is only available in the THIN option.")
  (:formats (float THINRAT WEITRAT))
  (:asserts (<= 1e-4 WEITRAT 1e6)))

;; 4.57 Electromagnetic Thinning Definition

(def-input (thinem "THINEM") (THINRAT WEITRAT)
  (:documentation "Electromagnetic Thinning Definition.

Format = (A6, 2F), Defaults = 1., 1.

THINRAT : Defines em-thinning limit differing from hadronic thinning limit
EFRCTHN by the ratio of εthem/εthhadr which gives the ratio between the energy
of the em-particles and the energy of the hadronic particles (specified by
keyword THIN, see above) below which the thinning algorithm becomes active for
these particle species (see also Sect. 4.55 above).

WEITRAT : Defines em-weight limit differing from hadronic weight limit WMAX by
the ratio of weight limit of em-particles to weight limit of hadronic particles
in case of thinning (see also Sect. 4.55 above).

A simultaneous use of the keyword THINEM together with THINH is not tolerated
and will lead to an error stop.

Limits are: ULIMIT·EFRCTHN·THINRAT ≤ 1·107 GeV (for ULIMIT see keyword ERANGE
Sect. 4.6 page 71); 1 · 10−4 ≤ WEITRAT ≤ 1 · 106 .

This keyword is only available in the THIN option.")
  (:formats (float THINRAT WEITRAT))
  (:asserts (<= 1e-4 WEITRAT 1e6)))

;; 4.58 Multiple Thinning Definition

(def-input (mthinh "MTHINH") (EFRCTHN WMAX THINRAT WEITRAT)
  (:documentation "Multiple Thinning Definition.

Format = (A6, 4F), Defaults = 1.E-4, 1.E30, 1., 1.

This ketword has to appear once for each thinning mode to be defined. At maximum
up to 6 thinning modes are possible. Their sequence is arbitrary.

EFRCTHN : Factor εthhadr which defines the energy fraction of the primary energy
below which the thinning algorithm becomes active for hadrons. If the fraction is
selected in a manner that this energy is below the lowest energy threshold of
ELCUT(i), i = 1...4 (keyword ECUTS page 94), thinning will not become active.

WMAX : Weight limit for thinning for hadrons. If the weight of a particle exceeds
WMAX, no further thinning is performed.

THINRAT : Defines em-thinning differing from hadronic thinning limit EFRCTHN by
the ratio of εthem/εthhadr which gives the ratio between the energy of the
em-particles and the energy of the hadronic particles below which the thinning
algorithm becomes active for these particle species (see also Sect. 4.55 page 90).

WEITRAT : Defines em-weight limit differing from hadronic weight limit WMAX by
the ratio of weight limit of em-particles to weight limit of hadronic particles
in case of thinning (see also Sect. 4.55 page 90 and Sect. 4.57 page 91).

Limits are: ULIMIT·EFRCTHN ≤ 1 · 107 GeV (for ULIMIT see keyword ERANGE Sect. 4.6
page 71); 0.1 ≤ WMAX ≤ 1 · 10^20 ; ULIMIT·EFRCTHN/THINRAT ≤ 1 · 10^7 GeV (for
ULIMIT see keyword ERANGE Sect. 4.6 page 71); 1 · 10^−4 ≤ WEITRAT ≤ 1 · 10^6 .

This keyword is only available in the MULTITHIN option.")
  (:formats (float EFRCTHN WMAX THINRAT WEITRAT))
  (:asserts (and (<= 0.1 WMAX 1e20)
                 (<= 1e-4 WEITRAT 1e-6))))

;; 4.59 Random Number Generator Initialization for MULTITHIN Modes

(def-input (mseed "MSEED") (ISEED1 ISEED2 ISEED3)
  (:documentation "Random Number Generator Initialization for MULTITHIN Modes.

Format = (A5, 3I), Defaults = j + 10, 0, 0

ISEED(1, j + 10) : Contains the seed of the random number sequence j + 10 .
ISEED(2..3, j + 10) : Contain the number of calls Nin to the generator that are
performed for initialization such that
  Nin =ISEED(2, j + 10) + 109·ISEED(3, j + 10).
At present at most j = 6 sequences are used to initialize the random generator
sequence j + 10 for the jth MULTITHIN mode. Their activation follows the sequence
of occurrence of the keyword lines.

The use of ISEED(2, j + 10) > 0 and especially of ISEED(3, j + 10) > 0 should be
avoided as presetting the random number generator by billions of calls needs
considerable computing time. To get different random sequences it is sufficient
to modify ISEED(1, j + 10).

Limit (to get independent sequences of random numbers) is:
1 ≤ ISEED(1, j + 10) ≤ 900 000 000 .

This keyword is only available in the MULTITHIN option.")
  (:formats (integer ISEED1 ISEED2 ISEED3))
  (:asserts (every (lambda (iseed) (<= 1 iseed 90000000))
                   (list ISEED1 ISEED2 ISEED3))))

;; 4.60 Maximum Radius in Multiple Thinning

(def-input (mthinr "MTHINR") (RCUT)
  (:documentation "Maximum Radius in Multiple Thinning.

Format = (A6, F), Default = 0.

RCUT : Maximum radius (in cm) at observation level within which all particles
are discarded. The value of RCUT is stored in the eventheader in EVTH(152).
For RCUT ≤ 0. no particles are discarded.

RCUT overrides RMAX of the keyword THIN (Sect. 4.55 page 90).

This keyword is only available in the MULTITHIN option.")
  (:formats (float RCUT))
  (:asserts t))

;; 4.61 Multiple Thinning used by COAST

(def-input (mweic "MWEIC") (IMWEIC MTHOUT)
  (:documentation "Multiple Thinning used by COAST.

Format = (A5, I, L), Default = 0, T

IMWEIC : Index (from 0 to the number of MTHINH lines) of the MTHINH line
used by COAST for the weight of particles. The value of IMWEIC is stored
in the eventheader in EVTH(223). For IMWEIC = 0 unthinned particles are
used.

MTHOUT : Enable/Disable writting the weight sub-block from MULTITHIN to
get a normal output file of unthinned particles but COAST uses the
thinned shower corresponding to IMWEIC.

This keyword is only available in the MULTITHIN option with COASTUSERLIB.")
  (:formats (integer IMWEIC) (boolean MTHOUT))
  (:asserts t))

;; 4.62 CORECUT Maximum Radius Cut

(def-input (corecut "CORECUT") (RCUT)
  (:documentation "CORECUT Maximum Radius Cut.

Format = (A7, F), Default = 0. (rsp. 200.E2 for AUGERHIT option)

RCUT : Maximum radius (in cm) at observation level within which all
particles are discarded. The value of RCUT is stored in the eventheader
in EVTH(152).

For RCUT ≤ 0. no particles are discarded.

RCUT overrides RMAX of the keyword THIN (Sect. 4.55 page 90).")
  (:formats (float RCUT))
  (:asserts t))

;; 4.63 Energy Cut-Offs

(def-input (ecuts "ECUTS") (ELCUT1 ELCUT2 ELCUT3 ELCUT4)
  (:documentation "Energy Cut-Offs.

Format = (A5, 4F), Defaults80 = 0.3, 0.3, 0.003, 0.003

ELCUT(i) : The low energy cut-off (in GeV) of the particle kinetic energy
may be chosen differently for hadrons (without π0’s) (i = 1), muons (i = 2),
electrons (i = 3), and photons (including π0’s) (i = 4). For nuclei ELCUT(1)
is applied to the energy per nucleon.

It is in the responsibility of the user to choose the cut-off values in a
reasonable way not to eliminate those parent particles which might decay to
secondaries which you are looking for in the investigated problem (e.g. decay
of muons to electrons).

Limits are: ELCUT(1) ≥ 0.05 ; ELCUT(2) ≥ 0.01; ELCUT(3), ELCUT(4) ≥ 0.00005
The value of ELCUT(3) is also taken as threshold value for the NKG calculation.
In this case an upper limit of ELCUT(3) < 0.08 is recommended.

In the CONEX option this keyword has to be called before the keyword CASCADE
(see page 88) is used.")
  (:formats (float ELCUT1 ELCUT2 ELCUT3 ELCUT4))
  (:asserts (and (>= ELCUT1 0.05)
                 (>= ELCUT2 0.01)
                 (>= ELCUT3 0.00005)
                 (>= ELCUT4 0.00005))))

;; 4.64 Time Cut-Off

(def-input (timlim "TIMLIM") (DSTLIM LTMLMPR)
  (:documentation "Time Cut-Off.

Format = (A6, F, L), Defaults = 1.D8, F

DSTLIM : Gives the distance (in cm) a particle would travel with velocity of
light downstream the detector before cut away by the time limit. An
additional security time of 25 µsec (corresponding with ≈ 7 km) is taken
into account.

DSTLIM can be positive or negative, but below 20km you must be careful. With
the INCLINED option , the minimum time is computed with respect to the
inclined observation plane origin. This option has to be used carefully not
to lose particles which should be counted.

LTMLMPR : Flag which enables (T) or disables (F) printing of particles which
exceed the time limit.

This keyword is only available in the CURVED option.")
  (:formats (float DSTLIM) (boolean LTMLMPR))
  (:asserts t))

;; 4.65 Longitudinal Shower Development

(def-input (longi "LONGI") (LLONGI THSTEP FLGFIT FLONGOUT)
  (:documentation "Longitudinal Shower Development.

Format = (A5, L, F, 2L), Defaults = F, 20.0, F, F

LLONGI : If .true., the longitudinal development of particle numbers for
gammas (EGS4), positrons (EGS4), electrons (EGS4), positive and negative
muons, hadrons, all charged, nuclei, and Cherenkov photons (CERENKOV) is
sampled. Moreover the longitudinal development of the energy content in
the various particle species (same order as before, but without Cherenkov
photons) is sampled. Additionally the longitudinal development of energy
deposit by ionization energy loss and by angular or energy cuts is sampled.
See also Sect. 10.1 page 127. To get the sampling in slant depth instead
of the (default) vertical depth you should use the SLANT option (page 61).
If .false., the longitudinal development is not sampled.

THSTEP : Vertical step width (rsp. slant step width in the SLANT option)
for sampling of the longitudinal development (in g/cm2). The sampling is
done in vertical (rsp. slant) depth. The altitudes are not depending on
the zenith angle of the primary particle (except the preprocessor option
SLANT has been selected). In the CURVED option the minimum step size has
to be selected in a manner that no more than 14998 steps are needed to
pass through the complete atmosphere.

FLGFIT : If .true. and LLONGI also .true., the longitudinal development
of all charged particles number is fitted. If .false., the fit is suppressed.

FLONGOUT : If .true. and LLONGI also .true., the longitudinal distributions
of particle numbers and energy deposit for the various particle groups are
written to the file ’DATnnnnnn.long’ (rsp. ’DATnnnnnnnnn.long’ with option
NRREXT, see Sect. 3.5.24 page 55) (see Sect. 10.5 page 145).

If .false. and LLONGI .true., the longitudinal distributions only of the
particle numbers for the various particle species are written out to the
particle output file ’DATnnnnnn’ (rsp. ’DATnnnnnnnnn’) in extra ’LONG’
sub-blocks (see Sect. 10.2, Table 6 page 130 and Table 15 page 140).

Limits are: 1. ≤ THSTEP ≤ 14998. 20. ≤ THSTEP ≤ 14998 for the SLANT option
and horizontal incidence.

Normally only to the number distribution of all charged particles a function
of the Gaisser-Hillas type

                                     t_max - t_0
                     t - t_0    ---------------------              t_max - t
  N(t) = N_max * (-------------) a + b * t + c * t^2   * exp(---------------------)
                   t_max - t_0                                a + b * t + c * t^2

is fitted to describe the dependence on the atmospheric depth t and the
resulting 6 parameters Nmax, t0, tmax, a, b, and c and the χ2/dof are stored
in the event end block. The longitudinal development of the electromagnetic
particles is only sampled if EGS4 is selected (see ELMFLG). If only NKG is
activated the fit is applied to the NKG longitudinal distribution which
consists of particle numbers from only ≤ 10 levels. If neither EGS4 nor NKG
is selected the charged particle distribution contains only muons and charged
hadrons. In the AUGERHIST option also a Gaisser-Hillas type function is fitted
to the longitudinal energy deposit, if EGS4 is selected.

In the Cherenkov versions the longitudinal distribution of photons is given in
differential mode (i.e. the number of photons generated within each step) as
default. By the preprocessor option INTCLONG the integral mode is selected
(i.e. accumulated number of generated Cherenkov photons for each step) which
needs additional computing time. If both kinds of the distribution are of no
interest, you may deselect the Cherenkov photon distribution completely by the
preprocessor option NOCLONG thus saving computing time.")
  (:formats (boolean LLONGI FLGFIT FLONGOUT) (float THSTEP))
  (:asserts t))

;; 4.66 Muon Multiple Scattering Treatment

(def-input (mumult "MUMULT") (FMOLI)
  (:documentation "Muon Multiple Scattering Treatment.

Format = (A6, L), Default = T

FMOLI : If .false., the muon multiple scattering angle is selected by Gauss
approximation. If .true., the muon multiple scattering angle is selected for
large steps by Moli`ere’s theory and for small steps by adding many single
Coulomb scattering events.")
  (:formats (boolean FMOLI))
  (:asserts t))

;; 4.67 Additional Electromagnetic Particle Information

(def-input (emaddi "EMADDI") (FEMADD)
  (:documentation "Additional Electromagnetic Particle Information.

Format = (A6, L), Default = F

FEMADD : If .false., no additional information on electromagnetic (EM) particles
is written to particle output file. If .true., additional information on mother
and grandmother hadrons of EM particles at the origin of the EM subshower is
written to the particle output file. Details are similar to those given in
Ref. [54] for muons in the EHISTORY option (Sect. 3.5.15 page 51). In case of
em-particle primaries the additional particle information is suppressed (by
automatically setting FEMADD = .false.) as for the bulk of the em-particles no
hadronic mother or grandmother particles exists.

This keyword is only available in the EHISTORY option and not in the
INTTEST option.")
  (:formats (boolean FEMADD))
  (:asserts t))

;; 4.68 Additional Muon Information

(def-input (muaddi "MUADDI") (FMUADD)
  (:documentation "Additional Muon Information.

Format = (A6, L), Default = F

FMUADD : If .false., no additional muon information is written to particle
output file. If .true., additional information on muons at their origin is
written to the particle output file. This additional muon information consists
of 7 data words according to Table 10 (page 135) and precedes the
corresponding muon particle on the particle output file. The first data word
contains the particle identification 75 (for µ+) or 76 (for µ−) combined with
3 digits of the hadronic generation counter (instead of the 2 digits + 1
digit for observation level), which in this case may differentiate between
muons originating from K-decay (normal generation counter) and π±-decay
(generation counter incremented by 50) and can be larger than 100 in some case
of electromagnetic interation. To indicate clearly the electromagnetic origin
of the muon (muon pair production or photonuclear interaction in muon history)
the hadronic generation counter is increased by 500. The 7th data word contains
the altitude (in cm) of the muon birth instead of time (see also Table 10 page
135). In combination with the EHISTORY option (see Sect. 3.5.15 page 51) an
extended additional muon information is written to the particle output file.
In the combination of the MUPROD option (see Sect. 3.5.22 page 55) with the
EHISTORY option the extended muon information is written also for muons which
decay or interact before reaching the lowest observation level.
Details are given in Ref. [54].

This keyword is not available in the INTTEST option.")
  (:formats (boolean FMUADD))
  (:asserts t))

;; 4.69 Additional Neutrino Information

(def-input (nuaddi "NUADDI") (FNUADD)
  (:documentation "Additional Neutrino Information.

Format = (A6, L), Default = F
FMUADD : If .false., no additional neutrino information is written to particle
output file. If .true., additional information on mother and grandmother hadrons
of neutrinos at their origin is written to the particle output file. Details are
similar to those given in Ref. [54] for muons in the EHISTORY option (Sect.
3.5.15 page 51). An extended additional neutrino information is written to the
particle output file.

This keyword is only available in the EHISTORY option and not in the INTTEST
option.")
  (:formats (boolean FNUADD))
  (:asserts t))

;; 4.70 Observation Level Definition

(def-input (obslev "OBSLEV") (OBSLEV)
  (:documentation "Observation Level Definition.

Format = (A6, F), Default = 110.E2

OBSLEV(i) : Observation level i above sea level (in cm). This keyword has to
appear once for each level to be defined. At maximum up to 10 observation levels
are possible83. Their sequence is arbitrary. In the UPWARD option (page 65) for
upward going primaries the observation level should be chosen preferentially at
the top of atmosphere, but at minimum above the starting point of the shower.
The value of OBSLEV has to be selected in a manner that the shower axis crosses
the observation level. Only one observation level is possible in the CURVED option.
Up to 20 levels might be specified for the production of histograms in the
AUGERHIST option, the lowest observation level must be at minimum 1 g/cm2 above
sea level.

Limits are84: 0 ≤ OBSLEV(i) < top of atmosphere

In the CURVED option only one observation level can be defined.")
  (:formats (float OBSLEV))
  (:asserts (<= 0.0 OBSLEV)))

;; 4.71 Inclined Observation Plane

(def-input (inclin "INCLIN") (XPINCL YPINCL ZPINCL THINCL PHINCL TDINCL)
  (:documentation "Inclined Observation Plane.

Format = (A6, 5F), Defaults = 0., 0., OBSLEV (1), 0., 0., 0.

XPINCL : X-coordinate (in cm) of reference point in inclined observation plane.

YPINCL : Y-coordinate (in cm) of reference point in inclined observation plane.

ZPINCL : Z-coordinate (in cm) of reference point in inclined observation plane.

THINCL : θ-angle (in deg) of normal vector of inclined observation plane. θ = 0.
points to the zenith (opposite to Fig. 1 page 120) and defines a horizontal plane.

PHINCL : φ-angle (in deg) of normal vector of inclined observation plane.

TDINCL : thickness (in g/cm2) of the origin of an observation plane perpendicular
to the shower axis (automatic definition of all other numbers which are not used
if TDINCL¿0). The behavior of this parameter depends on the SLANT option. If SLANT
is defined, TDINCL define the slant depth along the shower axis, where the
perpendicular plane should be placed. The origin to count the slant depth depends
on the use of FIXHEI. If this parameter is defined, then the slant depth is counted
from this point. Otherwise it will count from the starting point of the shower.
Without SLANT, TDINCL will define the fixed vertical depth (= fixed height) at
which the plane should be placed. In both cases, CORSIKA automatically defines
THINCL=THETAP and PHINCL=PHIP (shower by shower), to have a plane perpendicular to
the shower axis, and compute XPINCL, YPINCL and ZPINLC such that the axis is at the
origin of the frame of the inclined plane at the right vertical or slant depth. The
actual values of all the parameters including the thickness are registered is EVTH
(see Tab. 9 page 134).

All coordinates are defined in the standard CORSIKA output coordinate system (see
Fig. 1 page 120) with its origin at the point where the shower axis hits the
observation level. Detailed description of the coordinate definition is given in
Fig. 3 page 143.

ATTENTION: CORSIKA discards all particles BELOW the height of the lowest standard
observation level (see Sect. 4.70 page 97 for definition of OBSLEV), so as soon as
the inclined observation plane drops below the lowest standard (horizontal)
observation level, there will be no particles recorded any more. In case of UPWARDOLD
option, the particles ABOVE OBSLEV are discarded. This can also be used to limit the
computation time placing the horizontal observation level at the right place
compared to the inclined plane (in particular with UPWARDOLD for upward going
showers). TIMLIM (Sect. 4.64 page 94) has a special behavior and can be used
carefully to limit the computation time when the CURVED option is selected.
Particles on the inclined plane will be stored in a coordinate system within the
inclined plane, with the origin defined at (XPINCL, YPINCL, ZPINCL). See Sect. 10.3
page 142 for details.

This keyword uses the COAST package and is available only when the INCLINED option
(see page 27) in ./coconut has been selected. For atmospheric models 1 ≤ MODATM ≤ 9
and MODATM ≥ 17 limits are: −1.E5 < OBSLEV(i) < 112.8E5 Technically, the INCLINED
option uses a particular COASTUSERLIB library (Sec. 3.5.8 page 46), namely
InclinedPlane, which is shipped with COAST in the CorsikaOptions directory by
default.

This keyword is only available in the INCLINED option.")
  (:formats (float XPINCL YPINCL ZPINCL THINCL PHINCL TDINCL))
  (:asserts t))

;; 4.72 Observation Level Curvature

(def-input (curvout "CURVOUT") (FCURVOUT)
  (:documentation "Observation Level Curvature.

Format = (A7, L), Default = T

FCURVOUT : If .true. the observation level is a sphere following the Earth
curvature at an altitude H = OBSLEV (1) and (X, Y ) in the output file are
replaced by (X′, Y ′) which can be used to calculate the angles necessary to
obtain (X, Y, Z) in a Cartesian frame: defining θ and φ the spherical
coordinates of the particle as:

       √(X'^2 + Y'^2)
  θ = ----------------
        R_earth + H


  φ = atan2(Y', X')

with REarth = 637131500 cm. At the observation level (H = OBSLEV (1)) the
Cartesian coordinates can be obtained using D = (REarth + H) · sin θ as

  X = D * cos φ

  Y = D * sin φ

  Z = (R_earth + H) * cos θ − R_earth

If θ is small (close to shower core) then (X′, Y ′) = (X, Y ) . If .false. the
observation level is considered as flat and particle positions (X, Y ) are
defined in a Cartesian frame with the origin (0, 0) at the core position.
Particles are tracked only until Z = OBSLEV (1) in this Cartesian frame (apparent
height). As a consequence, far from the core, some particles can be discarded
before reaching the ground making a bias in the longitudinal profile. To avoid
such a problem, FCURVOUT = .false. is forbidden by default if the zenith angle is
between 85◦ and 95◦.

As usually the Cherenkov detector geometries are small, the FCURVOUT is set .false.
in the CERENKOV option.

The keyword FLATOUT = .not.FCURVOUT can still be used for backward compatibility.
This keyword is only available in the CURVED option and not in the ANAHIST or
AUGERHIST options.")
  (:formats (boolean FCURVOUT))
  (:asserts t))

;; 4.73 Array Rotation

(def-input (arrang "ARRANG") (ARRANG)
  (:documentation "Array Rotation.

Format = (A6, F), Default = 0.

ARRANG : Defines a rotation angle (in ◦) between the detector array x-direction
and magnetic north direction; positive if detector array x-direction points to
the West.

Attention: The default value of ARRANG is set differently to -92.08◦ if the
AUGERHIT option (see Sect. 3.5.5 page 44) is selected.

Limits are: -180. ≤ ARRANG ≤ 180.

This keyword is not available in the COASTUSERLIB option.")
  (:formats (float ARRANG))
  (:asserts (<= -180 ARRANG 180)))

;; 4.74 Auger Detector Scattering

(def-input (augsct "AUGSCT") (MAUGPOS DRADIUS DETDIS FTANKSHADOW FANYMODE)
  (:documentation "Auger Detector Scattering.

Format = (A6, I, 2F, 2L), Default = 1, 35., 1500., .true., .true.

MAUGPOS : Number of uses of each event for the Auger detector. For MAUGPOS = 0
the scatter positions are expected to be given via the keyword AUGHIT (see Sect.
4.75). For MAUGPOS > 0 the scatter positions are taken at random employing the
SOBOL quasi-random generator [40].

DRADIUS : Half width (in meter) of the stripes in which the Auger detector rows
are aligned.

DETDIS : Shortest distance (in meter) between two tanks on the hexagonal tank
array.

FTANKSHADOW : Enable (=.true.) or disables (=.false.) the check whether a particle
falls in a tank shadow. One should select DRADIUS large enough for long shadows
of very inclined particles. The tank dimensions are fixed at 1.8 m radius and
1.2 m height as used for the Auger detectors.

FANYMODE : Enable (=.true.) or disable (=.false.) the check whether a particle
survives any thin mode of the MULTITHIN option (see Sect. 3.5.20 page 54).

Limits are: 0 ≤ MAUGPOS ≤ 20; DRADIUS ≥ 0.; DETDIS ≥ 0.

This keyword is only available in the AUGERHIT option.")
  (:formats (integer MAUGPOS FTANKSHADOW FANYMODE) (float DRADIUS DETDIS))
  (:asserts (and (<= 0 MAUGPOS 20)
                 (>= DRADIUS 0)
                 (>= DETDIS 0.0))))

;; 4.75 Auger Detector Scattering Positions

(def-input (aughit "AUGHIT") (XSHCORE YSHCORE)
  (:documentation "Auger Detector Scattering Positions.

Format = (A6, 2F), Default = 0., 0.

XSHCORE(i) : Detector position in x-direction (in meter) of the ith scattering
relative to the shower core.

YSHCORE(i) : Detector position in y-direction (in meter) of the ith scattering
relative to the shower core.

This keyword has to be activated by setting the parameter MAUGPOS = 0 of the
keyword AUGSCT (see Sect. 4.74). The positions have to be specified in the
detector coordinate frame (with the positive X-axis pointing to East), the
length units are meter. For each scatter position its own input line is needed
and up to 20 lines are tolerated in arbitrary sequence.

This keyword is only available in the AUGERHIT option.")
  (:formats (float XSHCORE YSHCORE))
  (:asserts t))

;; 4.76 String Detector Configuration

(def-input (detcfg "DETCFG") (DETCFG)
  (:documentation "String Detector Configuration.

Format = (A6, F), Default = 0.

DETCFG : Gives the geometry configuration of a long vertical string detector
as the ratio height/diameter.

Limit is: DETCFG > 0.

This keyword is only available in the VOLUMECORR option.")
  (:formats (float DETCFG))
  (:asserts (> DETCFG 0.0)))

;; 4.77 Event Printout

(def-input (maxprt "MAXPRT") (MAXPRT)
  (:documentation "Event Printout.

Format = (A6, I), Default = 1

MAXPRT : Is the maximum number of events that produce a detailed printout
during the simulation run.

Limit is: MAXPRT ≥ 1")
  (:formats (integer MAXPRT))
  (:asserts (>= MAXPRT 1)))

;; 4.78 Particle Printout

(def-input (ectmap "ECTMAP") (ECTMAP)
  (:documentation "Particle Printout.

Format = (A6, F), Default = 1.E11

ECTMAP : Defines a cut in the particle γ factor (or energy in GeV for
em-particles and neutrinos) above which they are printed out on the logical
unit MONIOU when passing an observation level.")
  (:formats (float ECTMAP))
  (:asserts t))

;; 4.79 Output Directory

(def-input (direct "DIRECT") (DSN)
  (:documentation "Output Directory.

Format = (A6, A239), Defaults = ’anynameupto239characters/’

DSN : May be used to define a name of an output directory.
Lower case characters of DSN are not converted to capitals. Do not use
capitals with the ANAHIST, AUGERHIST, MUONHIST, or INTTEST options as the
HBOOK routines use only lower case characters. To suppress the output you
should use the keyword PAROUT and not the /dev/null path which is not
allowed anymore.

If you want to write into the directory from where you are starting your
CORSIKA run, you should give:

  DIRECT ./

or

  DIRECT ’ ’

or

  DIRECT " "

(a blank enclosed in apostrophes or quotation marks). Please keep in mind that
in FORTRAN an automatic expansion of UNIX names like ’$HOME’ is not possible,
rather you should give the fully expanded name of the directory ending with
a / (slash) character.

Limit is: DSN must not begin with a ∼ (tilde) character.")
  (:formats (string DSN))
  (:asserts (<= (length DSN) 239)))

;; 4.80 Table Output

(def-input (parout "PAROUT") (FPAROUT FTABOUT)
  (:documentation "Table Output.

Format = (A6, 2L) Defaults = T, F

FPAROUT : If .false., the particle output onto MPATAP is suppressed. This might
be of advantage with the CERENKOV option to suppress the particle output file
but keeping the Cherenkov output file (see Sect. 4.90).

FTABOUT : If .true., the tabular output of the charged particle development is
written out to the file ’DATnnnnnn.tab’ (rsp. ’DATnnnnnnnnn.tab’ with option
NRREXT, see Sect. 3.5.24 page 55) onto the output directory DSN (see Sect.
4.79 above).")
  (:formats (boolean FPAROUT FTABOUT))
  (:asserts t))

;; 4.81 Compact Output

(def-input (comout "COMOUT") (COMOUT)
  (:documentation "Compact Output.

Format = (A6, L), Default = T

COMOUT : If .true., the particle output is written in COMPACT form (see page 47).
If .false., the standard CORSIKA particle output is written.

This keyword is only available in the COMPACT option.")
  (:formats (boolean COMOUT))
  (:asserts t))

;; 4.82 Printer Output Unit

(def-input (output "OUTPUT") (MONNEW)
  (:documentation "Printer Output Unit.

Format = (A6, I), Default = 6

MONNEW : Logical unit of simulation control output on line printer. Make sure
that your selection of MONNEW is not conflicting with existing definitions
(see Table 1 page 25).

In the PARALLEL + PARALLELIB option the default value of MONNEW is changed to 89
for writing individual output files with names as the cutfile seeds, see Ref. [60].")
  (:formats (integer MONNEW))
  (:asserts t))

;; 4.83 Data Table Directory

(def-input (datdir "DATDIR") (DATDIR)
  (:documentation "Data Table Directory.

Format = (A6, A132), Default = ’./’

DATDIR : Can be used to specify a common directory, where CORSIKA will try to
find all required input data tables (except those belonging to the FLUKA
versions). Lower case characters of DATDIR are not converted to capitals.

Limit is: DATDIR must not begin with a ∼ (tilde) character.")
  (:formats (string DATDIR))
  (:asserts (<= (length DATDIR) 132)))

;; 4.84 Parameters for Parallel Treatment

(def-input (parallel "PARALLEL") (ECTCUT ECTMAX MPIID FECTOUT)
  (:documentation "4.84 Parameters for Parallel Treatment.

Format = (A8, 2F, I, L), Defaults = 1.E4, 1.E7, 1, F

ECTCUT : Threshold energy (in GeV) for subshowers. All particles with energies
above ECTCUT will have a seed from the 6th sequence of random numbers (see
keyword SEED page 70).

ECTMAX : Maximum energy (in GeV) for a complete subshower.

MPIID : Identification number of the parallel task which will be changed
internally by MPI.

FECTOUT : If .true., particles with energy above ECTCUT are written additionally
in an external DATnnnnnn-iiiiiiiii-kkkkkkkkk.cut file. This is needed when
parallel runs are performed via shell scripting. These cut-files are written in
binary mode. For the case of parallel runs via MPI treatment this is optional
and could be used for debugging or for additional re-simulations of secondary
subshowers to escape re-running the whole simulation from start/primary particle.

Those cut-files are written in ASCII.

This keyword has to be specified before the keyword CUTFILE (see Sect. 4.85 below)
is used.

This keyword is only available in the PARALLEL option 85.")
  (:formats (float ECTCUT ECTMAX) (integer MPIID) (boolean FECTOUT))
  (:asserts t))

;; 4.85 Reading of Cutted Particle File for Parallel Treatment

(def-input (cutfile "CUTFILE") (CFILINP I1CUTPAR I2CUTPAR)
  (:documentation "Reading of Cutted Particle File for Parallel Treatment.

Format = (A7, A255, 2I), Defaults = ’ ’, 0, 0

CFILINP : Input file name for cut particles to be read into second stack.

I1CUTPAR : Index of first particle to be used for the actual run.

I2CUTPAR : Index of last particle to be used for the actual run.

This keyword 85 has to be used when compilation is made with the PARALLEL
option (see Sect. 3.5.26) to run parallel CORSIKA simulations distributed
with the help of shell scripts. It must be used after the keyword PARALLEL
(see Sect. 4.84 above) has been specified.

This keyword is only available in the PARALLEL option, but not interpreted by
CORSIKA in the PARALLELIB option with MPI.")
  (:formats (string CFILINP) (integer I1CUTPAR I2CUTPAR))
  (:asserts (<= (length CFILINP) 255)))

;; 4.86 Parameters of Cutted Particles for Parallel Treatment
;; Note: this is not well defined

(def-input (cutpar "CUTPAR")
    (CUTPAR1
     CUTPAR2  CUTPAR3  CUTPAR4  CUTPAR5  CUTPAR6  CUTPAR7
     CUTPAR8  CUTPAR9  CUTPAR10 CUTPAR11 CUTPAR12 CUTPAR13
     CUTPAR14 CUTPAR15 CUTPAR16 CUTPAR17 CUTPAR18 CUTPAR19)
  (:documentation "Parameters of Cutted Particles for Parallel Treatment.

Format = (A6, 19Z)

CUTPAR(0...18) : Parameters (in hex. format) of particle to be read into the
second stack.

This keyword is only available in the PARALLEL option, but not in the
PARALLELIB option.")
  (:formats (string CUTPAR1
                    CUTPAR2  CUTPAR3  CUTPAR4  CUTPAR5  CUTPAR6  CUTPAR7
                    CUTPAR8  CUTPAR9  CUTPAR10 CUTPAR11 CUTPAR12 CUTPAR13
                    CUTPAR14 CUTPAR15 CUTPAR16 CUTPAR17 CUTPAR18 CUTPAR19))
  (:asserts t))

;; 4.87 Cherenkov Detector Array Definition

(def-input (cerary "CERARY") (NCERX NCERY DCERX DCERY ACERX ACERY)
  (:documentation "Cherenkov Detector Array Definition.

Format = (A6, 2I, 4F), Defaults = 27, 27, 1500., 1500., 100., 100.

NCERX : Number of Cherenkov detectors in X-direction.

NCERY : Number of Cherenkov detectors in Y-direction.

DCERX : Grid spacing (in cm) of Cherenkov detectors in X-direction.
The DCERX value has no relevance in case of NCERX = 1.

DCERY : Grid spacing (in cm) of Cherenkov detectors in Y-direction.
The DCERY value has no relevance in case of NCERY = 1.

ACERX : Length (in cm) of each Cherenkov detector in X-direction.

ACERY : Length (in cm) of each Cherenkov detector in Y-direction.

The altitude of this array is at the lowest observation level. For the
definition of the X and Y-directions see Fig. 1 (page 120) and keyword
ARRANG (page 99).

Limits are: NCERX, NCERY ≥ 1 ; DCERX, DCERY, ACERX, ACERY ≥ 1.

This keyword is only available in the CERENKOV option, but not in the IACT
option for Cherenkov telescopes.")
  (:formats (integer NCERX NCERY) (float DCERX DCERY ACERX ACERY))
  (:asserts (and (every (lambda (val) (>= val 1))
                        (list NCERX NCERY DCERX DCERY ACERX ACERY)))))

;; 4.88 Cherenkov Wavelength Band

(def-input (cwavlg "CWAVLG") (WAVLGL WAVLGU)
  (:documentation "Cherenkov Wavelength Band.

Format = (A6, 2F), Defaults = 300., 450.

WAVLGL : Lower limit (in nm) of the wavelength band for Cherenkov radiation
production.

WAVLGU : Upper limit (in nm) of the wavelength band for Cherenkov radiation
production.

Limits are: 100. ≤ WAVLGL < WAVLGU ≤ 700. (rsp. 2000 for the IACT option)

This keyword is only available in the CERENKOV, AUGCERLONG, or AUGERHIST
options.")
  (:formats (float WAVLGL WAVLGU))
  (:asserts (and (<= 100.0 WAVLGL WAVLGU 700.0)
                 (< WAVLGL WAVLGU))))

;; 4.89 Cherenkov Bunch Size Definition

(def-input (cersiz "CERSIZ") (CERSIZ)
  (:documentation "Cherenkov Bunch Size Definition.

Format = (A6, F), Default = 0.

CERSIZ : Defines the maximal bunch size of Cherenkov photons that are treated
together. If set to 0., by the subroutine getbus the program calculates a
bunch size which is found to be appropriate for the HEGRA-array.

Limit is: CERSIZ ≥ 0.

This keyword is only available in the CERENKOV, AUGCERLONG, or AUGERHIST options.")
  (:formats (float CERSIZ))
  (:asserts (>= CERSIZ)))

;; 4.90 Cherenkov Output Steering

(def-input (cerfil "CERFIL") (MCERFI)
  (:documentation "Cherenkov Output Steering.

Format = (A6, I), Default = 1 (rsp. 2 for CERWLEN option)

MCERFI : Steers the Cherenkov output to MPATAP or MCETAP.
+ MCERFI = 0: Output goes to MPATAP.
+ MCERFI 0 <MCERFI< 3: Output goes to MCETAP.
+ MCERFI ≥ 3: Output goes to MPATAP with name extension.
In the CERENKOV option (Cherenkov telescopes) with MCERFI > 0, the output
file name DSN (specified by keyword DIRECT) should be set to /dev/null 86 to
suppress the normal Cherenkov output file, as in the IACT option the Cherenkov
telescope output will be written to the eventio output file.

+ MCERFI = 1: is set automatically in the COMPACT option to prevent a writing
  of Cherenkov photons to the COMPACT output.
+ MCERFI = 2: In the THIN option the last (8th) item of a bunch (see Table 11
  page 135) gives the wavelength instead of the weight factor.
+ MCERFI ≥ 3: Acts like MCERFI = 2, but now the 7th item of each bunch gives
  the distance from the photon emission point to the detector array center
  instead of the photon emission height (See Table 11 page 135). Moreover the
  name convention of the Cherenkov data output files are changed to
  DATnnnnnn.cher-teliii (see Sect. 3.4.3 page 40) (rsp.
  DATnnnnnnnnn.cher-teliii with option NRREXT, see Sect. 3.5.24 page 55).

Limit is: 0 ≤ MCERFI .

This keyword is only available in the CERENKOV option.")
  (:formats (integer MCERFI))
  (:asserts (<= 0 MCERFI)))

;; 4.91 Cherenkov Quantum Efficiency

(def-input (cerqef "CERQEF") (CERQEF CERATA CERMIR)
  (:documentation "Cherenkov Quantum Efficiency.

Format = (A6, 3L), Defaults = F, F, F

CERQEF : If .true., quantum efficiency of detector photomultiplier is taken
into account. It needs reading in the quanteff.dat file.

CERATA : If .true., the atmospheric absorption of Cherenkov photons is taken
into account. It needs reading in the atmabs.dat file.

CERMIR : If .true., the mirror reflectivity of Cherenkov telescopes is taken
into account. It needs reading in the mirreff.dat file.

Respecting these effects at an early stage of the Cherenkov photon simulation
drastically reduces computing time and storage requirements for Cherenkov photon
output. For the influence onto the longitudinal distribution of Cherenkov photons
see Sect. 3.4.5 page 41 and keyword LONGI page 96.

This keyword is only available in the CEFFIC option together with the CERENKOV
option.")
  (:formats (boolean CERQEF CERATA CERMIR))
  (:asserts t))

;; 4.92 Cherenkov Wavelength Range

(def-input (cwavrang "CWAVRANG") (WLMIN WLMAX)
  (:documentation "Cherenkov Wavelength Range.

Format = (A8, 2F), Defaults = 180., 700.

This keyword is only necessary for user supplied data files with a Cherenkov
wavelength range different from the standard files delivered with CORSIKA
(see Sect. 3.4.5 page 41).

WLMIN : Lower limit (in nm) of the wavelength band available in the atmabs.dat,
quanteff.dat, and mirreff.dat data files.

WLMAX : Upper limit (in nm) of the wavelength band available in the atmabs.dat,
quanteff.dat, and mirreff.dat data files.

Limits are: 0. ≤ WLMIN < WLMAX ≤ 1000.

This keyword is only available in the CERENKOV, AUGCERLONG, or AUGERHIST options.")
  (:formats (float WLMIN WLMAX))
  (:asserts (and (<= 0.0 WLMIN WLMAX 1000.0)
                 (< WLMIN WLMAX))))

;; 4.93 Multiple Use of Cherenkov Events

(def-input (cscat "CSCAT") (ICERML XSCATT YSCATT)
  (:documentation "Multiple Use of Cherenkov Events.

Format = (A5, I, 2F), Defaults = 0, 0., 0.

ICERML : Number of uses of each event.

XSCATT : Maximum scattering of core location in ±X direction (in cm).
See Sect. 3.4.1 page 38 ff.

YSCATT : Maximum scattering of core location in ±Y direction (in cm). See
Sect. 3.4.1 page 38 ff.

Limits are: 0 ≤ ICERML ≤ 20 ; XSCATT, YSCATT ≥ 0.

In case of Cherenkov telescopes ICERML telescope arrays are simulated randomly
(see keyword SEED page 70) in the specified area which is a circle of radius
XSCATT, if YSCATT = 0., or within a rectangle of area 2 XSCATT · 2 YSCATT.

This keyword is only available in the CERENKOV option.")
  (:formats (integer ICERML) (float XSCATT YSCATT))
  (:asserts (and (<= 0 ICERML 20)
                 (>= XSCATT 0.0)
                 (>= YSCATT 0.0))))

;; 4.94 Core Position of Scattered Cherenkov Event

(def-input (corepos "COREPOS") (CERXOS CERYOS)
  (:documentation "Core Position of Scattered Cherenkov Event.

Format = (A7, 2F), Defaults = 0., 0..

CERXPOS(i) : ith position of scattered core in ±X direction (in cm).

CERYPOS(i) : ith position of scattered core in ±Y direction (in cm).

If the keyword COREPOS appears before the CSCAT keyword, the scattering by
the CSCAT input is disabled. IF this keyword appears after the CSCAT keyword
the core positions defined by COREPOS are appended as additional scatter
positions after the randomly selected core postitions.

Limit: 0 ≤ i ≤ 20 core positions are possible.

This keyword is only available in the CERENKOV option.")
  (:formats (float CERXOS CERYOS))
  (:asserts t))

;; 4.95 Cherenkov Telescope Dimensions

(def-input (telescope "TELESCOPE") (X Y Z R ID)
  (:documentation "Cherenkov Telescope Dimensions.

Format = (A9, 4F, I)

X, Y, Z : Coordinates of Cherenkov telescope (in cm) relative to the center of
the observation level. So at (0,0,0) means at the core of the shower at the
observation level altitude.

R : Radius of sphere (in cm) within which the telescope is fully contained.

ID : Identification number of Cherenkov telescope. This number must be given
without the IACT option. It is used to specify the extension of the output file
name CERnnnnnn-TELiii (rsp. CERnnnnnnnnn-TELiii) where iii is the telescope ID
(see Sect. 3.4.3 page 40). For ID = 0 the Cherenkov output file name extension
’-TELiii’ is omitted. This keyword adds a new telescope at position X, Y , Z
with radius R. At least one telescope has to be specified. It is recommended to
define at least Z = R not to cut half of the sphere by the observation level.
For the definition of the X and Y-directions see Fig. 1 (page 120) and keyword
ARRANG (page 99).

Limits are: 0 < R; 0 ≤ ID of telescope ≤ 999.

This keyword is only available in the CERENKOV rsp. IACT option.")
  (:formats (float X Y Z R) (integer ID))
  (:asserts (and (< R) (<= 0 ID 999))))

;; 4.96 Cherenkov Telescope Data File Name

(def-input (telfil "TELFIL") (TELFNM)
  (:documentation "Cherenkov Telescope Data File Name.

Format = (A6, A512), Default = ’ ’

TELFNM : The telescope-specific data are to be written to a file named TELFNM
in eventio format. Lower case characters of TELFNM are not converted to capitals.
If this file exists and is write-enabled, new data are appended. After ending the
run the file will be set read-only to avoid accidental overwriting. The file name
/dev/null suppresses the output file.

If you want to write into the directory from where you are starting your CORSIKA
run, you should give:

  TELFIL ./

or

  TELFIL ’ ’

or

  TELFIL " "

(a blank enclosed in apostrophes or quotation marks).

Please keep in mind that in FORTRAN an automatic expansion of UNIX names like
’$HOME’ is not possible, rather you should give the fully expanded name of the
directory ending with a / (slash) character.

Limit is: TELFNM must not begin with a ∼ (tilde) character.

This keyword is only available in the CERENKOV option together with the IACT
option for Cherenkov telescopes.")
  (:formats (string TELFNM))
  (:asserts (<= (length TELFNM) 512)))

;; 4.97 Trajectory Selection Flag

(def-input (traflg "TRAFLG") (TLOGIC)
  (:documentation "Trajectory Selection Flag.

Format = (A6, L), Default = T

TLOGIC : If .true., the zenith and azimuth angles of each shower event are
determined by the TRAJECT option to follow a source object movement in the sky.
The determination of the angles by other options (VIEWCONE, VOLUMECORR, or
VOLUMEDET) or by the keywords

THETAP and PHIP is disregarded.

This keyword is only available in the TRAJECT option.")
  (:formats (boolean TLOGIC))
  (:asserts t))

;; 4.98 Source Position Parameters

(def-input (srcpos "SRCPOS") (RA DEC)
  (:documentation "Source Position Parameters.

Format = (A6, 2F), Defaults = 5.57, 22.0

RA : Defines the right ascension (in hours) of the simulated source in equatorial
coordinates.

DEC : Defines the declination (in ◦) of the simulated source in equatorial
coordinates. The default values give the Crab Nebula.

Limits are: 0. < RA < 24.; −90. < DEC < +90. .

This keyword is only available in the TRAJECT option.")
  (:formats (float RA DEC))
  (:asserts (and (< 0.0 RA 24.0)
                 (< -90.0 DEC 90.0))))

;; 4.99 Trajectory Time Parameters

(def-input (tratm "TRATM") (TYEAR TMONTH TDAY THOUR TMINUTE TSECOND TDURATION)
  (:documentation "Trajectory Time Parameters.

Format = (A5, 7I), Defaults = 2000, 1, 1, 21, 0, 0, 3600

TYEAR, TMONTH, TDAY, THOUR, TMINUTE, TSECOND : Define the start time of the
observation (in year, month, day, hour, min, sec).

TDURATION : Duration of observation (in sec).

Limits are: Only valid values for date and time are admitted; 0 < TDURATON.
This keyword is only available in the TRAJECT option.")
  (:formats (integer TYEAR TMONTH TDAY THOUR TMINUTE TSECOND TDURATION))
  (:asserts (and (<= 0 THOUR 24)
                 (<= 0 TMINUTE 60)
                 (<= 0 TSECOND 60)
                 (< 0 TDURATION))))

;; 4.100 Lateral Telescope Site Parameters

(def-input (tlat "TLAT") (TLATDGR TLATMIN TLATSEC TLATDIR)
  (:documentation "Lateral Telescope Site Parameters.

Format = (A4, 3F, A1), Defaults = 28., 45., 42.462, ’N’

TLATDGR : Latitude of the telescope site (in ◦).
TLATMIN : Latitude of the telescope site (in min).
TLATSEC : Latitude of the telescope site (in sec).
TLATDIR : Direction North = ’N’, South = ’S’ of the latitude of the telescope site.

The default values give the site of the MAGIC telescope.

Limits are: 0. ≤ TLATDGR ≤ 90.; 0. ≤ TLATMIN ≤ 60.; 0. ≤ TLATSEC ≤ 60.;
TLATDIR = ’N’ or ’S’.

This keyword is only available in the TRAJECT option.")
  (:formats (float TLATDGR TLATMIN TLATSEC) (string TLATDIR))
  (:asserts (and (<= 0.0 TLATDGR 90.0)
                 (<= 0.0 TLATMIN 60.0)
                 (<= 0.0 TLATSEC 60.0)
                 (or (string= TLATDIR "N")
                     (string= TLATDIR "S")))))

;; 4.101 Longitudinal Telescope Site Parameters

(def-input (tlong "TLONG") (TLONGDGR TLONGMIN TLONGSEC TLONGDIR)
  (:documentation "Longitudinal Telescope Site Parameters.

Format = (A5, 3F, A1), Defaults = 17., 53., 26.525, ’W’

TLONGDGR : Longitude of the telescope site (in ◦).
TLONGMIN : Longitude of the telescope site (in min).
TLONGSEC : Longitude of the telescope site (in sec).
TLONGDIR : Direction East = ’E’, West = ’W’ of the longitude of the telescope site.

The default values give the site of the MAGIC telescope.
Limits are: 0. ≤ TLONGDGR ≤ 180.; 0. ≤ TLONGMIN ≤ 60.; 0. ≤ TLONGSEC ≤ 60.;
TLONGDIR = ’E’ or ’W’.

This keyword is only available in the TRAJECT option.")
  (:formats (float TLONGDGR TLONGMIN TLONGSEC) (string TLONGDIR))
  (:asserts (and (<= 0.0 TLONGDGR 180.0)
                 (<= 0.0 TLONGMIN 60.0)
                 (<= 0.0 TLONGSEC 60.0)
                 (or (string= TLONGDIR "W")
                     (string= TLONGDIR "E")))))

;; 4.102 Geomagnetic Declination of Telescope

(def-input (geodec "GEODEC") (GEODECL)
  (:documentation "Geomagnetic Declination of Telescope.

Format = (A6, F), Default = -6.35

GEODECL : Defines the geomagnetic declination 90, i.e. the directional
deviation of the magnetic North from the geographic North (in ◦).

The default value corresponds with the site of the MAGIC telescope.

Limit is: -45. ≤ GEODECL ≤ +45. .

This keyword is only available in the TRAJECT option.")
  (:formats (float GEODECL))
  (:asserts (<= -45.0 GEODECL 45.0)))

;; 4.103 Trajectory Broadening Parameter

(def-input (trarad "TRARAD") (TRAD)
  (:documentation "Trajectory Broadening Parameter.

Format = (A6, F), Default = 0.

TRAD : Defines the radius of a spread around the calculated trajectory
(in arcmin).

Limit is: 0. ≤ TRAD ≤ 3600. .

This keyword is only available in the TRAJECT option.")
  (:formats (float TRAD))
  (:asserts (<= 0.0 TRAD 3600.0)))

;; 4.104 Interesting Energy Threshold

(def-input (einter "EINTER") (ENERGY-INTER)
  (:documentation "Interesting Energy Threshold.

Format = (A6, F), Default = 1000.

ENERGY INTER : Threshold energy (in GeV) above which particles are interesting
for production of high-energy neutrinos for the IceCube [56] experiment.

Limit is: 0 ≤ ENERGY INTER.

This keyword is only available in the ICECUBE1 option.")
  (:formats (float ENERGY-INTER))
  (:asserts (<= 0.0 ENERGY-INTER)))

;; 4.105 PIPE Output Redirection Flag

(def-input (pipe "PIPE") (PIPE-OUTPUT)
  (:documentation "PIPE Output Redirection Flag.

Format = (A4, L), Default = F

PIPE OUTPUT : If .true., the output is redirected to a pipe-buffer.
This buffer should serve as on-line input to a detector simulation program.

This keyword is only available in the ICECUBE2 option.
The sign of the declination is defined positive for eastward declination,
negative for westward declination, see also Ref. [70]. ")
  (:formats (boolean PIPE-OUTPUT))
  (:asserts t))

;; 4.106 Compress Output Flag

(def-input (compress "COMPRESS") (GZIP-OUTPUT)
  (:documentation "Compress Output Flag.

Format = (A8, L), Default = T

GZIP OUTPUT : If .true., the output is compressed using the gzip procedure
to reduce storage space. The gain in space reduction will not be very
dramatic.

This keyword is only available in the ICECUBE2 option.")
  (:formats (boolean GZIP-OUTPUT))
  (:asserts t))

;; 4.107 Size of Stack
;; Note: not clear in spec doc. 

(def-input (dynstack "DYNSTACK") (N1-N2-NX)
  (:documentation "Size of Stack. (manually: DYNSTACK N1 N2 ... NX)

Format = (A8, A500), Default = ”

Forwards a variable number of integer numbers from the steering card to
the DYNSTACK setup routine. In normal use cases this should contain the
size of the stack.

This keyword is only available in the DYNSTACK option.")
  (:formats (string N1-N2-NX))
  (:asserts t))

;; 4.108 Parameter to Stack

(def-input (dynstack-param "DYNSTACK_P") (A500)
  (:documentation "Parameter to Stack.

Format = (A10, A500), Default = ”

Forwards a variable number of characters, separated at whitespaces, to the
DYNSTACK setup routine. This call should be used to set any stack-specific
settings used in the current simulation.

This keyword is only available in the DYNSTACK option.")
  (:formats (string A500))
  (:asserts (<= (length A500) 500)))

;; 4.109 Remote IP

(def-input (remotecontrol-ip "REMOTECONTROL_IP") (A20)
  (:documentation "Remote IP.

Format = (A16, A20), Default = ”

Forwards a IP from the external server to connect the network thread to.
The format equals the default IP format, e.g. 127.0.0.1:1337, where the
part subsequent to the colon denotes the port number.

This keyword is only available in the REMOTECONTROL option.")
  (:formats (string A20))
  (:asserts (<= (length A20) 20)))

;; 4.110 Parameter to Remotecontrol

(def-input (remotecontrol-param "REMOTECONTROL_P") (A500)
  (:documentation "Parameter to Remotecontrol.

Format = (A10, A500), Default = ”

Forwards a variable number of characters, separated at whitespaces, to
the REMOTECONTROL setup routine. This call should be used to set any
connection-specific settings used in the current simulation.

This keyword is only available in the REMOTECONTROL option.")
  (:formats (string A500))
  (:asserts (<= (length A500) 500)))

;; 4.111 Write Data Base File

(def-input (datbas "DATBAS") (FDBASE)
  (:documentation "Write Data Base File.

Format = (A6, L), Default = F

FDBASE : If .true., all essential run parameters are written to the file
’DATnnnnnn.dbase’ (rsp. ’DATnnnnnn.info’ or ’DATnnnnnnnnn.info’ in the
AUGERINFO option) onto the output directory DSN (keyword DIRECT page 101).
This file may be used to build a data base for examining the content of
an air shower library (page 145).

This keyword is only effective in the UNIX options.")
  (:formats (boolean FDBASE))
  (:asserts t))

;; 4.112 User Name

(def-input (user "USER") (USER)
  (:documentation "User Name.

Format = (A4, A60), Defaults = ’ ’

USER : A user name is read in to be written to the ’DATnnnnnn.dbase’ file.
If the NRREXT option is used the file name gets ’DATnnnnnnnnn.dbase’
(see Sect. 3.5.24 page 55). Lower case characters of USER are not converted
to capitals.")
  (:formats (string USER))
  (:asserts (<= (length USER) 60)))

;; 4.113 Host Name

(def-input (host "HOST") (HOST)
  (:documentation "Host Name.

Format = (A4, A60), Defaults = ’ ’

HOST : A host name is read in to be written to the ’DATnnnnnn.dbase’ file.
If the NRREXT option is used the file name gets ’DATnnnnnnnnn.dbase’
(see Sect. 3.5.24 page 55). Lower case characters of HOST are not converted
to capitals.")
  (:formats (string HOST))
  (:asserts (<= (length HOST) 60)))

;; 4.114 Debugging

(def-input (debugging "DEBUG") (DEBUG MDEBUG DEBDEL NDEBDL)
  (:documentation "Debugging.

Format = (A5, L, I, L, I), Defaults = F, MONIOU, F, 100000

DEBUG : If .false., debugging is disabled. If .true., additional output for
debugging purposes is given on logical unit MDEBUG.

MDEBUG : Logical unit where to write debugging information. Make sure that
your selection of MDEBUG is not conflicting with existing definitions (see
Table 1 page 25).

DEBDEL : If .true., the debugging printouts are activated after NDEBDL
particles above the ECTMAP energy have been printed. If .false., delayed
debugging is disabled. This feature helps to trace run time errors that
have occurred in long simulation runs.

NDEBDL : See DEBDEL

Limit is: 0 ≤ NDEBDL.")
  (:formats (boolean DEBUG DEBDEL) (integer MDEBUG NDEBDL))
  (:asserts (<= 0 NDEBDL)))

;; 4.115 Debugging EGS

(def-input (egsdeb "EGSDEB") (JCLOCK)
  (:documentation "Debugging EGS.

Format = (A6, I), Default = 2147483647

JCLOCK : Counter for delayed start of EGS4 debugging. After activation of
debug by DEBUG or by NDEBDL (see Sect. 4.114 above) each pass of subroutine
electr or photon is counted.

If the counter exceeds JCLOCK, the debug statements within the EGS4 portion
are activated.

This output appears on the unit MDEBUG.

Limit is: 0 ≤ JCLOCK.")
  (:formats (integer JCLOCK))
  (:asserts (<= 0 JCLOCK)))

;; 4.116 FLUKA Printing

(def-input (fludbg "FLUDBG") (FFLUDB)
  (:documentation "FLUKA Printing.

Format = (A6, L), Default = F

FFLUDB : If .true. the two files ’DATnnnnnn.flout’ for additional information
on the parameters used by FLUKA and ’DATnnnnnn.flerr’ on possible FLUKA error
messages are written onto the output directory DSN (keyword DIRECT page 101).
In case of the NRREXT option these files get the names ’DATnnnnnnnnn.flout’ rsp.
’DATnnnnnnnnn.flerr’. If .false. in the LINUX option the two files are written
to ’SCRATCH’ i.e. they are written to a temporary file which at the end of the
program will be deleted. Without the LINUX option the files are opened directly
to the directory /dev/null.

This keyword is only available in the FLUKA options.")
  (:formats (boolean FFLUDB))
  (:asserts t))

;; 4.117 GHEISHA Debugging

(def-input (gheidb "GHEIDB") (GHEISDB)
  (:documentation "GHEISHA Debugging.

Format = (A6, L), Default = F

GHEISDB : If .true., in the DEBUG case also the GHEISHA routines produce debug
output. This output appears on the unit MDEBUG.

This keyword is only available in the GHEISHA option.")
  (:formats (boolean GHEISDB))
  (:asserts t))

;; 4.118 URQMD Debugging

(def-input (urqmd "URQMD") (FURQMD IUDEBUG)
  (:documentation "URQMD Debugging.

Format = (A5, L, I), Default = T, 0

FURQMD : If .true., the UrQMD routines are used for the low-energy hadronic
interactions. If .false., the program will stop.

IUDEBUG : If > 0, in the DEBUG case also the routines of UrQMD produce some
output. With increasing value of IUDEBUG this printout becomes more and more
detailed. This output appears on the unit MDEBUG.

Limit is: 0 ≤ IUDEBUG ≤ 3 .

This keyword is only available in the URQMD option.")
  (:formats (boolean FURQMD) (integer IUDEBUG))
  (:asserts (<= 0 IUDEBUG 3)))

;; 4.119 PYTHIA Printing

(def-input (pythia "PYTHIA") (IFLGPYW IFLGPYE)
  (:documentation "PYTHIA Printing.

Format = (A6, 2I), Default = 0, 0

IFLGPYW : If set > 0 the printing of PYTHIA warnings is enabled. IFLGPYW
gives the number of warnings after which PYTHIA becomes silent (see MSTU(26)
of Pythia manual [31]).

IFLGPYE : If set > 0 the printing of PYTHIA errors is enabled. IFLGPYE gives
the number of errors after which PYTHIA becomes silent (see MSTU(22) of
Pythia manual [31]).

Limits are: 0 ≤ IFLGPYW; 0 ≤ IFLGPYE.

This keyword is only available in the DPMJET, CHARM, or TAULEP options.")
  (:formats (integer IFLGPYW IFLGPYE))
  (:asserts (and (<= 0 IFLGPYW)
                 (<= 0 IFLGPYE))))

;; 4.120 Cherenkov Debugging

(def-input (cdebug "CDEBUG") (LCERDB)
  (:documentation "Cherenkov Debugging.

Format = (L), Default = F

LCERDB : If .false., Cherenkov debug output is disabled. If .true., the
Cherenkov routines produce debug output. This output appears on the unit
MDEBUG.

This keyword is only available in the CERENKOV option.")
  (:formats (boolean LCERDB))
  (:asserts t))

;; 4.121 Interaction Test Target Definition

(def-input (inttst "INTTST") (ITTAR MCM)
  (:documentation "Interaction Test Target Definition.

Format = (A6, 2I), Defaults = 0, 0

ITTAR : Defines the target for the interaction test option:
+ 1 = proton;
+ 2 = neutron;
+ 9 = Beryllium;
+ 12 = Carbon;
+ 14 = Nitrogen;
+ 16 = Oxygen;
+ 40 = Argon;
+ 99 = air.

MCM : Defines the reference system for which the interaction products are
plotted:
+ 1 = rest system of 1 target nucleon and 1 projectile nucleon;
+ 2 = laboratory system;
+ 3 = rest system of all secondary particles (but not the spectators).

This keyword is only available in the INTTEST option.")
  (:formats (integer ITTAR MCM))
  (:asserts t))

;; 4.122 Interaction Test Decay

(def-input (intdec "INTDEC") (LPI0 LETA LHYP LK0S)
  (:documentation "Interaction Test Decay.

Format = (A6, 4L), Defaults = T, T, T, T

LPI0 : If .true. the π◦ particles decay before gathering them in the
interaction test.

LETA : If .true. the η particles decay before gathering them in the
interaction test.

LHYP : If .true. all hyperons decay before gathering them in the
interaction test.

LK0S : If .true. the K◦s particles decay before gathering them in the
interaction test.

This keyword is only available in the INTTEST option.")
  (:formats (boolean LPI0 LETA LHYP LK0S))
  (:asserts t))

;; 4.123 Interaction Test Spectator Definition

(def-input (intspc "INTSPC") (LSPEC)
  (:documentation "Interaction Test Spectator Definition.

Format = (A6, L), Default = F

LSPEC : If .true. spectators are plotted, if .false. spectators are not
plotted in the interaction test.

This keyword is only available in the INTTEST option.")
  (:formats (boolean LSPEC))
  (:asserts t))

;; 4.124 Interaction Test Diffraction Flag

(def-input (difoff "DIFOFF") (NDIF)
  (:documentation "Interaction Test Diffraction Flag.

Format = (A6, I), Default = 0

NDIF : Allows to select diffractive or non-diffractive interactions only.
+ 0 = diffractive and non-diffractive interactions mixed;
+ 1 = non-diffractive interactions only;
+ 2 = diffractive interactions only. With the QGSJET01d interaction
  model NDIF = 2 cannot be selected. With the EPOS, NEXUS and QGSJET-II
  models only NDIF = 0 is possible.

This keyword is only available in the INTTEST option.")
  (:formats (integer NDIF))
  (:asserts t))

;; 4.125 Interaction Test Trigger Condition

(def-input (trigger "TRIGGER") (NTRIG)
  (:documentation "Interaction Test Trigger Condition.

Format = (A7, I), Default = 0

NTRIG : Allows to select various trigger conditions for comparison with
experimental data:
+ 0 = accepts all events;
+ 1 = accepts only events according to the UA5-experiment [74] trigger;
+ 2 = accepts only events according to the CDF-experiment [75] trigger.
+ 3 = accepts only events according to the P238-experiment [76] trigger.
+ NTRIG != 0 may be combined only with NDIF = 0 .

This keyword is only available in the INTTEST option.")
  (:formats (integer NTRIG))
  (:asserts t))

;; 4.126 Interaction Test Histogram Output

(def-input (histds "HISTDS") (HISTDS)
  (:documentation "Interaction Test Histogram Output.

Format = (A6, A120), Defaults = ’histo.corsika.inttest’

HISTDS : May be used to specify a name of the histogram output directory
and data file. Lower case characters of HISTDS are not converted to
capitals. Do not use capitals as the HBOOK routines use only lower case
characters. The data file name is extended by a string containing
information about projectile, target, energy, and the type of interaction
which has been selected. At the end of the data file name .hbook is appended
such that the total data file name would look like:

  histo.corsika.inttest.p0014t14e100e3.diffractive.hbook

for a proton projectile on nitrogen target with a lab energy of 100E3 GeV
including diffractive events. If you want to write into the directory from
where you are starting your CORSIKA run, you should give:

  HISTDS ./

or

  HISTDS ’ ’

or

  HISTDS " "

(a blank enclosed in apostrophes or quotation marks).

Please keep in mind that in FORTRAN an automatic expansion of UNIX names like
’$HOME’ is not possible, rather you should give the fully expanded name of the
directory ending with a / (slash) character.

Limit is: HISTDS must not begin with a ∼ (tilde) character.

This keyword is only available in the INTTEST option.")
  (:formats (string HISTDS))
  (:asserts (<= (length HISTDS) 120)))

;; 4.127 Plot Output

(def-input (plotsh "PLOTSH") (PLOTSH)
  (:documentation "Plot Output.

Format = (A6, L), Default = F

PLOTSH : If .true., the track start- and endpoints of the electromagnetic,
muonic, and hadronic component of the shower are given out separately and
may be used to plot the shower development.

This keyword is only available in the PLOTSH and PLOTSH2 option.")
  (:formats (boolean PLOTSH))
  (:asserts t))

;; 4.128 Plot Axes Definition

(def-input (plaxes "PLAXES") (X1 X2 Y1 Y2 Z1 Z2)
  (:documentation "Plot Axes Definition.

Format=(A6,6F), Defaults = -500000., 500000., -500000., 500000., 0., 3000000.

X1, X2 : They denote the X-axis range (in cm) to be plotted in the map.
Y1, Y2 : They denote the Y-axis range (in cm) to be plotted in the map.
Z1, Z2 : They denote the Z-axis range (in cm) to be plotted in the map.

The point of first interaction determines the zero point of the X and Y axes
(see Fig. 1 page 120). Depending on the choice of these parameters, the whole
shower may be visualized, or one can ’zoom in’ on interesting regions of the s
hower.

Limits are: X1 < X2, Y1 < Y2, Z1 < Z2 .

This keyword is only available in the PLOTSH2 option.")
  (:formats (float X1 X2 Y1 Y2 Z1 Z2))
  (:asserts (and (< X1 X2) (< Y1 Y2) (< Z1 Z2))))

;; 4.129 Plot Energy Cut Definition

(def-input (plcuts "PLCUTS") (ELCUTS1 ELCUTS2 ELCUTS3 ELCUTS4 TCUT FBOXCUT)
  (:documentation "Plot Energy Cut Definition.

Format=(A6,5F,L), Defaults 0.3, 0.3, 0.003, 0.003, 100000., F

ELCUTS(1...4) : ELCUTS denote the energy cuts in the same order as those for
the keyword ECUTS (hadrons, muons, electrons, photons) (see page 94).

TCUT : This is an upper bound on the time (in ns) passed since the first
interaction. If, at the end point of a track, the time is above TCUT, the
track is not plotted. This cut allows a visualization of the shower development.

FBOXCUT : This flag determines whether only track segments inside the
three-dimensional box given by the axis ranges should be plotted. If .true.,
all track segments whose start- and endpoints both fall outside this box are
not plotted.

This keyword is only available in the PLOTSH2 option.")
  (:formats (float ELCUTS1 ELCUTS2 ELCUTS3 ELCUTS4 TCUT)
            (boolean FBOXCUT))
  (:asserts t))

;; 4.130 End of Steering

(def-input (exit "EXIT") ()
  (:documentation "Format = (A4)
This keyword ends the keyword input.")
  (:formats nil)
  (:asserts t))
