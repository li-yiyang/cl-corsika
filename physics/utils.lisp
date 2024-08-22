(in-package :cl-corsika/physics)

(defun muon? (particle-name)
  "Test if a `particle-name' is muon particle.
Return `t' if is, otherwise `nil'. "
  (and (atom particle-name)
       (or (eq particle-name :mu+)
	   (eq particle-name :mu-)
	   (eq particle-name :mu+-add-info)
	   (eq particle-name :mu--add-info))))

(defun muon-add-info? (particle-name)
  "Test if `particle-name' is muon with add info.
Return `t' if is, otherwise `nil'. "
  (and (atom particle-name)
       (or (eq particle-name :mu+-add-info)
	   (eq particle-name :mu--add-info))))

(defun gamma? (particle-name)
  "Test if a `particle-name' is gamma.
Return `t' if is, otherwise `nil'. "
  (and (atom particle-name)
       (eq particle-name :gamma)))

(defun electron? (particle-name)
  "Test if a `particle-name' is electron.
Return `t' if is, otherwise `nil'. "
  (and (atom particle-name)
       (eq particle-name :e+)
       (eq particle-name :e-)))
