;;; local_packages/random-theme/random-theme.el -*- lexical-binding: t; -*-

;; Colors to use
;; primary: (blues)
;; - for function names and stuff
;; secondary: (yellow)
;; - warnings
;; tertiary: (whites)
;; - basic text, variable names, the large content sections
;;
;; how to modify faces?
;; let the base one be the most vibrant example of it
;; -- some for differentiating different importance items --
;; PRIMARY
;; base
;; 1 darker (dark blue from blue)
;; 1 faint (for comments)
;; ~ 1 bit fainter (for font-lock-comment-delimiter-face)
;;
;; TERTIARY
;; base (for negation char, )
;; - 1 slightly fainter for most default text


;; Faces to control:
;;
;; Font Lock Suite
;;
;;

;; TODO
;; - start by just changing a bunch of faces based on 3 provided colors
;; - add ability to specify groups of faces to use certain colors
;; - add func to modify the base colors lsighyly on different colors


;; ===== Base =====
;; (219°, 67%, 93%) #4e85ed : primary
;; (234°, 67%, 93%) #4e5eed : primary darker
;; (218°, 21%, 73%) #93a1ba : faint, for readable docs
;; (219°, 51%, 69%) #5675b0 : faint, font-lock comment face
;; (206°, 85%, 66%) #5eb2f2 : highlight
;;
;; ===== Tertiary =====
;; (43°, 47%, 99%) #FDDC87 : warning
;;
;; ===== Secondary =====
;; (0°, 0%, 87%) #dddddd : base
;; (0°, 0%, 67%) #AAAAAA : base lighter
;;

(cl-defstruct (random-theme-face-groups
               (:constructor random-theme-face-groups-create)
               (:copier copy-random-theme-face-groups))
  "Struct describing which faces should take on which colors"
  (base-faces '() :documentation
                    "Faces that take on the color of the unmodified base color.")
  (base-darker-faces '() :documentation
                    "Faces that take on the color of the base color tinted darker.")
  (base-faces-faint '() :documentation
                    "Faces that take on the color of the base color tinted faint.")
  (base-faces-fainter '() :documentation
                    "Faces that take on the color of the base color tinted fainter.")
  (base-faces-light '() :documentation
                    "Faces that take on the color of the base color tinted lighter.")
  (tertiary-faces '() :documentation
                    "Faces that take on the color of the tertiary color.")
  (secondary-faces '() :documentation
                    "Faces that take on the color of the secondary.")
  (secondary-faces-faint '() :documentation
                    "Faces that take on the color of the secondary color tinted faint.")
  (secondary-faces-mid '() :documentation
                    "Faces that take on the color of the secondary between faint and normal."))




(defun random-theme--one-bounded-add (base add)
  "Add BASE to ADD, keeping result within 0.0..1.0."
  (let ((res (+ base add)))
    (if (> res 1.0)
        1.0
      (if (< res 0.0) 0 res))))

(defun random-theme--modify-color (color-hex-string hue-diff sat-diff lum-diff)
  "Returns hex string of COLOR-HEX-STRING by adding HUE-DIFF, SAT-DIFF, or LUM-DIFF to its components."
  (apply #'color-rgb-to-hex
   (apply #'color-hsl-to-rgb
   (pcase (apply #'color-rgb-to-hsl (color-name-to-rgb color-hex-string))
    (`(,hue ,sat, lum) `(,(random-theme--one-bounded-add hue hue-diff)
                         ,(random-theme--one-bounded-add sat sat-diff)
                         ,(random-theme--one-bounded-add lum lum-diff)))))))


(defun random-theme-set-faces-colors (primary secondary tertiary face-groups)
  "Changes faces using the 3 colors."
  (let
      ((primary-color primary)
       (primary-darker (random-theme--modify-color primary 0.05 0 0))
       (primary-faint (random-theme--modify-color primary -0.01 -0.6 -0.03))
       (primary-fainter (random-theme--modify-color primary 0.01 -0.46 -0.09))
       (primary-light (random-theme--modify-color primary -0.02 0.85 0.66))
       (tertiary-color tertiary)
       (secondary-color secondary)
       (secondary-faint (random-theme--modify-color secondary 0 0 -0.2))
       (secondary-mid (random-theme--modify-color secondary 0 0 -0.07))
       )
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground primary-color))
     (random-theme-face-groups-base-faces face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground primary-darker))
     (random-theme-face-groups-base-darker-faces face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground primary-faint))
     (random-theme-face-groups-base-faces-faint face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground primary-fainter))
     (random-theme-face-groups-base-faces-fainter face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground primary-light))
     (random-theme-face-groups-base-faces-light face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground tertiary-color))
     (random-theme-face-groups-tertiary-faces face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground secondary-color))
     (random-theme-face-groups-secondary-faces face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground secondary-faint))
     (random-theme-face-groups-secondary-faces-faint face-groups))
    (mapc
     (lambda (face) (set-face-attribute face nil :foreground secondary-mid))
     (random-theme-face-groups-secondary-faces-mid face-groups))
    ))


(random-theme-set-faces-colors "#ed854e" "#8AAAAA" "#aa00aa"  face-groups)


(random-theme-set-faces-colors "#4e85ed" "#AAAAAA" "#ffff00"  face-groups)


(defun random-theme-set-theme ()
  (interactive)
  (let* ((base-color (apply #'color-rgb-to-hex
                            (apply #'color-hsl-to-rgb
                                   `(
                                     ,(/ (random 360) 360.0) 0.82 0.62))))
         (secondary "#AAAAAA")
         (tertiary (color-complement-hex base-color)))
    (random-theme-set-faces-colors base-color secondary tertiary face-groups)))

;;
;; ====================
;;

(setq face-groups (random-theme-face-groups-create
                   :base-faces '(font-lock-function-name-face highlight-quoted-quote
                   font-lock-preprocessor-face font-lock-type-face )
                   :base-darker-faces '(font-lock-constant-face highlight-quoted-symbol)
                   :base-faces-faint '(font-lock-doc-face)
                   :base-faces-fainter '(font-lock-comment-face font-lock-comment-delimiter-face)
                   :base-faces-light '(highlight)
                   :tertiary-faces  '(warning)
                   :secondary-faces '(font-lock-negation-char-face
                                     font-lock-regexp-grouping-backslash
                                     font-lock-regexp-grouping-construct
                                     highlight-numbers-number)
                   :secondary-faces-faint '(default font-lock-string-face font-lock-keyword-face)
                   :secondary-faces-mid '(font-lock-variable-name-face)))

(random-theme-set-theme)