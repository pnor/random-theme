;;; local_packages/random-theme/random-theme.el -*- lexical-binding: t; -*-

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
       (secondary-mid (random-theme--modify-color secondary 0 0 -0.07)))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground primary-color)))
     (random-theme-face-groups-base-faces face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground primary-darker)))
     (random-theme-face-groups-base-darker-faces face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground primary-faint)))
     (random-theme-face-groups-base-faces-faint face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground primary-fainter)))
     (random-theme-face-groups-base-faces-fainter face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground primary-light)))
     (random-theme-face-groups-base-faces-light face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground tertiary-color)))
     (random-theme-face-groups-tertiary-faces face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground secondary-color)))
     (random-theme-face-groups-secondary-faces face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground secondary-faint)))
     (random-theme-face-groups-secondary-faces-faint face-groups))
    (mapc
     (lambda (face) (if (facep face) (set-face-attribute face nil :foreground secondary-mid)))
     (random-theme-face-groups-secondary-faces-mid face-groups))))

(defun random-theme--random-in-range (low high)
  "Generate random number in range [LOW..HIGH).
LOW and HIGH should be 0..1"
  (let* ((high-scaled (* high 100))
         (low-scaled (* low 100))
         (random-base (random (round (- high-scaled low-scaled))))
         (result (/ (+ random-base low-scaled) 100.0)))
    result))

;;;### autoload
(defun random-theme-set-theme ()
  (interactive)
  (let* ((base-color (apply #'color-rgb-to-hex
                            (apply #'color-hsl-to-rgb
                                   `(,(/ (random 360) 360.0)
                                     ,(random-theme--random-in-range 0.6 1.0)
                                     ,(random-theme--random-in-range 0.6 1.0)))))
         (secondary "#DDDDDD")
         (tertiary (color-complement-hex base-color)))
    (random-theme-set-faces-colors base-color secondary tertiary random-theme-face-groups)))

;;
;; ====================
;;

(defvar random-theme-face-groups (random-theme-face-groups-create
                   :base-faces '(font-lock-function-name-face highlight-quoted-quote
                   font-lock-preprocessor-face font-lock-type-face link)
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

(provide 'random-theme)
