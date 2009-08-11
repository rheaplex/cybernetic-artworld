;; aesthetic.lisp -  Generate, mutate, describe and evaluate aesthetics.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:aesthetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe (fun &key (probability 0.5) (default nil))
  "Call fun with aqrgs if random(0..1) is less than probability."
  (if (< (random 1.0) probability)
      (funcall fun)
    default))

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defun choose-randomly-deep (choices)
  "Choose one item from a list of lists."
  (choose-randomly (choose-randomly choices)))

(defun plus-or-minus-one ()
  "Choose either one or minus one."
  (choose-randomly '(1.0 -1.0)))

(defun random-range (from to)
  "Choose a random number from from to to."
  (+ (random (- to from))
     from))

(defmethod choose-one-of ((possibilities list))
  "Choose one or none of the option"
  (nth (random (length possibilities)) possibilities))

;;FIXME Force unique choices
(defmethod choose-n-of ((n integer) (choice-list list))
  "Choose n different entries from choice-list."
  (assert (<= n (length choice-list)))
  (let ((choices choice-list)
        (chosen '()))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
        (setf chosen (cons choice chosen))
        (setf choices (remove choice choices))))
    chosen))

(defun concatenate-string (&rest strings)
  "Concatenate a list of strings with an optional given prefix, separator and suffix."
  (let ((all (car strings)))
    (dolist (s (cdr strings))
      (when (not (equal s ""))
	(setf all (concatenate 'string all
			       (if (equal all "")
				   ""
				 " ") 
			       s))))
    all))

(defun pluralise (object plurality)
  "Make a word plural if necessary."
  (if (equal plurality "A")
      object
    (concatenate 'string object "s")))

(defun make-stretchy-vector (element-type)
  "Make an empty stretchy vector of element-type."
  (make-array 0
	      :element-type element-type
	      :fill-pointer 0
	      :adjustable t))

(defun make-char-stretchy-vector ()
  "Make an empty stretchy character vector."
  (make-stretchy-vector 'character))

(defun make-string-stretchy-vector ()
  "Make an empty stretchy string vector."
  (make-stretchy-vector 'string))

(defun vector-empty-p (vec)
  "Is the vector empty?"
  (= (length vec) 0))

(defmethod tokenize-string ((source string) (separators string))
  "Tokenize string to produce words separated by runs of separators."
  (let ((words (make-string-stretchy-vector))
	(chars (make-char-stretchy-vector)))
    (loop for char across source  
	  do (if (find char separators :test #'char-equal)
		 (when (not (vector-empty-p chars))
		   (vector-push-extend chars words)
		   (setf chars (make-char-stretchy-vector)))
	       (vector-push-extend char chars)))
    ;; If a word is at the end of the string without a terminator, add it
    (when (not (vector-empty-p chars))
      (vector-push-extend chars words))
    words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quantity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun amount ()
  "Generate a quantity description."
  (choose-randomly '("A" "A pair of" "Some" "Many")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter monochromes '("black" "grey" "white"))
(defparameter hues '("red" "orange" "yellow" "green" "blue" "purple"))
(defparameter colours '("magenta" "cyan" "brown" "pink" "turquoise" "mauve"))
(defparameter metals '("gold" "silver" "bronze" "platinum" "copper" 
		       "rust-coloured"))
(defparameter fabrics '("khaki" "cotton-coloured" "denim blue" 
			"suede-coloured"))
(defparameter naturals '("sky blue" "leaf green" "sea green" "sunset red"))
(defparameter artificials '("neon blue" "sunset yellow" "shocking pink" 
			    "non-repro blue" "blue-screen blue"))
(defparameter palettes (list monochromes hues colours metals fabrics naturals 
			     artificials))
(defparameter tone '("pale" "" "rich" "bright" "" "dark"))

(defun colour ()
  "Choose a flat colour from a palette"
  (choose-randomly-deep palettes))

(defun colour-description ()
  "Generate a colour description."
  (concatenate-string (choose-randomly tone) (colour) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun texture ()
  "Choose a texture."
  (choose-randomly '("halftoned" "crosshatched" "scumbled" "glazed" "sketchy" 
		     "smooth")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun appearance ()
  "Generate the appearance of a figure."
  (concatenate-string (maybe #'texture :default "")
		      (colour-description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter geometric '("circle" "triangle" "square" "pentagon" "hexagon" 
			  "octagon"))
(defparameter form '("organic shape" "spiky shape" "irregular shape"))
(defparameter abstract-shapes (list geometric form))
(defparameter abstract-shape-treatment '("" "" "outlined"))
(defparameter building '("house" "skyscraper"))
(defparameter transport '("car" "aeroplane" "ship"))
(defparameter animal '("bird" "cat" "dog" "horse"))
(defparameter generic-shapes (list building transport animal))
(defparameter generic-shape-treatments '("" "" "" "silhouetted" "outlined" 
					 "abstracted"))

(defun shape-size ()
  "Generate a size for the shape."
  (choose-randomly '("" "" "tiny" "small" "large" "massive")))

(defun shape-form (plural)
  "Generate a shape form description."
  (cond 
   ((> (random 1.0) 0.5)
    (concatenate-string (choose-randomly abstract-shape-treatment)
		 (pluralise (choose-randomly-deep abstract-shapes) plural)))
   (t
    (concatenate-string (choose-randomly generic-shape-treatments)
		 (pluralise (choose-randomly-deep generic-shapes) plural)))))

(defun shape ()
  (if (> (random 1.0) 0.5)
      (choose-randomly-deep abstract-shapes)
      (choose-randomly-deep generic-shapes)))

(defun shape-description (plural)
  "Generate a shape description."
  (concatenate-string (shape-size)
		      (appearance)
		      (shape-form plural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ground ()
  "Generate a simple ground description."
  (appearance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-description ()
  "Describe a single (set of) figure(s) on a single ground."
  (let ((plural (amount)))
    (concatenate-string plural (shape-description plural)
			"on a" (ground) "ground.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-aesthetic (aesthetic)
  "Gather the property names without their values."
  (loop for key being each hash-key of aesthetic
       collect key))

(defun aesthetic-opinions (aesthetic)
  "Sort the properties into likes and dislikes."
  (let ((likes '())
	(dislikes '()))
    (maphash #'(lambda (key val)
		 (if (>= val 0.0)
		     (push key likes)
		     (push key dislikes))) 
     aesthetic)
    (values likes dislikes)))

(defun describe-aesthetic ()
  "Describe the current likes and dislikes."
  ;;FIXME - Replace the final comma with an and or ampersand.
  (multiple-value-bind (likes dislikes) (aesthetic-opinions)
    (format nil "I like~{ ~A~^,~}. I dislike~{ ~A~^,~}." likes dislikes)))

(defun aesthetic-size (aesthetic)
  "Get the current size of *aesthetic*."
  (hash-table-count aesthetic))

(defun new-property ()
  "Choose a new property."
  (funcall (choose-one-of (list #'shape #'colour #'texture))))

(defun new-properties (count)
  "Choose n properties."
  (loop for i below count
       collect (new-property)))

(defun set-property (aesthetic prop)
  "Set valenced property."
    (setf (gethash prop aesthetic)
	  (plus-or-minus-one)))

(defun set-properties (aesthetic props)
  "Set valenced properties."
  (dolist (prop props)
    (set-property aesthetic prop))
  aesthetic)

(defparameter +min-properties+ 4)

(defparameter +max-properties+ 12)

(defun make-aesthetic ()
  "Generate an initial set of properties."
  (set-properties (make-hash-table :test 'equal)
		  (new-properties (random-range +min-properties+
						+max-properties+))))

(defparameter +max-properties-to-delete+ 2)
(defparameter +max-properties-to-mutate+ 2)
(defparameter +max-properties-to-add+ 2)

(defun delete-properties (aesthetic)
  "Delete 0+ properties, don't reduce properties below +min-properties+."
  ;;FIXME - Set the correct number here rather than checking with when
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
			     (list-aesthetic aesthetic)))
    (when (> (aesthetic-size aesthetic) +min-properties+) 
	       (remhash prop aesthetic)))
  aesthetic)

(defun mutate-properties (aesthetic)
  "Mutate zero or more properties."
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
			     (list-aesthetic aesthetic)))
    (setf (gethash prop aesthetic)
	  (- (gethash prop aesthetic))))
  aesthetic) 

(defun add-properties (aesthetic)
  "Add zero or more properties."
  (loop with remaining = (min (max +min-properties+ 
				   (random +max-properties-to-add+))
			      (- +max-properties+ (aesthetic-size aesthetic)))
	while (> remaining 0)
	do (let ((prop (new-property)))
	     (when (not (gethash prop aesthetic))
	       (set-property aesthetic prop)
	       (decf remaining))))
  aesthetic)

(defun update-aesthetic (aesthetic)
  "Update the aesthetic."
  (add-properties (mutate-properties(delete-properties aesthetic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Critique
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How to handle ambiguities? yellow vs sunset yellow?
;; Sort keys by length
;; Find longest first
;; Remove from string
;; But what if that creates bogus sequences? Break into substrings?
;; Just always hypenate multi-word sequences?

#|(defun score-artwork (artwork aesthetic)
  "Process the string description of the artwork to generate a score."
  (let ((clean-artwork (string-downcase artwork)))
    (loop for prop being each hash-key of *aesthetic*
       if (search prop clean-artwork)
       sum (gethash prop *aesthetic* 0.0))))

(defun evaluate-artwork (artwork)
  "Set range to max of +/-1, probably less."
  (/ (score-artwork artwork)
     (aesthetic-size)))

(defun describe-artwork-evaluation (score)
  "Turn the -1.0..1.0 score into a verbal description."
  (cond
    ((< score -0.6) "terrible")
    ((< score -0.1) "bad")
    ((< score 0.1) "ok")
    ((< score 0.6) "good")
    (t "excellent")))
|#

(defun positive-aesthetic-value (description aesthetic)
  "Sum the positive value of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
	(positive-value))
    (maphash #'(lambda (key val) 
		 (when (and (search key clean-artwork)
			    (> val 0.0))
		   (incf positive-value val)))
	     aesthetic)
    positive-value))

(defun negative-aesthetic-value (description aesthetic)
  "Sum the negative value of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
	(negative-value))
    (maphash #'(lambda (key val) 
		 (when (and (search key clean-artwork)
			    (> val 0.0))
		   (decf negative-value val)))
	     aesthetic)
    negative-value))

(defun total-aesthetic-values (description aesthetic)
  "Sum the values of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
	(+total 0.0)
	(-total 0.0))
    (maphash #'(lambda (key val) 
		 (when (search key clean-artwork)
		   (cond 
		     ((> val 0.0)
		      (incf +total val))
		     ((< val 0.0)
		      (decf -total val)))))
	     aesthetic)
    (values +total -total)))

(defun describe-aesthetic-value (negative positive)
  "Describe the value of the work allowing for pos & neg points."
  (cond 
    ((and (>= positive 2) (= negative 0)) "a masterpiece")
    ((and (>= negative 2) (= positive 0)) "a failure")
    ((> positive negative) "good")
    ((> negative positive) "bad")
    ((and (= negative positive) (>= negative 2)) "extremely mixed")
    ((= negative positive 1) "mixed")
    (t "uninteresting")))

(defun critique-artwork (description aesthetic identifier)
  "Verbally critique the artwork."
  (multiple-value-bind (+val -val) 
			(total-aesthetic-values description aesthetic)
    (format nil 
	    "~a ~a is ~a."
	    (choose-one-of '("I think that" "In my opinion," "I would say that"
			     "Aesthetically speaking," ))
	    identifier
	    (describe-aesthetic-value +val -val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similarity, strength of resemblance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun euclidean-aesthetic-similarity (aesthetic-list aesthetic-hash)
  "Get the normalized euclidean distance between two aesthetics"
  (let ((total 0.0)) 
    (dolist (key aesthetic-list) 
      (let ((aesthetic-weight (gethash key aesthetic-hash nil)))
	(when aesthetic-weight
	  ;; Assume the presence of a property to be the origin (0.0),
	  ;;  and so the distance is the aesthetic's value for that property
	  ;;  (-1.0 or +1.0)
	  (incf total (expt aesthetic-weight 2)))))
    (/ 1.0 (+ 1.0 (sqrt total)))))