;;; Quicklisp here.
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load libraries
(ql:quickload 'hunchentoot)
(ql:quickload 'ht-simple-ajax)
(ql:quickload 'cl-who)
(ql:quickload 'parenscript)

(ql:quickload 'vecto)

(defpackage :trc.art
  (:use :common-lisp
                :hunchentoot
                :ht-simple-ajax
                :cl-who
                :parenscript))

(in-package :trc.art)

;;; ART network
(defconstant +input-size+ 64 "Input vector size.")

(defvar *top-down-weights* nil "Weights between top and bottom layer, going up.")
(defvar *bottom-up-weights* nil "Weights between top and bottom layer, going down.")

(defvar *clusters* 0 "Clusters found so far.")

;;; math tools
(defun dot-product (v1 v2)
  (reduce #'+ (map 'vector (lambda (x y) (* x y)) v1 v2)))

;;; other tools
(defun tag-list-with-indices (list)
  (loop for i from 0 to (length list)
            for elem in list
           collecting (cons i elem)))

;;; lower-level functions
(defun init-ART ()
  (setf *clusters* 1
        *top-down-weights* (make-array 1
                                       :initial-element (make-array +input-size+)
                                       :fill-pointer 1
                                       :adjustable t)
        *bottom-up-weights* (make-array 1
                                        :initial-element (make-array +input-size+)
                                        :fill-pointer 1
                                        :adjustable t)))

(defun save-ART-match (match)
  ;; FIXME FIXME don't use.
  (incf *clusters*)
  (vector-push-extend *top-down-weights* match)
  (vector-push-extend *bottom-up-weights* match)) ;; FIXME copy? and find out how/what by to extend?

(defun should-reset-p (activation-sum input-sum vigilance)
  "Predicate that tells us whether we should have RESET signal - that is, whether given neuron should be rejected."
  (< (/ activation-sum input-sum) vigilance))

(defun select-best-matching-neuron (neurons input)
  "Select the best matching neuron. If none found, make one. Returns
  ID of a top-layer neuron."
  ;; TODO
  ;; NOTE Adding neuron will require figuring out the weights.
  42)

(defun update-weigths (best-neuron)
  "Update weights for given neuron."
  'fixme)


;;; high-level interface functions
(defun reset-ART ()
  (init-ART))

(defun train-ART (training-set-id))
;;; NOTE FIXME there are some places I forgot to make sure I copy
;;; arrays. Look out for those places.

(defun run-art-on-input (input)
  "Run ART on input; return the cluster ID to which the input was classified."
  ;; ART magic algorithm
  (let* ((input-sum (reduce #'+ input))
         ;; top-layer-reaction aka. f2.
         (top-layer-reaction (map 'vector (lambda (weight-set)
                                            (dot-product weight-set input))
                                  *bottom-up-weights*0))
         ;; Sort the neurons by top-layer-reaction (descending),
         ;; attaching an indice to each value so that we can also have
         ;; a corresponding information about which neuron is which.
         (neurons (sort (tag-list-with-indices (coerce top-layer-reaction 'list))
                        #'>
                        :key #'cdr))
         (best-matching (select-best-matching-neuron neurons input)))
    
    ;; Now that we have the above, we need to iterate over `neurons',
    ;; checking each one if it crosses the activation threshold; if it
    ;; does, we update weights; if it doesn't, we add a new one (and
    ;; set up weights).
    ;; (loop for neuron in neurons
    ;;   for activation-sum = (dot-product input (elt *top-down-weights* (car neuron)))
    ;; foo
    
    ;; if (not (should-reset-p activation-sum input-sum)) break
    ;; loop. I need to detect if we run out of entries in the
    ;; list - if so, we need to add a new cluster.
    (update-weigths best-matching)      ; <-- TODO
    best-matching))
  
  ;; preparations

  ;; ART magic concurrency stuff

  ;; 
  )


;;; HTTP management code
(defvar *web-interface* nil)

(defun run-art (port)
  (start (setf *web-interface* (make-instance 'acceptor :port port))))

(defun stop-art ()
  (stop *web-interface*))

;;; Web Interface

(define-easy-handler (art :uri "/art") ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
        (:html :xmlns "http://www.w3.org/1999/xhtml"
                   (:head (:title "ART"))

                   (:body
                        (:h1 "ART networkz ftw!")
                        (:div "Some dummy text.")))))