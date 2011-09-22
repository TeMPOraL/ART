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

;;; high-level interface functions
(defun reset-ART ())

(defun train-ART (training-set-id))

(defun run-art-on-input (input))


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