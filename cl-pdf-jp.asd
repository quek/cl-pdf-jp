;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :common-lisp-user)

(defpackage #:cl-pdf-jp-system
    (:use #:cl #:asdf))

(in-package #:cl-pdf-jp-system)

(defsystem :cl-pdf-jp
  :name "cl-pdf-jp"
  :licence "BSD like licence"
  :serial t
  :components ((:file "encodings")
	       (:file "ttu-font")
	       (:file "font"))
  :depends-on (:cl-pdf))
