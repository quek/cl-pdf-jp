;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

;;; Support for TrueTypeUnicode fonts

(in-package #:pdf)


(defclass UniJIS-UCS2-H (unicode-encoding)
  ((font-name-suffix :accessor font-name-suffix :initform "UniJIS-UCS2-H")
   (ordering :accessor ordering :initform "Japan1")
   (char-names :accessor char-names :initform #()
               :documentation "(defmethod initialize-instance :after ((font font) &key encoding &allow-other-keys) のためのダミースロット"))
  (:default-initargs
      :name "UniJIS-UCS2-H"
    :keyword-name :UniJIS-UCS2-H))

(defparameter *UniJIS-UCS2-H* (make-instance 'UniJIS-UCS2-H))

(defclass UniJIS-UCS2-HW-H (unicode-encoding)
  ((font-name-suffix :accessor font-name-suffix :initform "UniJIS-UCS2-HW-H")
   (ordering :accessor ordering :initform "Japan1")
   (char-names :accessor char-names :initform #()
               :documentation "(defmethod initialize-instance :after ((font font) &key encoding &allow-other-keys) のためのダミースロット"))
  (:default-initargs
      :name "UniJIS-UCS2-HW-H"
    :keyword-name :UniJIS-UCS2-HW-H))

(defparameter *UniJIS-UCS2-HW-H* (make-instance 'UniJIS-UCS2-HW-H))


(defclass jp-font-metrics (font-metrics)
  ((cid-widths :accessor cid-widths :initform #())
   (flags :accessor flags :initargs :flags)
   (stem-v :accessor stem-v :initargs :stem-v)))

(defmethod font-type ((fm jp-font-metrics))
  "Type0")


(defmethod font-descriptor ((fm jp-font-metrics) &key &allow-other-keys)
  (flet ((conv-dim (d) (round (* 1000 d))))
    (make-instance
     'indirect-object
     :content
     (make-instance
      'dictionary ; :obj-number 0 :no-link t
      :dict-values
      `(("/Type" . "/FontDescriptor")
	("/FontName"  . ,(add-/ (font-name fm)))
	("/Flags" . ,(flags fm))
	("/FontBBox" . ,(map 'vector #'conv-dim (font-bbox fm)))
	("/ItalicAngle" . ,(conv-dim (italic-angle fm)))
	("/Ascent" . ,(conv-dim (ascender fm)))
	("/Descent" . ,(conv-dim (descender fm)))
	("/CapHeight" . ,(conv-dim (cap-height fm)))
	("/XHeight" . ,(conv-dim (x-height fm)))
	("/StemV" . ,(stem-v fm)))))))


(defclass jp-cid-font ()
  ((base-font :accessor base-font :initarg :base-font)
   (descriptor :accessor descriptor :initarg :descriptor)
   (widths :accessor widths :initarg :widths)
   (encoding :accessor encoding :initarg :encoding)))

(defmethod make-dictionary ((font jp-cid-font) &key &allow-other-keys)
  (make-instance
   'dictionary
   :dict-values
   `(("/Type" . "/Font")
     ("/Subtype" . "/CIDFontType2")
     ("/BaseFont" . ,(add-/ (base-font font)))
     ("/CIDSystemInfo"
      . ,(make-instance
	  'dictionary
	  :dict-values
	  `(("/Registry" . ,(pdf-string "Adobe"))
	    ("/Ordering" . ,(pdf-string (ordering (get-encoding (encoding font)))))
	    ("/Supplement" . 6))))
     ("/FontDescriptor" . ,(descriptor font))
     ("/DW" . 1000)
     ("/W" . ,(widths font)))))

(defmethod make-dictionary ((fm jp-font-metrics) &key &allow-other-keys)
  (let* ((font-descriptor (font-descriptor fm :embed nil :errorp nil))
         (encoding (get-encoding (encoding-scheme fm)))
	 (cid-font (make-instance
		    'jp-cid-font
		    :base-font (font-name fm)
		    :descriptor font-descriptor
		    :widths (cid-widths fm)
                    :encoding (encoding-scheme fm)
                    )))
    (make-instance
     'dictionary
     :dict-values
     `(("/Type" . "/Font")
       ("/Subtype" . ,(add-/ (font-type fm)))
       ("/BaseFont" . ,(add-/ (concatenate 'string (font-name fm) "-" (font-name-suffix encoding))))
       ("/Encoding" . ,(if (standard-encoding encoding)
                           (add-/ (name encoding))
                           (find-encoding-object encoding)))
       ("/DescendantFonts"
	. ,(vector
	    (make-instance
	     'indirect-object
	     :content (make-dictionary cid-font))))))))

(defun initialize-jp-font-metrics ()
  (loop for (font-name font-bbox encoding cap-height ascender descender stem-v cid-widths flags) in
        '(("Ryumin-Light" #(-0.17 -0.331 1.024 0.903) :UniJIS-UCS2-HW-H 0.709 0.723 -0.241 69 #(231 632 500 8718 #(500 500)) 6)
          ("GothicBBB-Medium" #(-0.174 -0.268 1.001 0.944) :UniJIS-UCS2-HW-H 0.737 0.752  -0.271 99 #(231 632 500 8718 #(500 500)) 4))
        for font-metrics = (make-instance 'jp-font-metrics)
        do   (setf (font-name font-metrics) font-name
                   (full-name font-metrics) font-name
                   (encoding-scheme font-metrics) encoding
                   (font-bbox font-metrics) font-bbox
                   (stem-v font-metrics) stem-v
                   (cid-widths font-metrics) cid-widths
                   (flags font-metrics) flags)
             (setf (gethash (string-downcase font-name) pdf::*font-metrics*) font-metrics)))


(defclass jp-font (font)
  ())

(defmethod get-char-metrics ((char character) (font jp-font) (encoding UniJIS-UCS2-HW-H))
  (get-char-metrics (char-code char) font encoding))

(defmethod get-char-metrics ((code integer) (font jp-font) (encoding UniJIS-UCS2-HW-H))
  (make-instance 'char-metrics
                 :code code
                 :name (char-name (code-char code))
                 :index code
                 :width 1.0
                 :spacing 0.0
                 :right-italic-correction 0.0
                 :left-italic-correction 0.0
                 :bbox (font-bbox (font-metrics font))))

(defun get-font (&optional (name "helvetica") (encoding *default-encoding*))
  (setf name (string-downcase name))
  (let ((font-metrics (gethash name *font-metrics*)))
    (typecase font-metrics
      (ttu-font-metrics
         (setf encoding *unicode-encoding*))
      (jp-font-metrics
         (setf encoding *UniJIS-UCS2-HW-H*)))
    (let ((font (gethash (list name (get-encoding encoding)) *font-cache*)))
      (if font
          font
          (make-instance (typecase font-metrics
                           (jp-font-metrics 'jp-font)
                           (t 'font))
                         :name name :encoding encoding)))))

(defun load-fonts (&optional force)
  (when (or (not %fonts-loaded%) force)
    (dolist (font-dir *afm-files-directories*)
      (map nil 'read-afm-file (directory (merge-pathnames font-dir "*.afm"))))
    (initialize-jp-font-metrics)
    (clear-font-cache)
    (setf %fonts-loaded% t)))

(eval-when (:load-toplevel :execute)
  (load-fonts t))
