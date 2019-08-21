;;; tp.el --- Text property convenience library      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/tp.el
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library is to ease working with text properties in buffers.

;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'cl-lib))

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defalias 'tp-get 'get-text-property)

(cl-defun tp-next (&optional property &key limit value non-nil object
                             (start (point)) (testfn #'eq))
  "Return position of next change in PROPERTY, optionally with VALUE, up to LIMIT.
If NON-NIL, require that the value be any non-nil value.  Search
forward from START or current point, in OBJECT or current buffer.
If VALUE, compare with TESTFN.  Position returned is immediately
before PROPERTY has the desired value.  That is, this code would
return the specified VALUE:

  (tp-get (tp-next ...) PROPERTY OBJECT)"
  (pcase property
    ('nil  ;; Any property change.
     (next-property-change start object limit))
    (_   ;; Specific property.
     (pcase value
       ('nil ;; No specified value.
        (pcase non-nil
          ('nil ;; Specific property, any value.
           (next-single-property-change start property object limit))
          (_ ;; Specific property, any non-nil value.
           (cl-loop with pos = start
                    do (setf pos (next-single-property-change pos property object limit))
                    when pos
                    for value-at = (get-text-property pos property object)
                    when value-at return pos
                    while pos))))
       (_ ;; Specific value.
        (cl-loop with pos = start
                 do (setf pos (next-single-property-change pos property object limit))
                 when pos
                 for value-at = (get-text-property pos property object)
                 when (funcall testfn value value-at)
                 return pos
                 while pos))))))

(cl-defun tp-prev (&optional property &key limit value non-nil object
                             (start (point)) (testfn #'eq))
  "Return position of previous change in PROPERTY, optionally with VALUE, up to LIMIT.
If NON-NIL, require that the value be any non-nil value.  Search
forward from START or current point, in OBJECT or current buffer.
If VALUE, compare with TESTFN.  Position returned is immediately
after PROPERTY has the desired value.  That is, this code would
return the specified VALUE:

  (tp-get (1- (tp-prev ...)) PROPERTY OBJECT)"
  (pcase property
    ('nil ;; Any property change.
     (previous-property-change start object limit))
    (_  ;; Specific property.
     (pcase value
       ('nil ;; No specified value.
        (pcase non-nil
          ('nil ;; Specific property, any value.
           (previous-single-property-change pos property object limit))
          (_ ;; Specific property, any non-nil value.
           (cl-loop with pos = start
                    do (setf pos (previous-single-property-change pos property object limit))
                    when pos
                    for value-at = (get-text-property (max (1- pos) 0) property object)
                    when value-at return pos
                    while pos))))
       (_ ;; Specific value.
        (cl-loop with pos = start
                 do (setf pos (previous-single-property-change pos property object limit))
                 when pos
                 for value-at = (get-text-property (max (1- pos) 0) property object)
                 when (funcall testfn value value-at)
                 return pos
                 while pos))))))

;;;; Footer

(provide 'tp)

;;; tp.el ends here
