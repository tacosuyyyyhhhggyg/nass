;;; These are not actually being used right now.
;;;
;;; All of these define a kind of "generic" type based dispatch that
;;; behaves similar to *PRINT-PPRINT-DISPATCH*.

(defstruct dispatch-entry
  "Contains required TYPE and PRIORITY that FUNCTION is called.

Meant for use in a hashtable with a list of these matching a particular
function name so that dispatch based on _types_ can happen. By default we
just do `typep' on the function's arguments, but for more specialized
checking the function TEST-FUNCTION can be modified to return t when
FUNCTION should be called.

TEST-FUNCTION is a function of one argument, that of the prospective
arguments to check types on that returns t or nil if those arguements are
of a valid type."
  (type (assert-value-supplied))
  (test-function nil :type (or null nutils:function-designator))
  (priority 0 :type fixnum)
  (function (assert-value-supplied) :type nutils:function-designator))

(defun dispatch-entry-equal (entry1 entry2)
  "Compare two entries based on required type alone."
  (declare (dispatch-entry entry1 entry2))
  (equal (dispatch-entry-type entry1)
         (dispatch-entry-type entry2)))


(defun sortf-type-dispatch-entries (name dispatch-table)
  "Sort functions under NAME in DISPATCH-TABLE by priority."
  (setf (gethash name dispatch-table)
        (sort (gethash name dispatch-table)
              #'> :key #'dispatch-entry-priority)))

(defun tuncall (name table &rest args)
  (declare (optimize (speed 3) (safety 0))
           (dynamic-extent args))
  (apply (the function (loop for entry in (gethash name table)
                          when (typep args (dispatch-entry-type entry))
                          return (dispatch-entry-function entry)))
         args))
