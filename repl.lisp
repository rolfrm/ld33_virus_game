(load "std2.lisp")

(defvar t mat4-eye)
(setf (member t m03) 10.0)
t
(defvar b (vec 1 2 3 1))
(dot t b)

(defcmacro print-ptr (expr)
  (if (type-is-pointer? (type-of expr))
      (expr (progn
	      (unexpr expr)
	      (printstr "Pointer!")))
      (cast null (ptr expr))))
(overload print print-ptr)



(defvar a :type i32)
(defvar tid :type i32)
(setf a 0)
;;(go-init (addrof a))

(defun test-ptr (void)
  (progn
    (print "hello!\n")
    (usleep 100000)
    (print "thread!\n")
    null))

(defun test-launch (void)
  (progn
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)))

(print "..\n")
(print "..\n")
(print "..\n")
(set-printer (quote printnl))
