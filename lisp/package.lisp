(defpackage :ab-lisp
  (:use :cl))
(in-package :ab-lisp)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (import '(ccl::PWGLdef) :ab-lisp))
