(in-package :ASDF)

(defsystem "ab-lisp"

  :description "Lisp Library"
  :long-description ""
  :version "0.2"
  :author "Achim Bornhoeft"
  :licence "http://creativecommons.org/licenses/by-nc-sa/3.0/"
  :maintainer "Achim Bornhoeft"

  ;; :serial t means that each component is only compiled, when the
  ;; predecessors are already loaded
:serial t
  :components
  (	(:file "lisp/package") ; package definition
  	(:file "lisp/boxes") ; define your boxes and other lisp functions 	        
   	(:file "lisp/envelopes")
	(:file "lisp/lists")
	(:file "lisp/math")	
   	(:file "lisp/pitchclass")
	(:file "lisp/quantisation")
	(:file "lisp/random")
	(:file "lisp/utilities")
	(:file "lisp/menus") ; entries in the popup-menu (right-click)
	))
