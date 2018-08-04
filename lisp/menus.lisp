(in-package :ab-lisp)

;; user menu
(ccl::add-PWGL-user-menu 
 '(:menu-component
   ("ABLib"
    (("Envelopes" (env-scale
                   sample-values
                   resample-x
                   split-xy
                   sample-env))
     ("Lists" (flat-last
               list-intervals))
     ("Math" 
      ("Average" (average
                  mean-deviation
                  arithmetic-mean
                  harmonic-mean
                  hoelder-mean
                  power-mean
                  )))
     ("Pitchclass" (pc?
                    oct?
                    same-pitchclass-chord
                    same-pc
                    pc-instances
                    pc-simple-mult))
     
     ("Quantisation" (index-no
                      quantize
                      quantize-list
                      quant-up
                      quant-down
                      which-quant
                      round-up
                      round-down
                      round-to
                      prev-measure
                      next-measure
                      closest-measure))
     ("Random" (rand))
     ("Utilities" (switch-pos-neg
                   rev-switch
                   between?
		   iota
		   half
		   median
                   semi-to-srate
                   sec-to-min
                   midi-to-integer
                   integer-to-midi
                   measure-to-quarter
                   quarter-to-measure
                   index-no
		   )))
    )
   )
 )
