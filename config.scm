(sphere: "srfi")
(dependencies:
 (srfi-64
  (include
   (srfi: srfi-64-macros)))
 (srfi-78
  (include
   (srfi: srfi-78-macros)))
 ((srfi: srfi-64 version: (debug))
  (include
   (srfi: srfi-64-macros version: (debug))))
 ((srfi: srfi-78 version: (debug))
  (include
   (srfi: srfi-78-macros version: (debug)))))
