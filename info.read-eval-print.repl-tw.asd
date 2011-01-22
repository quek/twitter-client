;;; -*- mode: lisp; indent-tabs: nil -*-
(defsystem :info.read-eval-print.repl-tw
  :serial t
  ;; add new files to this list:
  :components ((:file "repl-tw-package")
               (:file "repl-tw"))
  :depends-on (:info.read-eval-print.tw))


