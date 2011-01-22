;;; -*- mode: lisp; indent-tabs: nil -*-
(defsystem :info.read-eval-print.clim-tw
  :serial t
  ;; add new files to this list:
  :components ((:file "clim-tw-package")
               (:file "clim-tw"))
  :depends-on (:quek
               :series
               :cl-oauth
               :cl-json
               :net-telent-date
               :trivial-shell))
