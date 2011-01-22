;;; -*- mode: lisp; indent-tabs: nil -*-
(defsystem :info.read-eval-print.tw
  :serial t
  ;; add new files to this list:
  :components ((:file "tw-package")
               (:file "tw"))
  :depends-on (:quek
               :series
               :cl-oauth
               :cl-json
               :net-telent-date
               :trivial-shell))
