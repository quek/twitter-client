;;; -*- mode: lisp; indent-tabs: nil -*-
(defsystem :info.read-eval-print.clim-tw
  :serial t
  ;; add new files to this list:
  :components ((:file "clim-tw-package")
               (:file "clim-tw"))
  :depends-on (:info.read-eval-print.tw
               :mcclim
               :mcclim-freetype
               :mcclim-gif-bitmaps
               :mcclim-jpeg-bitmaps
               :mcclim-png-bitmaps
               :mcclim-tiff-bitmaps
               :mcclim-uim))
