(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :series)
  (require :cl-oauth)
  (require :drakma)
  (require :cl-json))

;; 対 drakma 用おまじない
(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)

(defpackage :repl-twitter-client
  (:use :cl :series)
  (:shadowing-import-from :series let let* multiple-value-bind funcall defun)
  (:export #:home-timeline
           #:tweet))

(in-package :repl-twitter-client)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (series::install :pkg :repl-twitter-client :implicit-map t))


(defun query-message ()
  (string-right-trim
   #(#\Space #\Cr #\Lf #\Tab)
   (with-output-to-string (out)
     (loop for line = (read-line *terminal-io*)
           until (string= "." line)
           if (string= "\\q" line)
             do (return-from query-message nil)
           do (write-line line out)))))

(macrolet ((m ()
             (let ((sec (collect-first (scan-file "~/.twitter-oauth.lisp"))))
               `(defparameter *access-token*
                  (oauth:make-access-token :consumer (oauth:make-consumer-token
                                                      :key ,(getf sec :consumer-key)
                                                      :secret ,(getf sec :consumer-secret))
                                           :key ,(getf sec :access-key)
                                           :secret ,(getf sec :access-secret))))))
  (m))

(defun home-timeline ()
  (json:decode-json-from-string
   (oauth:access-protected-resource
    "http://api.twitter.com/1/statuses/home_timeline.json"
    *access-token*)))

(defun tweet (&optional (message (query-message)))
  (when message
    (json:decode-json-from-string
     (oauth:access-protected-resource
      "http://api.twitter.com/1/statuses/update.json"
      *access-token*
      :request-method :post
      :user-parameters `(("status" . ,message))))))
