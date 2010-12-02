(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :series)
  (require :cl-oauth)
  (require :drakma)
  (require :cl-json)
  (require :quek)
  (require :net-telent-date))

;; 対 drakma 用おまじない
(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)

(defpackage :repl-twitter-client
  (:use :cl :series :quek)
  (:shadowing-import-from :series let let* multiple-value-bind funcall defun)
  (:export #:tweet
           #:reply
           #:timeline))

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

(defun update (message &key reply-to)
  (when message
    (json:decode-json-from-string
     (oauth:access-protected-resource
      "http://api.twitter.com/1/statuses/update.json"
      *access-token*
      :request-method :post
      :user-parameters `(("status" . ,#"""#,message #'求職中""")
                         ,@(when reply-to `(("in_reply_to_status_id" . ,(princ-to-string reply-to)))))))))

(defun tweet ()
  (let ((message (query-message)))
    (update message)
    message))

(defun reply (in-reply-to-status-id)
  (let ((message (query-message)))
    (update message :reply-to in-reply-to-status-id)
    message))

(defun created-at-time (x)
  (multiple-value-bind (s m h) (decode-universal-time  (net.telent.date:parse-time x))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun print-tweet (json-string)
  (ignore-errors
    (json:with-decoder-simple-clos-semantics
      (let ((json:*json-symbols-package* :repl-twitter-client))
        (let ((x (json:decode-json-from-string json-string)))
          (with-slots (text user id created--at) x
            (with-slots (name screen--name) user
              (format
               *query-io*
               #"""~&  ~%#,screen--name (#,name,) #,(created-at-time created--at) #,id,~&#,text,~%"""))))))))

(defun timeline ()
  (bordeaux-threads:make-thread
   (^ with-open-stream (in (oauth:access-protected-resource
                            "https://userstream.twitter.com/2/user.json"
                            *access-token*
                            :drakma-args '(:want-stream t)))
      (loop for line = (read-line in nil)
            while line
            do (print-tweet line)))
   :name "https://userstream.twitter.com/2/user.json"))


#|
Emacs での画像表示法方
(insert-image (create-image (expand-file-name "archive/1.jpg") 'jpeg nil))
(add-text-properties (match-beginning 0) (match-end 0)
				       (list 'display (create-image file)))
|#
