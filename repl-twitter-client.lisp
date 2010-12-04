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
  (:export #:start
           #:tweet
           #:reply))

(in-package :repl-twitter-client)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (series::install :pkg :repl-twitter-client :implicit-map t))

(defparameter *profile-image-directory* (ensure-directories-exist "/tmp/repl-twitter-client-images/"))

(defun query-message ()
  (collect-fn 'string (constantly "") (^ format nil "~a~&~a" _a _b)
              (until-if (^ string= "." (if (string= "\\q" _)
                                           (return-from query-message nil)
                                           _))
                        (scan-stream *terminal-io* #'read-line))))

(macrolet ((m ()
             (let ((sec (collect-first (scan-file "~/.twitter-oauth.lisp"))))
               `(defparameter *access-token*
                  (oauth:make-access-token :consumer (oauth:make-consumer-token
                                                      :key ,(getf sec :consumer-key)
                                                      :secret ,(getf sec :consumer-secret))
                                           :key ,(getf sec :access-key)
                                           :secret ,(getf sec :access-secret))))))
  (m))

(defun update (message &key reply-to)
  (when message
    (json:decode-json-from-string
     (oauth:access-protected-resource
      "http://api.twitter.com/1/statuses/update.json"
      *access-token*
      :request-method :post
      :user-parameters `(("status" . ,#"""#,message `',求職中""")
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
            (with-slots (name screen--name profile--image--url (user-id id)) user
              (let ((path (get-profile-image id profile--image--url)))
                (format
                 *query-io*
                  #"""~&#,path #,screen--name (#,name,) #,(created-at-time created--at) #,id,~&#,text,~%""")))))))))

(defvar *timeline-process* nil)

(defun timeline ()
  (setf *timeline-process*
        (bordeaux-threads:make-thread
         (^ with-open-stream (in (oauth:access-protected-resource
                                  "https://userstream.twitter.com/2/user.json"
                                  *access-token*
                                  :drakma-args '(:want-stream t)))
            (collect-ignore (print-tweet (scan-stream in #'read-line))))
         :name "https://userstream.twitter.com/2/user.json")))


(defun local-profile-image-path (user-id profile-image-url)
  (merge-pathnames (file-namestring (puri:uri-path (puri:uri profile-image-url)))
                   #"""#,*profile-image-directory*,/#,user-id,/"""))

(defun %get-profile-image (image-url local-path)
  (unless (probe-file local-path)
    (ensure-directories-exist local-path)
    (with-open-file (out local-path :direction :output :element-type '(unsigned-byte 8))
      (write-sequence (drakma:http-request image-url
                                           :external-format-out :utf-8
                                           :external-format-in :utf-8)
                      out))))

(defun refresh-repl ()
  (sleep 0.1)
  (swank::with-connection ((swank::default-connection))
          (swank::eval-in-emacs '(save-current-buffer
                                  (set-buffer (get-buffer-create "*slime-repl sbcl*"))
                                  (save-excursion
                                    (iimage-mode 1))))))

(defvar *profile-image-process* nil)

(defun start-profile-image-process ()
  (setf *profile-image-process*
        (spawn (loop
                 (receive ()
                   ((profile-image-url local-path)
                    (%get-profile-image profile-image-url local-path)
                    (refresh-repl)))))))

(defun get-profile-image (user-id profile-image-url)
  (let ((local-path (local-profile-image-path user-id profile-image-url)))
    (send *profile-image-process* (list profile-image-url local-path))
    local-path))

(defun start ()
  (when *timeline-process*
    (sb-thread:destroy-thread *timeline-process*))
  (when *profile-image-process*
    (sb-thread:destroy-thread *profile-image-process*))
  (timeline)
  (start-profile-image-process))


#|
(drakma:http-request "http://a2.twimg.com/profile_images/119862218/スナップショット_2009-04-03_20-15-55_normal.png")
|#