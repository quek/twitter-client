(in-package :info.read-eval-print.tw)

(named-readtables:in-readtable quek:|#"|)

;; 対 drakma 用おまじない
(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)


;; OAuth access token
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
      :user-parameters `(("status" . ,#"""#,message""")
                         ,@(when reply-to `(("in_reply_to_status_id" . ,(princ-to-string reply-to)))))))))

(defun retweet (id)
  (json:decode-json-from-string
   (oauth:access-protected-resource
    #"""http://api.twitter.com/1/statuses/retweet/#,id,.json"""
    *access-token*
    :request-method :post)))

(defun created-at-time (x)
  (multiple-value-bind (s m h) (decode-universal-time  (net.telent.date:parse-time x))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))


(defmacro with-user-stream ((stream-var) &body body)
  `(with-open-stream (,stream-var (oauth:access-protected-resource
                                   "https://userstream.twitter.com/2/user.json"
                                   *access-token*
                                   :drakma-args '(:want-stream t)))
     ,@body))
