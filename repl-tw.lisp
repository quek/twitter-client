(in-package :info.read-eval-print.repl-tw)

(named-readtables:in-readtable quek:|#"|)

(defparameter *profile-image-directory* (ensure-directories-exist "/tmp/info.read-eval-print.repl-tw/"))

(defun query-message ()
  (collect-fn 'string (constantly "") (^ format nil "~a~&~a" _a _b)
              (until-if (^ string= "." (if (string= "\\q" _)
                                           (return-from query-message nil)
                                           _))
                        (scan-stream *terminal-io* #'read-line))))

(defun tweet ()
  (let ((message (query-message)))
    (update message)
    message))

(defun reply (in-reply-to-status-id)
  (let ((message (query-message)))
    (update message :reply-to in-reply-to-status-id)
    message))

(defvar *timeline-process* nil)

(defun timeline ()
  (bordeaux-threads:make-thread
   (^ (with-user-stream (in)
        (collect-ignore (print-tweet (scan-stream in #'read-line)))))
   :name "https://userstream.twitter.com/2/user.json"))


(defun local-profile-image-path (user-id profile-image-url)
  (merge-pathnames (file-namestring (puri:uri-path (puri:uri profile-image-url)))
                   #"""#,*profile-image-directory*,/#,user-id,/"""))

(defun %get-profile-image (image-url local-path)
  (unless (probe-file local-path)
    (ensure-directories-exist local-path)
    (with-open-file (out local-path :direction :output :element-type '(unsigned-byte 8))
      (write-sequence (drakma:http-request image-url) out))
    ;; ImageMagic に頼る。
    (trivial-shell:shell-command #"""mogrify -resize 32x32 #,local-path""")))

(defun refresh-repl ()
  (sleep 0.1)
  (swank::with-connection ((swank::default-connection))
          (swank::eval-in-emacs '(save-current-buffer
                                  (set-buffer (get-buffer-create "*slime-repl sbcl*"))
                                  (save-excursion
                                    (iimage-mode 1))))))

(defvar *profile-image-process* nil)

(defun receive-process ()
  (ignore-errors
    (receive ()
      ((profile-image-url local-path)
       (%get-profile-image profile-image-url local-path)
       (refresh-repl)))))

(defun start-profile-image-process ()
  (spawn (loop (ignore-errors (receive-process)))))

(defun get-profile-image (user-id profile-image-url)
  (let ((local-path (local-profile-image-path user-id profile-image-url)))
    (send *profile-image-process* (list profile-image-url local-path))
    local-path))

;; TODO 画像表示の改善 http://d.hatena.ne.jp/cranebird/20110430/1304122311
(defun print-tweet (json-string)
  (ignore-errors
    (json:with-decoder-simple-clos-semantics
      (let ((json:*json-symbols-package* :info.read-eval-print.repl-tw))
        (let ((x (json:decode-json-from-string json-string)))
          (with-slots (text user id created--at) x
            (with-slots (name screen--name profile--image--url (user-id id)) user
              (let ((path (get-profile-image id profile--image--url)))
                (format
                 *query-io*
                  #"""~&#,path #,screen--name (#,name,) #,(created-at-time created--at) #,id,~&#,text,~%""")))))))))

(defun start ()
  (when *timeline-process*
    (send *timeline-process* +exit+)
    (setf *timeline-process* nil))
  (when *profile-image-process*
    (send *profile-image-process* +exit+)
    (setf *profile-image-process* nil))
  (setf *timeline-process* (timeline))
  (setf *profile-image-process* (start-profile-image-process)))
