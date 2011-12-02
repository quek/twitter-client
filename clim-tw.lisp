(in-package :info.read-eval-print.clim-tw)

(defclass user-stream-event (clim:window-manager-event)
  ((tweet :accessor tweet :initarg :tweet))
  (:documentation "ユーザストリームリーム更新時のイベント"))


(defgeneric run-worker (worker))

(defgeneric update-timeline (worker))

(defclass worker ()
  ((frame :accessor frame :initarg :frame)))

(defmacro with-text ((line) &body body)
  `(ignore-errors
     (json:with-decoder-simple-clos-semantics
       (let ((json:*json-symbols-package* #.*package*))
         (let ((x (json:decode-json-from-string ,line)))
           (with-slots (text user id (created-at created--at)) x
             (with-slots (name
                          (screen-name screen--name)
                          (profile-image-url profile--image--url)
                          (user-id id)) user
               ,@body)))))))

(defclass tweet ()
  ((text       :accessor text       :initarg :text)
   (id         :accessor id         :initarg :id)
   (created-at :accessor created-at :initarg :created-at)
   (user       :accessor user       :initarg :user)))

(defvar *users* (make-hash-table :test #'equal) "key user-id, value user")

(defclass user ()
  ((id                :accessor id                :initarg :id)
   (screen-name       :accessor screen-name       :initarg :screen-name)
   (profile-image-url :accessor profile-image-url :initarg :profile-image-url)))

(defun get-user (id screen-name profile-image-url)
  (sif (gethash id *users*)
       it
       (setf it (make-instance 'user
                               :id id
                               :screen-name screen-name
                               :profile-image-url profile-image-url))))
(defun json-to-tweet (json)
  (with-text (json)
    (and id text created-at user-id screen-name profile-image-url
         (make-instance 'tweet
                        :id id
                        :text text
                        :created-at created-at
                        :user (get-user user-id screen-name profile-image-url)))))

(defmethod run-worker ((worker worker))
  (with-user-stream (in)
    (loop for line = (read-line in)
          for tweet = (json-to-tweet line)
          when tweet
            do (clim-internals::event-queue-prepend
                (climi::frame-event-queue (frame worker))
                (make-instance 'user-stream-event :tweet tweet)))))


(defun spawn-worker (frame)
  (spawn (run-worker (make-instance 'worker :frame frame))))



(defvar *user-profile-images* (make-hash-table :test #'equal))
(defvar *tmp-dir* #p"/tmp/mcclim-twitter-client/")

(defun make-pattern-from-url (url)
  (ensure-directories-exist *tmp-dir*)
  (let* ((name (subseq url (1+ (position #\/ url :from-end t))))
         (path (merge-pathnames name *tmp-dir*))
         (type (subseq name (1+ (position #\. name :from-end t)))))
    (with-open-file (out path
                         :direction :output
                         :element-type '(unsigned-byte 8))
      (write-sequence (drakma:http-request url) out))
    (unwind-protect
         (make-pattern-from-bitmap-file
          path :format (intern (string-upcase type) :keyword))
      (delete-file path))))

(defun get-user-profile-image (profile-image-url)
  (let* ((image (gethash profile-image-url *user-profile-images*)))
    (if image
        image
        (setf (gethash profile-image-url *user-profile-images*)
              (make-pattern-from-url profile-image-url)))))

(defun table-format (stream line)
  (with-text (line)
    (fresh-line stream)
    (clim:formatting-table (stream :x-spacing '(1 :character))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream)
          (let ((image (gethash profile-image-url
                                *user-profile-images*)))
            (if image
                (clim:draw-pattern* stream image 0 0)
                (princ "*" stream))))
        (clim:formatting-cell (stream)
          (princ screen-name stream))
        (clim:formatting-cell (stream)
          (princ text stream))
        (clim:formatting-cell (stream)
          (princ created-at stream))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type tweet ()))

(define-presentation-method present (tweet (type tweet)
                                           stream view &key)
  (with-text (tweet)
    (let ((image (gethash profile-image-url
                          *user-profile-images*)))
      (multiple-value-bind (x y) (stream-cursor-position stream)
        (when image
          (draw-pattern* stream image x y))
        (stream-increment-cursor-position stream 52 0)
        (format stream "~15a ~a~%"
                screen-name
                created-at)
        (stream-increment-cursor-position stream 52 0)
        (princ text stream)
        (stream-set-cursor-position stream 0 (+ y 30))))))

(defun display-timeline (frame pane)
  (with-slots (timeline) frame
    (mapc (lambda (tweet)
            (updating-output (pane :unique-id tweet)
              (present tweet 'tweet :stream pane)
              (terpri pane)))
          (reverse timeline))))

(define-application-frame twitter-frame ()
  ((timeline :initform nil :accessor timeline)
   (worker))
  (:menu-bar t)
  (:panes (timeline-pane
           :application
           :incremental-redisplay t
           :display-function 'display-timeline)
          (text-field
           :text-field
           :space-requirement (make-space-requirement :width 900)
           :activate-callback
           (lambda (this)
             (declare (ignore this))
             (execute-frame-command *application-frame*
                                    `(com-update-status))))
          (entry-button
           :push-button
           :label "投稿する"
           :activate-callback
           (lambda (button)
             (declare (ignore button))
             (execute-frame-command *application-frame*
                                    `(com-update-status)))))
  (:layouts (default (vertically (:width 900 :height 600)
                       timeline-pane
                       (horizontally (:height 50) text-field entry-button)))))

(define-twitter-frame-command (com-quit :menu t :name t
                                        :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(define-twitter-frame-command (com-update-status) ()
  (let* ((text-field (find-pane-named *application-frame* 'text-field))
         (new-status (gadget-value text-field)))
    (update new-status)
    (setf (gadget-value text-field) "")))

(defmethod adopt-frame :after (manager (frame twitter-frame))
  (declare (ignore manager))
  (setf (slot-value frame 'worker) (spawn-worker frame)))

(defmethod frame-exit :before ((frame twitter-frame))
  (quek:send (slot-value frame 'worker) :quit))

(defmethod handle-event ((frame twitter-frame) (event user-stream-event))
  (push (tweet event) (timeline frame))
  (redisplay-frame-panes frame))


#+nil
(run-frame-top-level (make-application-frame 'twitter-frame
                                             :top 300 :left 600))