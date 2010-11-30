#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :mcclim)
  (require :mcclim-freetype)
  (require :mcclim-jpeg-bitmaps)
  (require :mcclim-png-bitmaps)
  (require :mcclim-uim)
  (require :cl-twitter)
  (require :net-telent-date))

TODO
更新時のスクロール
改行したとき画像とかぶる
|#

(defpackage :mcclim-twitter-client
    (:use :clim :clim-lisp))

(in-package :mcclim-twitter-client)

;; 文字コードは UTF-8 で
(setf drakma:*drakma-default-external-format* :utf-8)

(defvar *auth*
  (with-open-file (in (merge-pathnames ".twitter.lisp"
                                       (user-homedir-pathname)))
    (read in))
  "Basic 認証のパラメータを取得する。~/.twitter.lisp の中身は
(\"username\" \"password\")")

(defclass tworker ()
  ((public-timeline :initform nil :accessor public-timeline)
   (friend-timeline :initform nil :accessor friend-timeline)
   (last-id :initform nil :accessor last-id)
   (friend-timeline-update-callback
    :initarg :friend-timeline-update-callback)))

(defgeneric update-friend-timeline (tworker))
(defmethod update-friend-timeline ((tworker tworker))
  (with-slots (friend-timeline last-id friend-timeline-update-callback)
      tworker
    (let ((update (apply #'twitter:friends-timeline
                         (and last-id (list :since-id last-id)))))
      (when update
        (setf last-id (twitter:tweet-id (car update)))
        (setf friend-timeline (append update friend-timeline))
        (mapc (lambda (tweet)
                (ignore-errors (get-user-profile-image tweet)))
              update)
        (funcall friend-timeline-update-callback friend-timeline)))))

(defgeneric tworker-loop (tworker))
(defmethod tworker-loop ((tworker tworker))
  (apply #'twitter:authenticate-user *auth*)
  (update-friend-timeline tworker)
  (loop
    (quek:receive (80 (update-friend-timeline tworker))
      ((:send text)
       (twitter:send-tweet text)
       (update-friend-timeline tworker))
      (:update-friend-timeline
       (update-friend-timeline tworker))
      (:quit
       (return)))))

(defgeneric tworker-start (tworker))
(defmethod tworker-start ((tworker tworker))
  (quek:spawn (tworker-loop tworker)))

(defun dispay-create-at (tweet)
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time
       (net.telent.date:parse-time (twitter:tweet-created-at tweet)))
    (format nil "~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d"
            month date hour minute second)))

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

(defun get-user-profile-image (tweet)
  (let* ((url (twitter:twitter-user-profile-image-url
               (twitter:tweet-user tweet)))
         (image (gethash url *user-profile-images*)))
    (if image
        image
        (setf (gethash url *user-profile-images*)
              (make-pattern-from-url url)))))

(defun table-format (stream timeline)
  (fresh-line stream)
  (formatting-table (stream :x-spacing '(1 :character))
    (loop for tweet in timeline
          do (formatting-row (stream)
               (formatting-cell (stream)
                 (let ((image (gethash (twitter:twitter-user-profile-image-url
                                        (twitter:tweet-user tweet))
                                       *user-profile-images*)))
                   (if image
                       (draw-pattern* stream image 0 0)
                       (princ "*" stream))))
               (formatting-cell (stream)
                 (princ (twitter:twitter-user-screen-name
                         (twitter:tweet-user tweet))
                        stream))
               (formatting-cell (stream)
                 (princ (twitter:tweet-text tweet) stream))
               (formatting-cell (stream)
                 (princ (dispay-create-at tweet) stream))))))


;;(defun display-timeline (frame pane)
;;  (with-slots (timeline) frame
;;    (table-format pane timeline)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type twitter:tweet ()))

(define-presentation-method present (tweet (type twitter:tweet)
                                           stream view &key)
  (let ((image (gethash (twitter:twitter-user-profile-image-url
                         (twitter:tweet-user tweet))
                        *user-profile-images*)))
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (when image
        (draw-pattern* stream image x y))
      (stream-increment-cursor-position stream 52 0)
      (format stream "~15a ~a~%"
              (twitter:twitter-user-screen-name
               (twitter:tweet-user tweet))
              (dispay-create-at tweet))
      (stream-increment-cursor-position stream 52 0)
      (princ (twitter:tweet-text tweet) stream)
      (stream-set-cursor-position stream 0 (+ y 30)))))

(defun display-timeline (frame pane)
  (with-slots (timeline) frame
    (mapc (lambda (tweet)
            (updating-output (pane :unique-id tweet)
              (present tweet 'twitter:tweet :stream pane)
              (terpri pane)))
          (reverse timeline))))

(define-application-frame twitter-frame ()
  ((timeline :initform nil :accessor timeline)
   (tworker))
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

(define-twitter-frame-command (com-update-timeline :menu t :name t) ()
  (quek:send (slot-value *application-frame* 'tworker)
             :update-friend-timeline))

(define-twitter-frame-command (com-update-status) ()
  (let* ((text-field (find-pane-named *application-frame* 'text-field))
         (new-status (gadget-value text-field)))
    (quek:send (slot-value *application-frame* 'tworker)
               (list :send new-status))
    (setf (gadget-value text-field) "")))

(defmethod adopt-frame :after (manager (frame twitter-frame))
  (declare (ignore manager))
  (let ((tworker (make-instance
                  'tworker
                  :friend-timeline-update-callback
                  (lambda (friend-timeline)
                    (setf (timeline frame) friend-timeline)
                    (redisplay-frame-panes frame)))))
  (setf (slot-value frame 'tworker)
        (tworker-start tworker))))

(defmethod frame-exit :before ((frame twitter-frame))
  (quek:send (slot-value frame 'tworker) :quit))



#|
(progn
  (define-application-frame foo-frame ()
    ()
    (:pane
     (make-pane
      'application-pane
      :display-function
      (lambda (frame stream)
        (declare (ignore frame))
        (let ((pattern
               ;;(make-pattern-from-bitmap-file
               ;; "/home/ancient/archive/1.jpeg"
               ;;:format :jpeg)
               (make-pattern-from-bitmap-file
                "/home/ancient/letter/lisp/clbuild/source/cl-png/test/images/butterfly8.png"
                :format :png)
                ))
          (draw-pattern* stream
                         pattern
                         0 0)))))
    (:geometry :width 300 :height 300 :top 300 :left 500))

  (define-foo-frame-command (com-quit :menu t :name t
                                      :keystroke (#\q :meta)) ()
    (frame-exit *application-frame*))

  (run-frame-top-level (make-application-frame 'foo-frame)))


(make-pattern-from-bitmap-file "/home/ancient/archive/1.jpeg" :format :jpeg)
|#

#+nil
(run-frame-top-level (make-application-frame 'twitter-frame
                                             :top 300 :left 600))