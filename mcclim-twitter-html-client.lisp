#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :mcclim)
  (require :mcclim-freetype)
  (require :mcclim-uim)
  (require :cl-twitter)
  (require :net-telent-date))
|#

(defpackage :mcclim-twitter-html-client
    (:use :clim :clim-lisp))

(in-package :mcclim-twitter-html-client)

;; 文字コードは UTF-8 で
(setf drakma:*drakma-default-external-format* :utf-8)

(defvar *auth*
  (with-open-file (in (merge-pathnames ".twitter.lisp"
                                       (user-homedir-pathname)))
    (read in))
  "Basic 認証のパラメータを取得する。~/.twitter.lisp の中身は
(\"username\" \"password\")")

(defun dispay-create-at (tweet)
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time
       (net.telent.date:parse-time (twitter:tweet-created-at tweet)))
    (format nil "~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d"
            month date hour minute second)))

(defun update-timeline (frame)
  (with-output-to-string (*standard-output*)
    (with-slots (timeline last-id) frame
      (let ((update (twitter:friends-timeline :since-id last-id)))
        (when update
          (setf last-id (twitter:tweet-id (car update)))
          (setf timeline (append update timeline)))))))

(defun update-status (new-status)
  (twitter:send-tweet new-status))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type twitter:tweet ()))

(define-presentation-method present (object (type twitter:tweet)
                                            stream view &key)
  (format stream "~15a ~a ~a"
          (twitter:twitter-user-screen-name
                                    (twitter:tweet-user object))
          (twitter:tweet-text object)
          (dispay-create-at object)))

(defun table-format (stream timeline)
  (fresh-line stream)
  (formatting-table (stream :x-spacing '(1 :character))
    (loop for tweet in timeline
          do (formatting-row (stream)
               (formatting-cell (stream)
                 (princ (twitter:twitter-user-screen-name
                         (twitter:tweet-user tweet))
                        stream))
               (formatting-cell (stream)
                 (princ (twitter:tweet-text tweet) stream))
               (formatting-cell (stream)
                 (princ (dispay-create-at tweet) stream))))))


(defun display-timeline (frame pane)
  (with-slots (timeline) frame
    (table-format pane timeline)))

;; (defun display-timeline (frame pane)
;;   (with-slots (timeline) frame
;;     (mapc (lambda (tweet)
;;             (updating-output (pane :unique-id tweet)
;;               (present tweet 'twitter:tweet :stream pane)
;;               (terpri pane)))
;;           timeline)))

(define-application-frame twitter-frame ()
  ((timeline :initform nil :accessor timeline)
   (last-id :initform 1 :accessor last-id)
   (worker))
  (:menu-bar t)
  (:panes (timeline-pane
           :application
           :incremental-redisplay t
           :display-function 'display-timeline)
          (text-editor
           :text-editor
           :space-requirement (make-space-requirement :width 900))
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
                       (horizontally (:height 50) text-editor entry-button)))))

(define-twitter-frame-command (com-quit :menu t :name t) ()
  (frame-exit *application-frame*))

(define-twitter-frame-command (com-update-timeline :menu t :name t) ()
  (update-timeline *application-frame*))

(define-twitter-frame-command (com-update-status) ()
  (let* ((text-editor (find-pane-named *application-frame* 'text-editor))
         (new-status (gadget-value text-editor)))
    (update-status new-status)
    (setf (gadget-value text-editor) "")
    (update-timeline *application-frame*)
    (redisplay-frame-panes *application-frame*)))

(defmethod adopt-frame :after (manager (frame twitter-frame))
  (declare (ignore manager))
  (apply #'twitter:authenticate-user *auth*)
  (execute-frame-command frame `(com-update-timeline))
  (setf (slot-value frame 'worker)
        (quek:spawn (loop (quek:receive (:timeout 70)
                            (:quit (return)))
                          (update-timeline frame)
                          (redisplay-frame-panes frame)))))


(defmethod frame-exit :before ((frame twitter-frame))
  (quek:send (slot-value frame 'worker) :quit))

#+nil
(run-frame-top-level (make-application-frame 'twitter-frame
                                             :top 300 :left 600))