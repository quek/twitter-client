(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :mcclim)
  (require :mcclim-freetype)
  (require :mcclim-uim)
  (require :drakma)
  (require :cl-ppcre))

(defpackage :mcclim-twitter-html-client
    (:use :clim :clim-lisp :quek))

(in-package :mcclim-twitter-html-client)

;; 文字コードは UTF-8 で
(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)


(defparameter *basic-authorization*
  (with-open-file (in (merge-pathnames #p".twitter.lisp"
                                       (user-homedir-pathname)))
    (read in))
  "Basic 認証のパラメータを取得する。~/.twitter.lisp の中身は
(\"username\" \"password\")")

(defparameter *r* (ppcre:create-scanner
                   (string+
                    "<tr id=\"status_([^\"]+)\".*?"
                    ">([^<]*)</a></strong>.*?"
                    "<span class=\"entry-content\">(.*?)</span>")
                   :single-line-mode t))

(defvar *status* (make-hash-table :test #'equal))
;;(clrhash *status*)

(defclass status ()
  ((status-id :initarg :status-id)
   (user-id :initarg :user-id)
   (content :initarg :content)))

(defun update-timeline ()
  (labels ((trim (s)
             (string-trim
              '(#\space #\cr  #\lf)
              (reduce (lambda (a b)
                        (ppcre:regex-replace-all (car b) a (cadr b)))
                      '(("&lt;" "<")
                        ("&gt;" ">")
                        ("&quot;" "\"")
                        ("&amp;" "&"))
                      :initial-value s)))
           (request ()
             (drakma:http-request "http://twitter.com/home"
                                  :basic-authorization *basic-authorization*)))
    (let ((res (request)))
      (ppcre:do-register-groups ((#'trim status-id)
                                 (#'trim user-id)
                                 (#'trim content))
          (*r* res)
        (sunless (gethash status-id *status*)
          (setf it (make-instance 'status
                                  :status-id status-id
                                  :user-id user-id
                                  :content content)))))
    *status*))

(defun update-status (new-status)
  (drakma:http-request "http://twitter.com/statuses/update.json"
                       :basic-authorization *basic-authorization*
                       :method :post
                       :parameters `(("status" . ,new-status))))

(define-application-frame twitter-frame ()
  ()
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

(defun text-field-value-changed (pane value)
  (declare (ignore pane))
  (setf (entered-status *application-frame*) value))

(define-presentation-type status ())

(define-presentation-method present (object (type status) stream view &key)
  (with-slots (status-id user-id content) object
    (format stream "~15a ~a" user-id content)))

(defun display-timeline (frame pane)
  (declare (ignore frame))
  (mapc (lambda (status)
          (updating-output (pane :unique-id status)
            (present status 'status :stream pane)
            (terpri pane)))
        (sort (loop for k being the hash-keys in *status*
                    using (hash-value v)
                    collect v)
              (lambda (x y)
                (string<= (slot-value x 'status-id)
                          (slot-value y 'status-id))))))

(define-twitter-frame-command (com-quit :menu t :name t) ()
  (frame-exit *application-frame*))

(define-twitter-frame-command (com-update-timeline :menu t :name t) ()
  (update-timeline))

(define-twitter-frame-command (com-update-status) ()
  (let* ((text-editor (find-pane-named *application-frame* 'text-editor))
         (new-status (gadget-value text-editor)))
    (update-status new-status)
    (setf (gadget-value text-editor) "")
    (update-timeline)
    (redisplay-frame-panes *application-frame*)))

(defmethod adopt-frame :after (manager (frame twitter-frame))
  (declare (ignore manager))
  (execute-frame-command
   frame
   `(com-update-timeline)))

;;(clrhash *status*)
#+nil
(run-frame-top-level (make-application-frame 'twitter-frame
                                             :top 300 :left 600))