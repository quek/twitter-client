(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gtk)
  (require :cl-twitter)
  (require :net-telent-date)
  (require :drakma))

(gtk:clg-init)

(setf drakma:*drakma-default-external-format* :utf-8)

(defvar *auth*
  (with-open-file (in (merge-pathnames ".twitter.lisp"
                                       (user-homedir-pathname)))
    (read in)))

(defvar *user-profile-images* (make-hash-table :test #'equal))

(defun dispay-create-at (tweet)
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time
       (net.telent.date:parse-time (twitter:tweet-created-at tweet)))
    (format nil "~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d"
            month date hour minute second)))

(defun get-user-profile-image-pixbuf (tweet)
  (let* ((url (twitter:twitter-user-profile-image-url
               (twitter:tweet-user tweet)))
         (pixbuf (gethash url *user-profile-images*)))
    (if pixbuf
        pixbuf
        (setf (gethash url *user-profile-images*)

              (with-open-stream (var (drakma:http-request url
                                                          :want-stream t))
                (make-instance 'gdk:pixbuf :source var)))
        (make-instance 'gdk:pixbuf :width 10 :height 10))))

(defun update-timeline (last-id store)
  (let ((new-timeline (twitter:friends-timeline :since-id last-id)))
    (print new-timeline)
    (if new-timeline
        (progn
          (loop with iter = (make-instance 'gtk:tree-iter)
                for i in (reverse new-timeline)
                do (gtk:list-store-prepend
                       store
                       (list :user (twitter:twitter-user-screen-name
                                    (twitter:tweet-user i))
                             :text (twitter:tweet-text i)
                             :time (dispay-create-at i)
                             :user-image (get-user-profile-image-pixbuf i))
                       iter))
          (twitter:tweet-id (car new-timeline)))
        last-id)))

(defun send-text (text-buffer)
  (let ((text (gtk:text-buffer-text text-buffer)))
    (unless (string= "" text)
      (twitter:send-tweet text)
      (gtk:text-buffer-set-text text-buffer ""))))

(defun main ()
  (apply #'twitter:authenticate-user *auth*)
  (let* ((last-id 1)
         (store (make-instance 'gtk:list-store
                               :column-types '(string string string gdk:pixbuf)
                               :column-names '(:user :text :time :user-image)))
         (tree  (make-instance 'gtk:tree-view :model store
                               :expand t :fill t))
         (text-view (make-instance 'gtk:text-view))
         (buffer (gtk:text-view-buffer text-view))
         (scrolled-window (make-instance 'gtk:scrolled-window
                                         :child tree))
         (timer nil))
    (labels ((update ()
               (setf last-id (update-timeline last-id store)))
             (scroll-to-top (&rest args)
               (print args)
               (let ((adjustment (gtk:scrolled-window-vadjustment
                                  scrolled-window)))
                 (setf (gtk:adjustment-value adjustment) 0))))
      (gtk:signal-connect (gtk:scrolled-window-vadjustment scrolled-window)
                          :changed
                          #'scroll-to-top)
      (update)
      (setf timer (gtk:timeout-add 70000 #'update))
      (let ((column (make-instance 'gtk:tree-view-column :expand nil))
            (cell (make-instance 'gtk:cell-renderer-pixbuf)))
        (gtk:cell-layout-pack column cell :expand nil)
        (gtk:cell-layout-add-attribute
         column cell 'pixbuf (gtk:tree-model-column-index store :user-image))
        (gtk:tree-view-append-column tree column))
      (loop for (title index sizing) in '(("ユーザ" :user :autosize)
                                          ("さえずり" :text :fixed)
                                          ("いつ" :time :autosize))
            do (let ((column (make-instance 'gtk:tree-view-column :title title
                                            :expand (eq :fixed sizing)
                                            :resizable t
                                            :sizing sizing))
                     (cell (make-instance 'gtk:cell-renderer-text)))
                 (gtk:cell-layout-pack column cell :expand nil)
                 (gtk:cell-layout-add-attribute
                  column cell 'text (gtk:tree-model-column-index store index))
                 (gtk:tree-view-append-column tree column)))
      (gtk:within-main-loop
       (make-instance
        'gtk:window
        :default-width  900
        :default-height 700
        :title "clg で twitter"
        :border-width 2
        :visible t :show-children t
        :signal (list :delete-event
                      (lambda (event)
                        (declare (ignore event))
                        (gtk:timeout-remove timer)
                        nil))
        :child (make-instance
                'gtk:v-box
                :child (list
                        (make-instance
                         'gtk:h-box
                         :child (list text-view)
                         :child (list (make-instance
                                       'gtk:button
                                       :label "投稿する"
                                       :signal (list 'clicked
                                                     (lambda ()
                                                       (send-text buffer)
                                                       (update))))
                                      :fill nil :expand nil)
                         :border-width 2)
                        :fill nil :expand nil)
                :child (list scrolled-window :expand t :fill t)))))))

;;(main)