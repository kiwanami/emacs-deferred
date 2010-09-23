;;; Basic Chain

(deferred:$
  (deferred:next 
    (lambda (x) (message "deferred start")))
  (deferred:nextc it
    (lambda (x) 
      (message "chain 1")
      1))
  (deferred:nextc it
    (lambda (x)
      (message "chain 2 : %s" x)))
  (deferred:nextc it
    (lambda (x)
      (read-minibuffer "Input a number: ")))
  (deferred:nextc it
    (lambda (x)
      (message "Got the number : %i" x)))
  (deferred:error it
    (lambda (err)
      (message "Wrong input : %s" err))))

;;; Web Access

;; Simple web access

(require 'url)

(deferred:$
  (deferred:url-retrieve "http://www.gnu.org")
  (deferred:nextc it
    (lambda (buf)
      (insert  (with-current-buffer buf (buffer-string)))
      (kill-buffer buf))))

;; Get an image

(deferred:$
  (deferred:url-retrieve "http://www.google.co.jp/intl/en_com/images/srpr/logo1w.png")
  (deferred:nextc it
    (lambda (buf)
      (insert-image 
       (create-image 
        (let ((data (with-current-buffer buf (buffer-string))))
          (substring data (+ (string-match "\n\n" data) 2)))
        'png t))
      (kill-buffer buf))))

;; Parallel deferred

(deferred:$
  (deferred:parallel
    (lambda ()
      (deferred:url-retrieve "http://www.google.co.jp/intl/en_com/images/srpr/logo1w.png"))
    (lambda ()
      (deferred:url-retrieve "http://www.google.co.jp/images/srpr/nav_logo14.png")))
  (deferred:nextc it
    (lambda (buffers)
      (loop for i in buffers
            do 
            (insert 
             (format 
              "size: %s\n"
              (with-current-buffer i (length (buffer-string)))))
            (kill-buffer i)))))

;; Get an image by wget and resize by ImageMagick

(deferred:$ 

  ;; try
  (deferred:$
    (deferred:process "wget" "-O" "a.jpg" "http://www.gnu.org/software/emacs/tour/images/splash.png")
    (deferred:nextc it
      (lambda (x) (deferred:process "convert" "a.jpg" "-resize" "100x100" "jpg:b.jpg")))
    (deferred:nextc it
      (lambda (x)
        (clear-image-cache)
        (insert-image (create-image (expand-file-name "b.jpg") 'jpeg nil)))))

  ;; catch
  (deferred:error it ; 
    (lambda (err) 
      (insert "Can not get a image! : " err)))

  ;; finally
  (deferred:nextc it
    (lambda (x)
      (deferred:parallel
        (lambda () (delete-file "a.jpg"))
        (lambda () (delete-file "b.jpg")))))
  (deferred:nextc it
    (lambda (x) (message ">> %s" x))))

