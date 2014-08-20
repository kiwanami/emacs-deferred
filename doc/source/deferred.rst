====================
 deferred.el manual
====================

Sample
======

You can find following sample codes in ``deferred-sample.el``.
Executing :el:function:`eval-last-sexp` (C-x C-e), you can try those
codes.

Basic usage
-----------

This is a basic deferred chain. This code puts some outputs into
message buffer, and then require a number from minibuffer.

Chain::

    (deferred:$
      (deferred:next
        (lambda () (message "deferred start")))
      (deferred:nextc it
        (lambda ()
          (message "chain 1")
          1))
      (deferred:nextc it
        (lambda (x)
          (message "chain 2 : %s" x)))
      (deferred:nextc it
        (lambda ()
          (read-minibuffer "Input a number: ")))
      (deferred:nextc it
        (lambda (x)
          (message "Got the number : %i" x)))
      (deferred:error it
        (lambda (err)
          (message "Wrong input : %s" err))))

* This s-exp returns immediately.

 * Asynchronous tasks start subsequently.

* The macro :el:function:`deferred:$` chains deferred objects.

 * The anaphoric variable :el:var:`it` holds a deferred object in the
   previous line.

* The next deferred task receives the value that is returned by the
  previous deferred one.

* Inputting a wrong value, such as alphabets, this s-exp raises an
  error. The error is caught by the errorback function defined by
  :el:function:`deferred:error`.

Timer
-----

After evaluating this s-exp and waiting for 1 second, a message is shown in the minibuffer.

Timer::

  (deferred:$
    (deferred:wait 1000) ; 1000msec
    (deferred:nextc it
      (lambda (x)
        (message "Timer sample! : %s msec" x))))

* The next deferred task subsequent to deferred:wait receives the
  actual elapse time in millisecond.

Commands and Sub-process
------------------------

This s-exp inserts the result that is performed by the command 'ls
-la'. (This s-exp may not run in windows. Try 'dir' command.)

Command process::

  (deferred:$
    (deferred:process "ls" "-la")
    (deferred:nextc it
      (lambda (x) (insert x))))

* This s-exp hardly blocks Emacs because of asynchronous mechanisms.

HTTP GET : Text
---------------

This s-exp inserts a text from http://www.gnu.org asynchronously. (You can clear the result with undo command.)

HTTP GET::

  (require 'url)
  
  (deferred:$
    (deferred:url-retrieve "http://www.gnu.org")
    (deferred:nextc it
      (lambda (buf)
        (insert  (with-current-buffer buf (buffer-string)))
        (kill-buffer buf))))

HTTP Get : Image
----------------

This s-exp inserts an image from google asynchronously.

Get an image::

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

Parallel
--------

This s-exp retrieves two images from google concurrently and wait for the both results. Then, the file sizes of the images are inserted the current buffer.

Parallel deferred::

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

* The function 'deferred:parallel' runs asynchronous tasks
  concurrently.

* The function wait for all results, regardless normal or
  abnormal. Then, the subsequent tasks are executed.

* The next task receives a list of the results.

  * The order of the results is corresponding to one of the argument.
  * Giving an alist of tasks as the argument, the results alist is returned.

Deferred Combination : try-catch-finally
----------------------------------------

This s-exp executes following tasks:

* Getting an image by wget command,

* Resizing the image by convert command in ImageMagick,

* Insert the re-sized image into the current buffer. You can construct
  the control structure of deferred tasks, like try-catch-finally in
  Java.

Get an image by wget and resize by ImageMagick::

  (deferred:$ 
  
    ;; try
    (deferred:$
      (deferred:process "wget" "-O" "a.jpg" "http://www.gnu.org/software/emacs/tour/images/splash.png")
      (deferred:nextc it
        (lambda () (deferred:process "convert" "a.jpg" "-resize" "100x100" "jpg:b.jpg")))
      (deferred:nextc it
        (lambda ()
          (clear-image-cache)
          (insert-image (create-image (expand-file-name "b.jpg") 'jpeg nil)))))
  
    ;; catch
    (deferred:error it ; 
      (lambda (err) 
        (insert "Can not get a image! : " err)))
  
    ;; finally
    (deferred:nextc it
      (lambda ()
        (deferred:parallel
          (lambda () (delete-file "a.jpg"))
          (lambda () (delete-file "b.jpg")))))
    (deferred:nextc it
      (lambda (x) (message ">> %s" x))))

* In this case, the deferred tasks are statically connected.

Here is an another sample code for try-catch-finally blocks. This is
simpler than above code because of the 'deferred:try' macro. (Note:
They bring the same results practically, but are not perfectly
identical. The 'finally' task may not be called because of
asynchrony.)

Try-catch-finally::

  (deferred:$
    (deferred:try
      (deferred:$
        (deferred:process "wget" "-O" "a.jpg" "http://www.gnu.org/software/emacs/tour/images/splash.png")
        (deferred:nextc it
          (lambda () (deferred:process "convert" "a.jpg" "-resize" "100x100" "jpg:b.jpg")))
        (deferred:nextc it
          (lambda ()
            (clear-image-cache)
            (insert-image (create-image (expand-file-name "b.jpg") 'jpeg nil)))))
      :catch
      (lambda (err) (insert "Can not get a image! : " err))
      :finally
      (lambda ()
        (delete-file "a.jpg")
        (delete-file "b.jpg")))
    (deferred:nextc it
      (lambda (x) (message ">> %s" x))))

Timeout
-------

Although a long time command is executed (3 second sleeping), the task
is canceled by timeout for 1 second.

The function 'deferred:earlier' also runs asynchronous tasks
concurrently, however, the next deferred task receives the first
result. The other results and tasks will be canceled.

Timeout Process::

  (deferred:$
    (deferred:earlier
      (deferred:process "sh" "-c" "sleep 3 | echo 'hello!'")
      (deferred:$
        (deferred:wait 1000) ; timeout msec
        (deferred:nextc it (lambda () "canceled!"))))
    (deferred:nextc it
      (lambda (x) (insert x))))

* Changing longer timeout for 'deferred:wait', the next task receives
  a result of the command.

* When a task finishes abnormally, the task is ignored.

  * When all tasks finishes abnormally, the next task receives nil.

* The functions 'deferred:parallel' and 'deferred:earlier' may be
  corresponding to 'and' and 'or', respectively.

Here is an another sample code for timeout, employing 'deferred:timeout' macro.

Timeout macro::

  (deferred:$
    (deferred:timeout
      1000 "canceled!"
      (deferred:process "sh" "-c" "sleep 3 | echo 'hello!'"))
    (deferred:nextc it
      (lambda (x) (insert x))))

Loop and Animation
------------------

This s-exp plays an animation at the cursor position for few
seconds. Then, you can move cursor freely, because the animation does
not block Emacs.

Returning a deferred object in the deferred tasks, the returned task
is executed before the next deferred one that is statically connected
on the source code. (In this case, the interrupt task is dynamically
connected.)

Employing a recursive structure of deferred tasks, you can construct a
deferred loop. It may seem the multi-thread in Emacs Lisp.

Loop and animation::

  (lexical-let ((count 0) (anm "-/|\\-")
                (end 50) (pos (point))
                (wait-time 50))
    (deferred:$
      (deferred:next
        (lambda (x) (message "Animation started.")))
  
      (deferred:nextc it
        (deferred:lambda (x)
          (save-excursion
            (when (< 0 count)
              (goto-char pos) (delete-char 1))
            (insert (char-to-string 
                     (aref anm (% count (length anm))))))
          (if (> end (incf count)) ; return nil to stop this loop
              (deferred:nextc (deferred:wait wait-time) self)))) ; return the deferred
  
      (deferred:nextc it
        (lambda (x)
          (save-excursion
            (goto-char pos) (delete-char 1))
          (message "Animation finished.")))))

* 'deferred:lambda' is an anaphoric macro in which 'self' refers
  itself. It is convenient to construct a recursive structure.

Wrapping asynchronous function
------------------------------

Let's say you have an asynchronous function which takes a
callback. For example, dbus.el, xml-rpc.el and websocket.el has such
kind of asynchronous APIs. To use such libraries with deferred.el, you
can make an unregistered deferred object using deferred:new and then
start the deferred callback queue using deferred:callback-post in the
callback given to the asynchronous function. If the asynchronous
function supports "errorback", you can use deferred:errorback-post to
pass the error information to the following callback queue.

In the following example, run-at-time is used as an example for the
asynchronous function. Deferred.el already has deferred:wait for this
purpose so that you don't need the following code if you want to use
run-at-time.

::

  (deferred:$
    (deferred:next
      (lambda ()
        (message "1")
        1))
    (deferred:nextc it
      (lambda (x)
        (lexical-let ((d (deferred:new #'identity)))
          (run-at-time 0 nil (lambda (x)
                               ;; Start the following callback queue now.
                               (deferred:callback-post d x))
                       x)
          ;; Return the unregistered (not yet started) callback
          ;; queue, so that the following queue will wait until it
          ;; is started.
          d)))
    ;; You can connect deferred callback queues
    (deferred:nextc it
      (lambda (x)
        (message "%s" (1+ x)))))
