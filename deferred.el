;;; deferred.el --- Simple asynchronous functions for emacs lisp

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: deferred, async

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 'deferred.el' is a simple library for asynchronous tasks.  The API
;; is almost the same as JSDeferred written by cho45. See the
;; JSDeferred and Mochikit.Async web sites for further documentations.
;; [http://github.com/cho45/jsdeferred]
;; [http://mochikit.com/doc/html/MochiKit/Async.html]

;; A good introduction document (JavaScript)
;; [http://cho45.stfuawsc.com/jsdeferred/doc/intro.en.html]

;;; Samples:

;; ** HTTP Access

;; (require 'url)
;; (deferred:$
;;   (deferred:url-retrieve "http://www.gnu.org")
;;   (deferred:nextc it
;;     (lambda (buf)
;;       (insert  (with-current-buffer buf (buffer-string)))
;;       (kill-buffer buf))))

;; ** Invoking command tasks

;; (deferred:$
;;   (deferred:process "wget" "-O" "a.jpg" "http://www.gnu.org/software/emacs/tour/images/splash.png")
;;   (deferred:nextc it
;;     (lambda (x) (deferred:process "convert" "a.jpg" "-resize" "100x100" "jpg:b.jpg")))
;;   (deferred:nextc it
;;     (lambda (x)
;;       (insert-image (create-image (expand-file-name "b.jpg") 'jpeg nil)))))


;;; API Guide

;; ** Base functions

;; * deferred:next(fun) -> d
;; Execute a function asynchronously.

;; * deferred:nextc(d, fun) -> d
;; Add a callback function to the deferred object.

;; * deferred:error(d, fun) -> d
;; Add a errorback function to the deferred object.

;; * deferred:cancel(d) -> d
;; Cancel the deferred task.

;; * deferred:wait(msec) -> d
;; Return a deferred object that will be called after the specified millisecond.

;; * deferred:$
;; Anaphoric macro for deferred chains.

;; ** Utility functions

;; * deferred:loop(num, fun) -> d
;; Iterate the function for the specified times.

;; * deferred:parallel(fun1, fun2, ...) -> d
;; Execute given functions in parallel and wait for all callback values.

;; * deferred:earlier(fun1, fun2, ...) -> d
;; Execute given functions in parallel and wait for the first callback value.

;; ** deferred instance methods

;; * deferred:new(&optional fun) -> d
;; Create a deferred object.

;; * deferred:callback(value)
;; Start callback chain.

;; * deferred:errorback(error)
;; Start errorback chain.
;; All errors occurred in deferred tasks are cought by errorback handlers.
;; If you want to debug the error, set `deferred:debug-on-signal' to non-nil.
;; (setq deferred:debug-on-signal t)

;; * deferred:callback-post(value)
;; Start callback chain asynchronously.

;; * deferred:callback-post(error)
;; Start errorback chain asynchronously.

;; ** Wrapper function

;; * deferred:call(fun, arg1, arg2, ...) -> d
;; * deferred:apply(fun, arg-list) -> d
;; Call the function asynchronously.

;; * deferred:process(cmd, args, ...) -> d
;; Execute the command asynchronously.

;; * deferred:url-retrieve(url) -> d
;; Retrieve an content of the url asynchronously.

;; ** Applications

;; *Inertial scrolling for Emacs
;; [http://github.com/kiwanami/emacs-inertial-scroll]

;; This program makes simple multi-thread function, using
;; deferred.el.



(eval-when-compile
  (require 'cl))

;;; Code:

(defmacro deferred:aand (test &rest rest)
  "[internal] Anaphoric AND."
  (declare (debug ("test" form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest `(deferred:aand ,@rest) 'it))))

(defmacro deferred:$ (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form)))
  `(let (it)
     ,@(loop for i in elements
             with it = nil
             collect
             `(setq it ,i))
     it))

(defmacro deferred:lambda (args &rest body)
  "Anaphoric lambda macro for self recursion."
  (declare (debug ("args" form &rest form)))
  (let ((argsyms (loop for i in args collect (gensym))))
  `(lambda (,@argsyms)
     (lexical-let (self)
       (setq self (lambda( ,@args ) ,@body))
       (funcall self ,@argsyms)))))

(defun deferred:setTimeout (f msec)
  "[internal] Timer function that emulates the `setTimeout' function in JS."
  (run-at-time (/ msec 1000.0) nil f))

(defun deferred:cancelTimeout (id)
  "[internal] Timer cancellation function that emulates the `cancelTimeout' function in JS."
  (cancel-timer id))

(defun deferred:call-lambda (f &optional arg)
  "[internal] Call a function with one or zero argument safely.
The lambda function can define with zero and one argument."
  (condition-case err
      (funcall f arg)
    ('wrong-number-of-arguments (funcall f))))


;; debug

(eval-and-compile
  (defvar deferred:debug nil "Debug output switch."))
(defvar deferred:debug-count 0 "[internal] Debug output counter.")

(defmacro deferred:message (&rest args)
  "[internal] Debug log function."
  (when deferred:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*deferred:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" deferred:debug-count (format ,@args)))))
       (incf deferred:debug-count))))

(defun deferred:message-mark ()
  "[internal] Debug log function."
  (interactive)
  (deferred:message "==================== mark ==== %s" 
    (format-time-string "%H:%M:%S" (current-time))))

(defvar deferred:debug-on-signal nil
"If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro deferred:condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`deferred:debug-on-signal'."
  (declare (debug (symbolp form &rest form)))
  `(cond
    ((null deferred:debug-on-signal)
     (condition-case ,var ,protected-form ,@handlers))
    (t
     (let ((deferred:debug-on-signal-backup debug-on-signal))
       (setq debug-on-signal deferred:debug-on-signal)
       (unwind-protect
           (condition-case ,var ,protected-form ,@handlers)
         (setq debug-on-signal deferred:debug-on-signal-backup))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar deferred:tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this paramter. However, applications should not
modify it because the applications run on various environments.")

(defvar deferred:queue nil
  "[internal] The execution queue of deferred objects. 
See the functions `deferred:post-task' and `deferred:worker'.")

(defmacro deferred:pack (a b c)
  `(cons ,a (cons ,b ,c)))

(defun deferred:remove-from-queue (d)
  "[internal] Remove the deferred object from the execution queue
so that the deferred task will not be called twice."
  (setq deferred:queue 
        (loop for pack in deferred:queue
              unless (eq d (car pack))
              collect pack)))

(defun deferred:schedule-worker ()
  "[internal] Schedule consuming a deferred task in the execution queue."
  (run-at-time deferred:tick-time nil 'deferred:worker))

(defun deferred:post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`deferred:queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (deferred:remove-from-queue d)
  (push (deferred:pack d which arg) deferred:queue)
  (deferred:message "QUEUE-POST [%s]: %s" 
    (length deferred:queue) (deferred:pack d which arg))
  (deferred:schedule-worker)
  d)

(defun deferred:clear-queue ()
  "Clear the execution queue. For test and debugging."
  (interactive)
  (deferred:message "QUEUE-CLEAR [%s -> 0]" (length deferred:queue))
  (setq deferred:queue nil))

(defun deferred:esc-msg (msg)
  "[internal] Escaping the character '%'."
  (replace-regexp-in-string
   "\\([^%]\\|^\\)%\\([^%]\\)" "\\1%%\\2" msg))

(defun deferred:worker ()
  "[internal] Consume a deferred task. 
Mainly this function is called by timer asynchronously."
  (when deferred:queue
    (let* ((pack (car (last deferred:queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq deferred:queue (nbutlast deferred:queue))
      (condition-case err
          (setq value (deferred:exec-task d which arg))
        (error 
         (deferred:message "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

(defun deferred:flush-queue! ()
  "Call all deferred tasks synchronously. For test and debugging."
  (let (value)
    (while deferred:queue
      (setq value (deferred:worker)))
    value))

(defun deferred:sync! (d)
  "Wait for the given deferred task. For test and debugging."
  (progn 
    (lexical-let ((last-value 'deferred:undefined*))
      (deferred:nextc d
        (lambda (x) (setq last-value x)))
      (while (eq 'deferred:undefined* last-value)
        (sit-for 0.05)
        (sleep-for 0.05))
      last-value)))



;; Struct: deferred
;; 
;; callback    : a callback function (default `deferred:default-callback')
;; errorback   : an errorback function (default `deferred:default-errorback')
;; cancel      : a canceling function (default `deferred:default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;; 
(defstruct deferred
  (callback 'deferred:default-callback)
  (errorback 'deferred:default-errorback)
  (cancel 'deferred:default-cancel)
  next status value)

(defun deferred:default-callback (i)
  "[internal] Default callback function."
  (identity i))

(defun deferred:default-errorback (error-msg)
  "[internal] Default errorback function."
  (error (deferred:esc-msg error-msg)))

(defun deferred:default-cancel (d)
  "[internal] Default canceling function."
  (deferred:message "CANCEL : %s" d)
  (setf (deferred-callback d) 'deferred:default-callback)
  (setf (deferred-errorback d) 'deferred:default-errorback)
  (setf (deferred-next d) nil)
  d)

(defun deferred:exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (deferred:message "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "deferred:exec-task was given a nil."))
  (let ((callback (if (eq which 'ok) 
                      (deferred-callback d)
                    (deferred-errorback d))))
    (cond
     (callback
      (let (value (next-deferred (deferred-next d)))
        (deferred:condition-case err
            (progn 
              (setq value
                    (deferred:call-lambda callback arg))
              (cond
               ((deferred-p value)
                (deferred:message "WAIT NEST : %s" value)
                (if next-deferred
                    (deferred:set-next value next-deferred)
                  value))
               (t
                (if next-deferred
                    (deferred:post-task next-deferred 'ok value)
                  (setf (deferred-status d) 'ok)
                  (setf (deferred-value d) value)
                  value))))
          (error 
           (cond
            (next-deferred
             (deferred:post-task next-deferred 'ng (error-message-string err)))
            (deferred:onerror
              (deferred:call-lambda deferred:onerror err))
            (t
             (deferred:message "ERROR : %s" err)
             (message "deferred error : %s" err)
             (setf (deferred-status d) 'ng)
             (setf (deferred-value d) (error-message-string err))
             (deferred:esc-msg (error-message-string err))))))))
     (t ; <= (null callback)
      (let ((next-deferred (deferred-next d)))
        (cond
         (next-deferred
          (deferred:exec-task next-deferred which arg))
         ((eq which 'ok) arg)
         (t (error (deferred:esc-msg arg)))))))))

(defun deferred:set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (deferred-next prev) next)
  (cond
   ((eq 'ok (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (deferred:exec-task 
                 next 'ok (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   ((eq 'ng (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (deferred:exec-task next 'ng (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   (t
    next)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions for deferred objects

(defun deferred:new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-deferred :callback callback)
    (make-deferred)))

(defun deferred:callback (d &optional arg)
  "Start deferred chain with a callback message."
  (deferred:exec-task d 'ok arg))

(defun deferred:errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (deferred:exec-task d 'ng arg))

(defun deferred:callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (deferred:post-task d 'ok arg))

(defun deferred:errorback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (deferred:post-task d 'ng arg))

(defun deferred:cancel (d)
  "Cancel all callbacks and deferred chain in the deferred object."
  (deferred:message "CANCEL : %s" d)
  (funcall (deferred-cancel d) d)
  d)

(defun deferred:status (d)
  "Return a current status of the deferred object. The returned value means following:
`ok': the callback was called and waiting for next deferred.
`ng': the errorback was called and waiting for next deferred.
 nil: The neither callback nor errorback was not called."
  (deferred-status d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic utility functions

(defvar deferred:onerror nil 
  "Default error handler. This value is nil or a function that
  have one argument for the error message.")

(defun deferred:succeed (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (deferred:new)))
    (deferred:exec-task d 'ok arg)
    d))

(defun deferred:fail (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (deferred:new)))
    (deferred:exec-task d 'ok arg)
    d))

(defun deferred:next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (deferred:callback-post (deferred:new callback))."
  (let ((d (if callback
               (make-deferred :callback callback)
             (make-deferred))))
    (deferred:callback-post d arg)
    d))

(defun deferred:nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (let ((nd (make-deferred :callback callback)))
    (deferred:set-next d nd)))

(defun deferred:error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (let ((nd (make-deferred :errorback callback)))
    (deferred:set-next d nd)))

(defun deferred:wait (msec)
  "Return a deferred object scheduled at MSEC millisecond later."
  (lexical-let 
      ((d (deferred:new)) (start-time (float-time)) timer)
    (deferred:message "WAIT : %s" msec)
    (setq timer (deferred:setTimeout 
                  (lambda () 
                    (deferred:exec-task d 'ok 
                      (* 1000.0 (- (float-time) start-time)))
                    nil) msec))
    (setf (deferred-cancel d) 
          (lambda (x) 
            (deferred:cancelTimeout timer)
            (deferred:default-cancel x)))
    d))

(defun deferred:call (f &rest args)
  "Call the given function asynchronously."
  (lexical-let ((f f) (args args))
    (deferred:next
      (lambda (x)
        (apply f args)))))

(defun deferred:apply (f &optional args)
  "Call the given function asynchronously."
  (lexical-let ((f f) (args args))
    (deferred:next
      (lambda (x)
        (apply f args)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun deferred:empty-p (times-or-list)
  (or (and (numberp times-or-list) (<= times-or-list 0))
      (and (listp times-or-list) (null times-or-list))))

(defun deferred:loop (times-or-list func)
  "Return a iteration deferred object."
  (deferred:message "LOOP : %s" times-or-list)
  (if (deferred:empty-p times-or-list) (deferred:next)
    (lexical-let*
        (items (rd 
                (cond
                 ((numberp times-or-list)
                  (loop for i from 0 below times-or-list
                        with ld = (deferred:next)
                        do 
                        (push ld items)
                        (setq ld 
                              (lexical-let ((i i) (func func))
                                (deferred:nextc ld (lambda (x) (deferred:call-lambda func i)))))
                        finally return ld))
                 ((listp times-or-list)
                  (loop for i in times-or-list
                        with ld = (deferred:next)
                        do 
                        (push ld items)
                        (setq ld 
                              (lexical-let ((i i) (func func))
                                (deferred:nextc ld (lambda (x) (deferred:call-lambda func i)))))
                        finally return ld)))))
      (setf (deferred-cancel rd)
            (lambda (x) (deferred:default-cancel x)
              (loop for i in items
                    do (deferred:cancel i))))
      rd)))

(defun deferred:trans-multi-args (args self-func list-func main-func)
  "[internal] Check the argument values and dispatch to methods."
  (cond
   ((and (= 1 (length args)) (consp (car args)) (not (functionp (car args))))
    (let ((lst (car args)))
      (cond
       ((or (null lst) (null (car lst)))
        (deferred:next))
       ((deferred:aand lst (car it) (or (functionp it) (deferred-p it)))
        ;; a list of deferred objects
        (funcall list-func lst))
       ((deferred:aand lst (consp it))
        ;; an alist of deferred objects
        (funcall main-func lst))
       (t (error "Wrong argument type. %s" args)))))
   (t (funcall self-func args))))

(defun deferred:parallel-array-to-alist (lst)
  "[internal] Translation array to alist."
  (loop for d in lst
        for i from 0 below (length lst)
        collect (cons i d)))

(defun deferred:parallel-alist-to-array (alst)
  "[internal] Translation alist to array."
  (loop for pair in 
        (sort alst (lambda (x y)
                     (< (car x) (car y))))
        collect (cdr pair)))

(defun deferred:parallel-func-to-deferred (alst)
  "[internal] Normalization for parallel and earlier arguments."
  (loop for pair in alst 
        for d = (cdr pair)
        collect
        (progn 
          (unless (deferred-p d)
            (setf (cdr pair) (deferred:next d)))
          pair)))

(defun deferred:parallel-main (alst)
  "[internal] Deferred alist implementation for `deferred:parallel'. "
  (deferred:message "PARALLEL<KEY . VALUE>" )
  (lexical-let ((nd (deferred:new))
                (len (length alst))
                values)
    (loop for pair in 
          (deferred:parallel-func-to-deferred alst)
          with cd ; current child deferred
          do 
          (lexical-let ((name (car pair)))
            (setq cd
                  (deferred:nextc (cdr pair)
                    (lambda (x)
                      (push (cons name x) values)
                      (deferred:message "PARALLEL VALUE [%s/%s] %s" 
                        (length values) len (cons name x))
                      (when (= len (length values))
                        (deferred:message "PARALLEL COLLECTED")
                        (deferred:post-task nd 'ok (nreverse values)))
                      nil)))
            (deferred:error cd
              (lambda (e)
                (push (cons name e) values)
                (deferred:message "PARALLEL ERROR [%s/%s] %s" 
                  (length values) len (cons name e))
                (when (= (length values) len)
                  (deferred:message "PARALLEL COLLECTED")
                  (deferred:post-task nd 'ok (nreverse values)))
                nil))))
    nd))

(defun deferred:parallel-list (lst)
  "[internal] Deferred list implementation for `deferred:parallel'. "
  (deferred:message "PARALLEL<LIST>" )
  (lexical-let*
      ((pd (deferred:parallel-main (deferred:parallel-array-to-alist lst)))
       (rd (deferred:nextc pd 'deferred:parallel-alist-to-array)))
    (setf (deferred-cancel rd)
          (lambda (x) (deferred:default-cancel x)
            (deferred:cancel pd)))
    rd))

(defun deferred:parallel (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for all callbacks. The following
deferred task will be called with an array of the return
values. ARGS can be a list or an alist of deferred objects or
functions."
  (deferred:message "PARALLEL : %s" args)
  (deferred:trans-multi-args args 
    'deferred:parallel 'deferred:parallel-list 'deferred:parallel-main))

(defun deferred:earlier-main (alst)
  "[internal] Deferred alist implementation for `deferred:earlier'. "
  (deferred:message "EARLIER<KEY . VALUE>" )
  (lexical-let ((nd (deferred:new)) 
                (len (length alst))
                value results)
    (loop for pair in 
          (deferred:parallel-func-to-deferred alst)
          with cd ; current child deferred
          do 
          (lexical-let ((name (car pair)))
            (setq cd
                  (deferred:nextc (cdr pair)
                    (lambda (x)
                      (push (cons name x) results)
                      (cond
                       ((null value)
                        (setq value (cons name x))
                        (deferred:message "EARLIER VALUE %s" (cons name value))
                        (deferred:post-task nd 'ok value))
                       (t
                        (deferred:message "EARLIER MISS [%s/%s] %s" (length results) len (cons name value))
                        (when (eql (length results) len)
                          (deferred:message "EARLIER COLLECTED"))))
                      nil)))
            (deferred:error cd
              (lambda (e)
                (push (cons name e) results)
                (deferred:message "EARLIER ERROR [%s/%s] %s" (length results) len (cons name e))
                (when (and (eql (length results) len) (null value))
                  (deferred:message "EARLIER FAILED")
                  (deferred:post-task nd 'ok nil))
                nil))))
    nd))

(defun deferred:earlier-list (lst)
  "[internal] Deferred list implementation for `deferred:earlier'. "
  (deferred:message "EARLIER<LIST>" )
  (lexical-let*
      ((pd (deferred:earlier-main (deferred:parallel-array-to-alist lst)))
       (rd (deferred:nextc pd (lambda (x) (cdr x)))))
    (setf (deferred-cancel rd)
          (lambda (x) (deferred:default-cancel x)
            (deferred:cancel pd)))
    rd))


(defun deferred:earlier (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for the first callback. The
following deferred task will be called with the first return
value. ARGS can be a list or an alist of deferred objects or
functions."
  (deferred:message "EARLIER : %s" args)
  (deferred:trans-multi-args args 
    'deferred:earlier 'deferred:earlier-list 'deferred:earlier-main))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application functions

(defvar deferred:uid 0 "[internal] Sequence number for some utilities. See the function `deferred:uid'.")

(defun deferred:uid ()
  "[internal] Generate a sequence number."
  (incf deferred:uid))

(defun deferred:buffer-string (strformat buf)
  "[internal] Return a string in the buffer with the given format."
  (format strformat
          (with-current-buffer buf (buffer-string))))

(defun deferred:process (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout string from the command
process."
  (lexical-let
      ((pd (apply 'deferred:process-buffer command args)) d)
    (setq d (deferred:nextc pd
              (lambda (buf)
                (prog1 
                    (with-current-buffer buf (buffer-string))
                  (kill-buffer buf)))))
    (setf (deferred-cancel d)
          (lambda (x) 
            (deferred:default-cancel d)
            (deferred:default-cancel pd)))
    d))

(defun deferred:process-buffer (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout buffer from the command
process."
  (let ((d (deferred:next)) (uid (deferred:uid)))
    (lexical-let
        ((command command) (args args)
         (proc-name (format "*deferred:*%s*:%s" command uid))
         (buf-name (format " *deferred:*%s*:%s" command uid))
         (nd (deferred:new)) proc-buf proc)
      (deferred:nextc d
        (lambda (x)
          (setq proc-buf (get-buffer-create buf-name))
          (condition-case err
              (progn
                (setq proc
                      (if (null (car args))
                          (apply 'start-process proc-name buf-name command nil)
                        (apply 'start-process proc-name buf-name command args)))
                (set-process-sentinel
                 proc
                 (lambda (proc event)
                   (cond
                    ((string-match "exited abnormally" event)
                     (let ((msg (if (buffer-live-p proc-buf)
                                    (deferred:buffer-string "%s" proc-buf)
                                  (concat "NA:" proc-name))))
                       (kill-buffer proc-buf)
                       (deferred:post-task nd 'ng msg)))
                    ((equal event "finished\n")
                       (deferred:post-task nd 'ok proc-buf)))))
                (setf (deferred-cancel nd) 
                      (lambda (x) (deferred:default-cancel x)
                        (when proc
                          (kill-process proc)
                          (kill-buffer proc-buf)))))
            (error (deferred:post-task nd 'ng (error-message-string err))))
          nil))
      nd)))

(eval-after-load "url"
  ;; for url package
  ;; TODO: proxy, charaset
  '(progn

     (defun deferred:url-retrieve (url &optional cbargs)
       "A wrapper function for url-retrieve. The next deferred
object receives the buffer object that URL will load
into. Currently dynamic binding variables are not supported."
       (lexical-let ((nd (deferred:new)) (url url) (cbargs cbargs) buf)
         (deferred:next
           (lambda (x)
             (setq buf
                   (url-retrieve 
                    url (lambda (xx) (deferred:post-task nd 'ok buf))
                    cbargs))
             nil))
         (setf (deferred-cancel nd)
               (lambda (x) 
                 (when (buffer-live-p buf)
                   (kill-buffer buf))))
         nd))

     (defun deferred:url-delete-header (buf)
       (with-current-buffer buf
         (goto-char (point-min))
         (let ((pos (re-search-forward "\n\n")))
           (when pos
             (delete-region (point-min) pos))))
       buf)

     (defun deferred:url-delete-buffer (buf)
       (when (and buf (buffer-live-p buf))
         (kill-buffer buf))
       nil)

     (defun deferred:url-get (url &optional params)
       "Perform a HTTP GET method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. The next deferred
object receives the buffer object that URL will load into."
       (when params
         (setq url
               (concat url "?" (deferred:url-param-serialize params))))
       (let ((d (deferred:$
                  (deferred:url-retrieve url)
                  (deferred:nextc it 'deferred:url-delete-header))))
         (deferred:set-next
           d (deferred:new 'deferred:url-delete-buffer))
         d))

     (defun deferred:url-post (url &optional params)
       "Perform a HTTP POST method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. The next deferred
object receives the buffer object that URL will load into."
       (lexical-let ((nd (deferred:new)) 
                     (url url) (params params)
                     buf)
         (deferred:next
           (lambda (x)
             (let ((url-request-method "POST")
                   (url-request-extra-headers
                    '(("Content-Type" . "application/x-www-form-urlencoded")))
                   (url-request-data
                    (deferred:url-param-serialize params)))
               (setq buf 
                     (url-retrieve 
                      url 
                      (lambda (xx) (deferred:post-task nd 'ok buf)))))
             nil))
         (setf (deferred-cancel nd)
               (lambda (x) 
                 (when (buffer-live-p buf)
                   (kill-buffer buf))))
         (let ((d (deferred:nextc nd 'deferred:url-delete-header)))
           (deferred:set-next
             d (deferred:new 'deferred:url-delete-buffer))
           d)))

     (defun deferred:url-escape (val)
       "[internal] Return a new string that is VAL URI-encoded."
       (unless (stringp val)
         (setq val (format "%s" val)))
       (url-hexify-string 
        (encode-coding-string val 'utf-8)))

     (defun deferred:url-param-serialize (params)
       "[internal] Serialize a list of (key . value) cons cells
into a query string."
       (when params
         (mapconcat
          'identity
          (loop for p in params
                collect
                (cond
                 ((consp p)
                  (concat 
                   (deferred:url-escape (car p)) "="
                   (deferred:url-escape (cdr p))))
                 (t
                  (deferred:url-escape p))))
          "&")))
     ))


(provide 'deferred)
;;; deferred.el ends here
