;;; concurrent.el --- Concurrent utility functions for emacs lisp

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <sakurai at kiwanami.net>
;; Keywords: deferred, async, concurrent

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

;; 'concurrent.el' is a higher level library for concurrent tasks
;; based on 'deferred.el'. This library has following features:
;; 
;; - Generator
;; - Green thread
;; - Semaphore
;; - Dataflow
;; - Signal/Channel
;; 
;; 

(eval-when-compile
  (require 'cl))

(require 'deferred)

;;; Code:



(defmacro cc:aif (test-form then-form &rest else-forms)
  (declare (debug ("test-form" form "then-form" form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'cc:aif 'lisp-indent-function 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator 

(defun cc:generator-replace-yield (tree)
  (let (ret)
    (loop for i in tree
          do (cond
              ((eq i 'yield) 
               (push 'funcall ret)
               (push i ret))
              ((listp i)
               (push (cc:generator-replace-yield i) ret))
              (t
               (push i ret))))
    (nreverse ret)))

(defun cc:generator-line (line)
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (deferred:nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line)))
    `(setq ,chain 
      (deferred:nextc ,chain 
        (deferred:lambda (x) 
         (if ,condition 
             (deferred:nextc 
               (progn
                 ,@(cc:generator-replace-yield body)) self)))))))
   ;; statement
   (t
    `(setq ,chain 
           (deferred:nextc ,chain 
             (deferred:lambda (x) ,(cc:generator-replace-yield line)))))))

(defmacro cc:generator (callback &rest body)
  (let ((chain (gensym))
        (cc (gensym))
        (waiter (gensym)))
    `(lexical-let*
         (,chain
          (,cc ,callback)
          (,waiter (deferred:new))
          (yield (lambda (x) (funcall ,cc x) ,waiter)))
       (setq ,chain ,waiter)
       ,@(loop for i in body
               collect
               (cc:generator-line i))
       (lambda () (deferred:callback ,waiter)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread

(defun cc:thread-line (wait-time chain line)
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (deferred:nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line))
          (retsym (gensym)))
    `(setq ,chain 
      (deferred:nextc ,chain 
        (deferred:lambda (x) 
         (if ,condition 
             (deferred:nextc 
               (let ((,retsym (progn ,@body)))
                 (if (deferred-p ,retsym) ,retsym
                   (deferred:wait ,wait-time)))
               self)))))))
   ;; statement
   (t
    `(setq ,chain 
           (deferred:nextc ,chain 
             (lambda (x) ,line))))))

(defmacro cc:thread (wait-time &rest body)
  (let ((chain (gensym))
        (dstart (gensym)))
    `(lexical-let*
         (,chain
          (,dstart (deferred:new)))
       (setq ,chain ,dstart)
       ,@(loop for i in body
               collect
               (cc:thread-line wait-time chain i))
       (deferred:callback ,dstart))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semaphore

(defstruct cc:semaphore max-permits permits waiting-deferreds)

(defun cc:semaphore-create(permits-num)
  (make-cc:semaphore :max-permits permits-num :permits permits-num))

(defun cc:semaphore-aquire(semaphore)
  (cond
   ((< 0 (cc:semaphore-permits semaphore))
    (decf (cc:semaphore-permits semaphore))
    (deferred:succeed))
   (t
    (let ((d (deferred:new)))
      (push d (cc:semaphore-waiting-deferreds semaphore))
      d))))

(defun cc:semaphore-release(semaphore)
  (when (<= (cc:semaphore-max-permits semaphore) 
            (cc:semaphore-permits semaphore))
    (error "Too many calling semaphore-release. [max:%s <= permits:%s]" 
           (cc:semaphore-max-permits semaphore) 
           (cc:semaphore-permits semaphore)))
  (let ((waiting-deferreds
         (cc:semaphore-waiting-deferreds semaphore)))
    (cond
     (waiting-deferreds
      (let* ((d (car (last waiting-deferreds))))
        (setf (cc:semaphore-waiting-deferreds semaphore)
              (nbutlast waiting-deferreds))
        (deferred:callback-post d)))
     (t
      (incf (cc:semaphore-permits semaphore)))))
  semaphore)

(defun cc:semaphore-release-all (semaphore)
  (setf (cc:semaphore-permits semaphore)
        (cc:semaphore-max-permits semaphore))
  (let ((ds (cc:semaphore-waiting-deferreds semaphore)))
    (when ds
      (setf (cc:semaphore-waiting-deferreds semaphore) nil))
    ds))

(defun cc:semaphore-interrupt-all (semaphore)
  (when (cc:semaphore-waiting-deferreds semaphore)
    (setf (cc:semaphore-waiting-deferreds semaphore) nil)
    (setf (cc:semaphore-permits semaphore) 0))
  (cc:semaphore-aquire semaphore))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signal / Channel

(defun cc:signal-channel (&optional name parent-channel)
  (lexical-let
      ((ch (cons
            (or name (format "signal%s" (deferred:uid))) ; name for debug
            (cons
             parent-channel ; parent-channel
             nil)))) ; observers
    (when parent-channel
      (cc:signal-connect 
       parent-channel
       t (lambda (event)
           (destructuring-bind 
               (event-name event-args) event
             (apply 'cc:signal-send 
                    ch event-name event-args)))))
    ch))
       
(defmacro cc:signal-name (ch)
  `(car ,ch))

(defmacro cc:signal-parent-channel (ch)
  `(cadr ,ch))

(defmacro cc:signal-observers (ch)
  `(cddr ,ch))

(defun cc:signal-connect (channel event-sym &optional callback)
  (let ((d (if callback
               (deferred:new callback) 
             (deferred:new))))
    (push (cons event-sym d) 
          (cc:signal-observers channel))
    d))

(defun cc:signal-send (channel event-sym &rest args)
  (let ((observers (cc:signal-observers channel))
        (event (list event-sym args)))
    (loop for i in observers
          for name = (car i)
          for d = (cdr i)
          if (or (eq event-sym name) (eq t name))
          do (deferred:callback-post d event))))

(defun cc:signal-send-global (channel event-sym &rest args)
  (cc:aif (cc:signal-parent-channel channel)
      (apply 'cc:signal-send-global it event-sym args)
    (apply 'cc:signal-send channel event-sym args)))


(defun cc:signal-disconnect (channel deferred)
  (let ((observers (cc:signal-observers channel)) deleted)
    (setf 
     (cc:signal-observers channel) ; place
     (loop for i in observers
           for d = (cdr i)
           unless (eq d deferred)
           collect i
           else 
           do (push i deleted)))
    deleted))

(defun cc:signal-disconnect-all (channel)
  (setf 
   (cc:signal-observers channel) ; place
   nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dataflow

(defstruct cc:dataflow key (value 'cc:dataflow-undefine) deferred-list)

(defun cc:dataflow-undefine-p (obj)
  (eq 'cc:dataflow-undefine (cc:dataflow-value obj)))

(defun cc:dataflow-environment (&optional parent-env test-func channel)
  (let ((this (list parent-env
                    (or test-func 'equal)
                    (or channel 
                        (cc:signal-channel
                         'dataflow 
                         (and parent-env
                              (cc:dataflow-channel parent-env)))))))
    (cc:dataflow-init-connect this)
    this))

(defun cc:dataflow-init-connect (df)
  "[internal]"
  (lexical-let ((df df))
    (cc:dataflow-connect 
     df 'set
     (lambda (args)
       (destructuring-bind (event (key)) args
         (let* ((obj (cc:dataflow-get-object-for-value df key))
                (value (and obj (cc:dataflow-value obj))))
           (when obj
             (loop for i in (cc:aif (cc:dataflow-get-object-for-deferreds df key) 
                                (cc:dataflow-deferred-list it) nil)
                   do (deferred:callback-post i value))
             (setf (cc:dataflow-deferred-list obj) nil))))))))

(defmacro cc:dataflow-parent-environment (df)
  "[internal]"
  `(car ,df))

(defmacro cc:dataflow-test (df)
  "[internal]"
  `(cadr ,df))

(defmacro cc:dataflow-channel (df)
  "[internal]"
  `(caddr ,df))

(defmacro cc:dataflow-list (df)
  "[internal]"
  `(cdddr ,df))

(defun cc:dataflow-get-object-for-value (df key)
  "[internal]"
  (or
   (loop for i in (cc:dataflow-list df)
         with test = (cc:dataflow-test df)
         if (and (funcall test key (cc:dataflow-key i))
                 (not (cc:dataflow-undefine-p i)))
         return i)
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-object-for-value it key))))

(defun cc:dataflow-get-object-for-deferreds (df key)
  "[internal]"
  (loop for i in (cc:dataflow-list df)
        with test = (cc:dataflow-test df)
        if (funcall test key (cc:dataflow-key i))
        return i))

(defun cc:dataflow-connect (df event-sym &optional callback)
  (cc:signal-connect (cc:dataflow-channel df) event-sym callback))

(defun cc:dataflow-signal (df event &optional arg)
  (cc:signal-send (cc:dataflow-channel df) event arg))

(defun cc:dataflow-get (df key)
  (let ((obj (cc:dataflow-get-object-for-value df key)))
    (cond
     ((and obj (cc:dataflow-value obj))
      (cc:dataflow-signal df 'get key)
      (deferred:succeed (cc:dataflow-value obj)))
     (t
      (setq obj (cc:dataflow-get-object-for-deferreds df key))
      (unless obj
        (setq obj (make-cc:dataflow :key key))
        (push obj (cc:dataflow-list df))
        (cc:dataflow-signal df 'get-first key))
      (let ((d (deferred:new)))
        (push d (cc:dataflow-deferred-list obj))
        (cc:dataflow-signal df 'get-waiting key)
        d)))))

(defun cc:dataflow-get-sync (df key)
  (let ((obj (cc:dataflow-get-object-for-value df key)))
    (and obj (cc:dataflow-value obj))))

(defun cc:dataflow-set (df key value)
  (let ((obj (cc:dataflow-get-object-for-deferreds df key)))
    (cond
     ((and obj (not (cc:dataflow-undefine-p obj)))
      ;; overwrite!
      (error "Can not set a dataflow value. The key [%s] has already had a value. NEW:[%s] OLD:[%s]" key value (cc:dataflow-value obj)))
     (obj
      (setf (cc:dataflow-value obj) value))
     (t
      ;; just value arrived
      (push (make-cc:dataflow :key key :value value)
            (cc:dataflow-list df))))
    ;; value arrived and start deferred objects 
    (cc:dataflow-signal df 'set key)
    value))

(defun cc:dataflow-clear (df key)
  (cc:dataflow-signal df 'clear key)
  (setf (cc:dataflow-list df) 
        (loop for i in (cc:dataflow-list df)
              with test = (cc:dataflow-test df)
              unless (funcall test key (cc:dataflow-key i))
              collect i)))

(defun cc:dataflow-get-avalable-pairs (df)
  (append
   (loop for i in (cc:dataflow-list df)
         for key = (cc:dataflow-key i)
         for val = (cc:dataflow-value i)
         unless (cc:dataflow-undefine-p i) collect (cons key val))
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-avalable-pairs it))))

(defun cc:dataflow-get-waiting-keys (df)
  (append
   (loop for i in (cc:dataflow-list df)
         for key = (cc:dataflow-key i)
         for val = (cc:dataflow-value i)
         if (cc:dataflow-undefine-p i) collect key)
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-waiting-keys it))))

(defun cc:dataflow-clear-all (df)
  (cc:dataflow-signal df 'clear-all)
  (setf (cc:dataflow-list df) nil))


(provide 'concurrent)
;;; concurrent.el ends here

