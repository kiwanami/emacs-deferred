;;; test code for deferred.el

;; Copyright (C) 2010  SAKURAI Masashi
;; Author: SAKURAI Masashi <m.sakurai@kiwanami.net>

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


(require 'el-expectations)
(require 'deferred)
(require 'cl)
(require 'pp)

(defmacro aand (test &rest rest)
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(aand ,@rest)) 'it))))

(defmacro $ (&rest elements)
  `(let (it)
     ,@(loop for i in elements
             with it = nil
             collect
             `(setq it ,i))
     it))

(defmacro dnew(&rest aforms)
  (if aforms
      `(deferred:new (lambda (x) ,@aforms))
    `(deferred:new)))

(defmacro next(&rest aforms)
  `(deferred:next (lambda (x) ,@aforms)))

(defmacro nextc(d &rest aforms)
  `(deferred:nextc ,d (lambda (x) ,@aforms)))

(defmacro errorc(d &rest aforms)
  `(deferred:error ,d (lambda (e) ,@aforms)))

(defmacro errorf(d formatstr)
  `(deferred:error ,d (lambda (e) (error ,formatstr e))))

(defmacro cancelc(d)
  `(deferred:cancel ,d))

(defmacro wait(msec)
  `(deferred:wait ,msec))

(defmacro dloop(&rest body)
  `(deferred:loop ,@body))

(defmacro parallel(&rest args)
  `(deferred:parallel ,@args))

(defmacro earlier(&rest args)
  `(deferred:earlier ,@args))

(defmacro flush ()
  `(deferred:flush-queue!))

(defmacro clear ()
  `(setq deferred:queue nil))

(defmacro dtest (&rest form)
  `(progn 
     (clear)
     (lexical-let (last-value)
       (nextc
        ($
         ,@form)
        (setq last-value x))
       (flush)
       last-value)))

(defmacro wtest (time &rest form)
  `(progn 
     (clear)
     (lexical-let (last-value)
       (nextc
        ($
         ,@form)
        (setq last-value x))
       (sit-for ,time)
       (flush)
       last-value)))

(defun deferred:setTimeout (f msec)
  "overrided for test"
  (deferred:call f))

(defun deferred:cancelTimeout (id)
  "overrided for test"
  (when (deferred-p id)
    (deferred:cancel id)))



(dont-compile
  (when (fboundp 'expectations)

    (defun deferred:not-called-func (&optional m)
      (error "Must not be called!! %s" m))

    (expectations

     (desc ">>> Basic Test")

     (expect 1 (deferred:call-lambda (lambda ()  1)))
     (expect 1 (deferred:call-lambda (lambda ()  1) 1))
     (expect 1 (deferred:call-lambda (lambda (x) 1)))
     (expect 1 (deferred:call-lambda (lambda (x) 1) 1))
     (expect 1 (deferred:call-lambda (deferred:lambda () 1)))
     (expect 1 (deferred:call-lambda (deferred:lambda () 1) 1))
     (expect nil (deferred:call-lambda 'car))
     (expect 2 (deferred:call-lambda 'car '(2 1)))
     (expect nil (deferred:call-lambda (symbol-function 'car)))
     (expect 2 (deferred:call-lambda (symbol-function 'car) '(2 1)))
     (expect 3 (lexical-let ((st 1))
                 (deferred:call-lambda
                   (lambda () (+ st 2)))))
     (expect 3  (lexical-let ((st 1))
                  (deferred:call-lambda
                    (lambda () (+ st 2)) 0)))
     (expect 3 (lexical-let ((st 1))
                 (deferred:call-lambda
                   (lambda (x) (+ st 2)))))
     (expect 3  (lexical-let ((st 1))
                  (deferred:call-lambda
                    (lambda (x) (+ st 2)) 0)))

     (expect (true) 
             ;; function test
             (deferred:new))

     (expect (not-called deferred:not-called-func)
             ;; basic cancel test
             (let ((d (deferred:next 'deferred:not-called-func)))
               (cancelc d)
               (flush)))

     (expect (type vector)
             ;; basic post function test
             (clear)
             (lexical-let ((d (dnew)))
               (nextc d x)
               (deferred:exec-task d 'ok "ok!")))

     (expect (type vector)
             ;; basic error post function test
             (clear)
             (lexical-let ((d (dnew)))
               (deferred:error d (lambda (e) e))
               (deferred:exec-task d 'ng "error")))

     (desc "> result propagation")
     (expect 'ok
             ;; value saving test
             (let ((d (deferred:succeed 1)))
               (deferred:status d)))

     (expect 1
             ;; value saving test
             (let ((d (deferred:succeed 1)))
               (deferred-value d)))

     (expect nil
             ;; value clearing test
             (let ((d (deferred:succeed 1)))
               (deferred:set-next d (dnew))
               (deferred:status d)))

     (expect 1
             ;; value propagating test
             (let ((d (deferred:succeed 1))
                   (nd (dnew)))
               (deferred:set-next d nd)
               (deferred-value nd)))

     (desc "> error propagation")
     (expect 'ok
             ;; value saving test
             (let ((d (deferred:succeed 1)))
               (deferred:status d)))

     (expect 1
             ;; value saving test
             (let ((d (deferred:succeed 1)))
               (deferred-value d)))

     (expect nil
             ;; value clearing test
             (let ((d (deferred:succeed 1)))
               (deferred:set-next d (dnew))
               (deferred:status d)))

     (expect 1
             ;; value propagating test
             (let ((d (deferred:succeed 1))
                   (nd (dnew)))
               (deferred:set-next d nd)
               (deferred-value nd)))

     (desc ">>> Main Test")

     (expect '(2 1 0)
             ;; basic deferred chain test
             (clear)
             (lexical-let (vs)
               ($ (next (push 1 vs))
                  (nextc it (push 2 vs)))
               (push 0 vs)
               (flush)
               vs))

     (desc ">>> Test callback, errorback chain")
     (expect "errorback called"
             ;; basic errorback test
             (dtest (next (error "errorback"))
                    (errorc it (concat e " called"))))

     (expect "next callback called"
             ;; error recovery test
             (dtest
              (next (error "callback called"))
              (errorc it e)
              (nextc it (concat "next " x))))

     (expect "second errorback called"
             ;; error recovery test 2
             (dtest
              (next (error "callback called"))
              (nextc it (deferred:not-called-func "second errorback1"))
              (errorc it e)
              (errorc it (deferred:not-called-func "second errorback2"))
              (nextc it (error "second errorback called"))
              (nextc it "skipped")
              (errorc it e)))

     (expect "Child deferred chain"
             ;; child deferred chain test
             (dtest
              (next
               (next "Child deferred chain"))
              (errorf it "Error on simple chain : %s")))

     (desc "> async connect")
     
     (expect "saved result!"
             ;; asynchronously connect deferred and propagate a value
             (let (d ret)
               (clear)
               (setq d (next "saved "))
               (deferred:callback d)
               (flush)
               (setq d (nextc d (concat x "result")))
               (nextc d (setq ret (concat x "!")))
               ret))

     (desc "> global onerror")

     (expect "ONERROR"
             ;; default onerror handler test
             (let (ret (deferred:onerror 
                         (lambda (e) (setq ret (concat "ON" (error-message-string e))))))
               (dtest
                (next (error "ERROR"))) 
               ret))

     (desc "> async call")
     (expect "ASYNC CALL"
             ;; basic async 'call' test
             (dtest
              (deferred:call 'concat "ASYNC" " " "CALL")))

     (expect "ASYNC APPLY"
             ;; basic async 'apply' test
             (dtest
              (deferred:apply 'concat '("ASYNC" " " "APPLY"))))

     (desc "> wait")
     (expect "wait ok"
             ;; basic wait test
             (dtest
              (wait 1)
              (nextc it (if (< x 300) "wait ok" x))
              (errorf it "Error on simple wait : %s")))

     (expect "waitc ok"
             ;; wait chain test
             (dtest
              (wait 1)
              (nextc it "wait")
              (nextc it (wait 1))
              (nextc it (if (< x 300) "waitc ok" x))
              (errorf it "Error on simple wait chain : %s")))

     (expect nil
             ;; wait cancel test
             (dtest
              (wait 1000)
              (cancelc it)
              (nextc it (deferred:not-called-func "wait cancel"))))



     (desc ">>> Utility Functions Tests")

     (desc "> loop")
     (expect 10
             ;; basic loop test
             (lexical-let ((v 0))
               (dtest
                (dloop 5 (lambda (i) (setq v (+ v i))))
                (errorf it "Error on simple loop calling : %s"))
               v))

     (expect "loop ok 4"
             ;; return value for a loop
             (dtest
              (dloop 5 (lambda (i) i))
              (nextc it (format "loop ok %i" x))
              (errorf it "Error on simple loop calling : %s")))

     (expect "nested loop ok (4 nil 3 2 1 0)"
             ;; nested deferred task in a loop 
             (lexical-let (count)
               (dtest
                (dloop 5 (lambda (i) 
                           (push i count)
                           (if (eql i 3) (next (push x count)))))
                (nextc it (format "nested loop ok %s" count))
                (errorf it "Error on simple loop calling : %s"))
               )
             )

     (expect '(6 4 2)
             ;; do-loop test
             (lexical-let (count)
               (dtest
                (dloop '(1 2 3)
                       (lambda (x) (push (* 2 x) count)))
                (errorf it "Error on do-loop calling : %s"))))

     (expect nil
             ;; zero times loop test
             (dtest
              (dloop 0 (lambda (i) (deferred:not-called-func "zero loop")))))

     (expect nil
             ;; loop cancel test
             (dtest
              (dloop 3 (lambda (i) (deferred:not-called-func "loop cancel")))
              (cancelc it)))

     (expect "loop error!"
             ;; loop error recover test
             (dtest
              (deferred:loop 5
                (lambda (i) (if (= 2 i) (error "loop error"))))
              (nextc it (deferred:not-called-func))
              (errorc it (format "%s!" e))
              (nextc it x)))

     (expect "loop error catch ok"
             ;; try catch finally test
             (lexical-let ((body (lambda ()
                                   (deferred:loop 5
                                     (lambda (i) (if (= 2 i) (error "loop error")))))))
               (dtest
                (next  "try ") ; try
                (nextc it (funcall body)) ; body
                (errorc it (format "%s catch " e)) ; catch 
                (nextc it (concat x "ok"))))) ; finally

     (expect "4 ok"
             ;; try catch finally test
             (lexical-let ((body (lambda ()
                                   (deferred:loop 5
                                     (lambda (i) i)))))
               (dtest
                (next  "try ") ; try
                (nextc it (funcall body)) ; body
                (errorc it (format "%s catch " e)) ; catch 
                (nextc it (format "%s ok" x))))) ; finally

     (desc "> parallel")
     (expect nil
             ;; nil test
             (dtest
              (parallel '())))

     (expect '(1)
             ;; single job test: argument
             (dtest
              (parallel 
               (next 1))))

     (expect '(1)
             ;; single job test: function
             (dtest
              (parallel 
               (lambda () 1))))

     (expect '(1)
             ;; single job test: list
             (dtest
              (parallel 
               (list (next 1)))))

     (expect '((a . 1))
             ;; single job test: alist
             (dtest
              (parallel 
               (list (cons 'a (next 1))))))

     (expect '(0 1)
             ;; simple parallel test: just return value
             (dtest
              (parallel 
               (next 0) (next 1))))

     (expect '(13 14)
             ;; simple parallel test: list
             (dtest
              (parallel
               (list (next 13)
                     (next 14)))))

     (expect '((a . 20) (b . 30))
             ;; simple parallel test: alist
             (dtest
              (parallel
               (list (cons 'a (next 20))
                     (cons 'b (next 30))))))

     (expect '(0 1)
             ;; simple parallel test: function list
             (dtest
              (parallel 
               (lambda () 0) (lambda () 1))))

     (expect '(0 1)
             ;; nested deferred and order change test
             (dtest
              (parallel
               (lambda () (next 0))
               (next 1))))

     (expect "(ERROR OK ERROR2)"
             ;; error handling
             (dtest
              (parallel
               (next (error "ERROR")) (next "OK") (next (error "ERROR2")))
              (nextc it (format "%s" x))))

     (expect "(ERROR ERROR2)"
             ;; failed test
             (dtest
              (parallel
               (next (error "ERROR")) (next (error "ERROR2")))
              (nextc it (format "%s" x))))

     (expect "((b . OK) (a . ERROR) (c . ERROR2))"
             ;; error handling
             (dtest
              (parallel
               (cons 'a (next (error "ERROR")))
               (cons 'b (next "OK"))
               (cons 'c (next (error "ERROR2"))))
              (nextc it (format "%s" x))))

     (expect "((a . ERROR) (b . ERROR2))"
             ;; failed test
             (dtest
              (parallel
               (cons 'a (next (error "ERROR")))
               (cons 'b (next (error "ERROR2"))))
              (nextc it (format "%s" x))))

     (expect nil
             ;; parallel cancel test
             (dtest
              (parallel
               (list (next (deferred:not-called-func "parallel 1"))
                     (next (deferred:not-called-func "parallel 2"))))
              (cancelc it)))

     (expect "nest parallel ok"
             ;; parallel next
             (lexical-let* ((flow (lambda (x)
                                    (parallel
                                     (next "nest ") 
                                     (next "parallel ")))))
               (dtest
                (next  "start ")
                (nextc it (funcall flow x))
                (nextc it (apply 'concat x))
                (nextc it (concat x "ok")))))

     (desc "> earlier")
     (expect nil
             ;; nil test
             (dtest
              (earlier '())))

     (expect 1
             ;; single job test: argument
             (dtest
              (earlier 
               (nextc (wait 10) 1))
              (nextc it x)))

     (expect 1
             ;; single job test: function
             (dtest
              (earlier 
               (lambda () 1))
              (nextc it x)))

     (expect 1
             ;; single job test: list
             (dtest
              (earlier 
               (list (next 1)))
              (nextc it x)))

     (expect '(a . 1)
             ;; single job test: alist
             (dtest
              (earlier 
               (list (cons 'a (next 1))))
              (nextc it x)))

     (expect '0
             ;; simple earlier test
             (dtest
              (earlier 
               (next 0) (next 1))
              (nextc it x)))

     (expect '11
             ;; simple earlier test: argument
             (dtest
              (earlier
               (next 11) (next 12))
              (nextc it x)))

     (expect '13
             ;; simple earlier test: list
             (dtest
              (earlier
               (list (next 13) (next 14)))
              (nextc it x)))

     (expect '(a . 20)
             ;; simple earlier test: alist
             (dtest
              (earlier
               (list (cons 'a (next 20))
                     (cons 'b (next 30))))
              (nextc it x)))

     (expect '0
             ;; simple earlier test: function list
             (dtest
              (earlier 
               (lambda () 0) (lambda () 1))
              (nextc it x)))

     (expect '1
             ;; nested deferred and order change test
             (dtest
              (earlier
               (lambda () (dnew 0))
               (next 1))))

     (expect "OK"
             ;; error handling
             (dtest
              (earlier
               (next (error "ERROR")) (next "OK") (next (error "ERROR2")))
              (nextc it x)))

     (expect nil
             ;; failed test
             (dtest
              (earlier
               (next (error "ERROR")) (next (error "ERROR2")))
              (nextc it x)))

     (expect '(b . "OK")
             ;; error handling
             (dtest
              (earlier
               (cons 'a (next (error "ERROR")))
               (cons 'b (next "OK"))
               (cons 'c (next (error "ERROR2"))))
              (nextc it x)))

     (expect nil
             ;; failed test
             (dtest
              (earlier
               (cons 'a (next (error "ERROR")))
               (cons 'b (next (error "ERROR2"))))
              (nextc it x)))

     (expect nil
             ;; cancel test
             (dtest
              (earlier
               (list (next (deferred:not-called-func "earlier 1"))
                     (next (deferred:not-called-func "earlier 2"))))
              (cancelc it)))
     
     (desc ">>>Application")

     (expect 
      (with-temp-buffer 
        (call-process "pwd" nil t nil)
        (buffer-string))
      (wtest 0.1 ;; maybe fail in some environments...
       (deferred:process "pwd")))
      
     (expect "Searching for program: no such file or directory, pwd---"
             (dtest
              (deferred:process "pwd---")
              (nextc it (deferred:not-called-func))
              (errorc it e)))

     ) ;expectations
    ))

