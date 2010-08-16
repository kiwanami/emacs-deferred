(require 'el-expectations)
(require 'el-deferred)
(require 'cl)
(require 'pp)

(defmacro aand (test &rest rest)
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(aand ,@rest)) 'it))))

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

(defmacro waitc(d msec)
  `(deferred:waitc ,d ,msec))

(defmacro dloop(&rest body)
  `(deferred:loop ,@body))

(defmacro parallel(&rest args)
  `(deferred:parallel ,@args))

(defmacro parallelc(d &rest args)
  `(deferred:parallelc ,d ,@args))

(defmacro earlier(&rest args)
  `(deferred:earlier ,@args))

(defmacro earlierc(d &rest args)
  `(deferred:earlierc ,d ,@args))

(defmacro chain(&rest args)
  `(deferred:chain ,@args))

(defmacro chainc(d &rest args)
  `(deferred:chainc ,d ,@args))

(defmacro fire ()
  `(deferred:fire-queue!))

(defmacro clear ()
  `(setq deferred:queue nil))

(defmacro dtest (&rest form)
  `(progn 
     (clear)
     (lexical-let (last-value)
       (nextc
        (aand
         ,@form)
        (setq last-value x))
       (fire)
       last-value)))

(defun deferred:setTimeout (msec f)
  "overrided for test"
  (deferred:call f))

(defun deferred:cancelTimeout (id)
  "overrided for test"
  (when (deferred-p id)
    (deferred:cancel id)))



(dont-compile
  (when (fboundp 'expectations)

    (defun el-deferred:not-called-func (&optional m)
      (error "Must not be called!! %s" m))

    (expectations

     (desc ">>> Basic Test")
     (expect (true) 
             ;; function test
             (deferred:new))

     (expect (not-called el-deferred:not-called-func)
             ;; basic cancel test
             (let ((d (deferred:next 'el-deferred:not-called-func)))
               (cancelc d)
               (fire)))

     (expect (type vector)
             ;; basic post function test
             (clear)
             (lexical-let ((d (dnew)))
               (nextc d x)
               (deferred:post-message d 'ok "ok!")))

     (expect (type vector)
             ;; basic error post function test
             (clear)
             (lexical-let ((d (dnew)))
               (deferred:error d (lambda (e) e))
               (deferred:post-message d 'ng "error")))

     (desc ">>> Main Test")

     (expect '(2 1 0)
             ;; basic deferred chain test
             (clear)
             (lexical-let (vs)
               (aand (next (push 1 vs))
                     (nextc it (push 2 vs)))
               (push 0 vs)
               (fire)
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
              (nextc it (el-deferred:not-called-func "second errorback1"))
              (errorc it e)
              (errorc it (el-deferred:not-called-func "second errorback2"))
              (nextc it (error "second errorback called"))
              (nextc it "skipped")
              (errorc it e)))

     (expect "Child deferred chain"
             ;; child deferred chain test
             (dtest
              (next
               (next "Child deferred chain"))
              (errorf it "Error on simple chain : %s")))

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
              (waitc it 1)
              (nextc it (if (< x 300) "waitc ok" x))
              (errorf it "Error on simple wait chain : %s")))

     (expect nil
             ;; wait cancel test
             (dtest
              (wait 1000)
              (cancelc it)
              (nextc it (el-deferred:not-called-func "wait cancel"))))



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
                           (if (eql i 3) (dnew (push x count)))))
                (nextc it (format "nested loop ok %s" count))
                (errorf it "Error on simple loop calling : %s"))
               )
             )

     (expect nil
             ;; zero times loop test
             (dtest
              (dloop 0 (lambda (i) (el-deferred:not-called-func "zero loop")))))

     (expect nil
             ;; loop cancel test
             (dtest
              (dloop 3 (lambda (i) (el-deferred:not-called-func "loop cancel")))
              (cancelc it)))

     (desc "> parallel")
     (expect nil
             ;; nil test
             (dtest
              (parallel '())))

     (expect 100
             ;; nil test2
             (dtest
              (next 100)
              (parallelc it '())))

     (expect '(1)
             ;; single job test: argument
             (dtest
              (parallel 
               (next 1))
              (nextc it (reverse x))))

     (expect '(1)
             ;; single job test: function
             (dtest
              (parallel 
               (lambda (x) 1))
              (nextc it (reverse x))))

     (expect '(1)
             ;; single job test: list
             (dtest
              (parallel 
               (list (next 1)))
              (nextc it (reverse x))))

     (expect '((a . 1))
             ;; single job test: alist
             (dtest
              (parallel 
               (list (cons 'a (next 1))))
              (nextc it (reverse x))))

     (expect '(0 1)
             ;; simple parallel test: just return value
             (dtest
              (parallel 
               (next 0) (next 1))
              (nextc it (reverse x))))

     (expect '(11 12)
             ;; simple parallel test: argument
             (dtest
              (next 10)
              (parallelc it 
                         (next (+ x 1)) (next (+ x 2)))
              (nextc it (reverse x))))

     (expect '(13 14)
             ;; simple parallel test: list
             (dtest
              (next 10)
              (parallelc it 
                         (list (next (+ x 3))
                               (next (+ x 4))))
              (nextc it (reverse x))))

     (expect '((a . 20) (b . 30))
             ;; simple parallel test: alist
             (dtest
              (next 10)
              (parallelc it 
                         (list (cons 'a (next (+ x 10)))
                               (cons 'b (next (+ x 20)))))
              (nextc it (reverse x))))

     (expect '(0 1)
             ;; simple parallel test: function list
             (dtest
              (parallel 
               (lambda (x) 0) (lambda (x) 1))
              (nextc it (reverse x))))

     (expect '(0 1)
             ;; nested deferred and order change test
             (dtest
              (parallel
               (lambda (x) (dnew 0))
               (next 1))))

     (expect "(ERROR2 ERROR OK)"
             ;; error handling
             (dtest
              (parallel
               (next (error "ERROR")) (next "OK") (next (error "ERROR2")))
              (nextc it (format "%s" x))))

     (expect "(ERROR2 ERROR)"
             ;; failed test
             (dtest
              (parallel
               (next (error "ERROR")) (next (error "ERROR2")))
              (nextc it (format "%s" x))))

     (expect "((c . ERROR2) (a . ERROR) (b . OK))"
             ;; error handling
             (dtest
              (parallel
               (cons 'a (next (error "ERROR")))
               (cons 'b (next "OK"))
               (cons 'c (next (error "ERROR2"))))
              (nextc it (format "%s" x))))

     (expect "((b . ERROR2) (a . ERROR))"
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
               (list (next (el-deferred:not-called-func "parallel 1"))
                     (next (el-deferred:not-called-func "parallel 2"))))
              (cancelc it)))
     
     (desc "> earlier")
     (expect nil
             ;; nil test
             (dtest
              (earlier '())))

     (expect 100
             ;; nil test2
             (dtest
              (next 100)
              (earlierc it '())))

     (expect 1
             ;; single job test: argument
             (dtest
              (earlier 
               (next 1))
              (nextc it x)))

     (expect 1
             ;; single job test: function
             (dtest
              (earlier 
               (lambda (x) 1))
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
              (next 10)
              (earlierc it 
                         (next (+ x 1)) (next (+ x 2)))
              (nextc it x)))

     (expect '13
             ;; simple earlier test: list
             (dtest
              (next 10)
              (earlierc it 
                         (list (next (+ x 3)) (next (+ x 4))))
              (nextc it x)))

     (expect '(a . 20)
             ;; simple earlier test: alist
             (dtest
              (next 10)
              (earlierc it 
                         (list (cons 'a (next (+ x 10)))
                               (cons 'b (next (+ x 20)))))
              (nextc it x)))

     (expect '0
             ;; simple earlier test: function list
             (dtest
              (earlier 
               (lambda (x) 0) (lambda (x) 1))
              (nextc it x)))

     (expect '1
             ;; nested deferred and order change test
             (dtest
              (earlier
               (lambda (x) (dnew 0))
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
               (list (next (el-deferred:not-called-func "earlier 1"))
                     (next (el-deferred:not-called-func "earlier 2"))))
              (cancelc it)))
     
     (desc "> chain")

     (expect nil
             ;; nil test
             (dtest
              (chain '())))

     (expect "simple chain"
             ;; simple chain test
             (dtest
              (chain
               (list
                (lambda (x) "simple")
                (lambda (x) (concat x " chain"))))))

     (expect "1211"
             ;; parallel chain test
             (dtest
              (chain
               (list 
                (lambda (x) 10)
                (list
                 (lambda (x) (+ x 1))
                 (lambda (x) (+ x 2)))
                (lambda (ar) 
                  (apply 
                   'concat 
                   (mapcar 'number-to-string ar)))))))

     (expect "error ok!"
             ;; errorback chain test
             (dtest
              (chain
               (list
                (lambda (x) (error "error"))
                ':error (lambda (x) (concat x " ok"))
                (lambda (x) (concat x "!"))))))

     (expect "nested chain"
             ;; nested chain test
             (dtest
              (chain
               (list
                (lambda (x) (dnew "nested"))
                (lambda (x) (concat x " chain"))))))

     (expect nil
             ;; cancel test
             (dtest
              (chain
               (list
                (lambda (x) (el-deferred:not-called-func "chain 1"))
                (lambda (x) (el-deferred:not-called-func "chain 2"))))
              (cancelc it)))

     ) ;expectations
    ))

