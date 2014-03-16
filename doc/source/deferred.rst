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


API
===

.. el:require:: deferred

.. el:function:: deferred:next
   :auto:
.. el:function:: deferred:nextc
   :auto:
.. el:function:: deferred:error
   :auto:
.. el:function:: deferred:cancel
   :auto:
.. el:function:: deferred:watch
   :auto:
.. el:function:: deferred:wait
   :auto:
.. el:function:: deferred:$
   :auto:
.. el:function:: deferred:loop
   :auto:
.. el:function:: deferred:parallel
   :auto:
.. el:function:: deferred:earlier
   :auto:
.. el:function:: deferred:call
   :auto:
.. el:function:: deferred:apply
   :auto:
.. el:function:: deferred:process
   :auto:
.. el:function:: deferred:process-shell
   :auto:
.. el:function:: deferred:process-buffer
   :auto:
.. el:function:: deferred:process-shell-buffer
   :auto:
.. el:function:: deferred:wait-idle
   :auto:
.. el:function:: deferred:url-retrieve
   :auto:
.. el:function:: deferred:url-get
   :auto:
.. el:function:: deferred:url-post
   :auto:
.. el:function:: deferred:new
   :auto:
.. el:function:: deferred:succeed
   :auto:
.. el:function:: deferred:fail
   :auto:
.. el:function:: deferred:callback
   :auto:
.. el:function:: deferred:callback-post
   :auto:
.. el:function:: deferred:errorback
   :auto:
.. el:function:: deferred:errorback-post
   :auto:
.. el:function:: deferred:try
   :auto:
.. el:function:: deferred:timeout
   :auto:
.. el:function:: deferred:processc
   :auto:
.. el:function:: deferred:process-bufferc
   :auto:
.. el:function:: deferred:process-shellc
   :auto:
.. el:function:: deferred:process-shell-bufferc
   :auto:
