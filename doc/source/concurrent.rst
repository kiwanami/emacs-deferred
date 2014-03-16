======================
 concurrent.el manual
======================

API
===

.. el:require:: concurrent

Pseudo-thread
-------------

.. el:function:: cc:thread
   :auto:

Generator
---------

.. el:function:: cc:generator
   :auto:

Semaphore
---------

.. el:function:: cc:semaphore-create
   :auto:
.. el:function:: cc:semaphore-acquire
   :auto:
.. el:function:: cc:semaphore-release
   :auto:
.. el:function:: cc:semaphore-with
   :auto:
.. el:function:: cc:semaphore-release-all
   :auto:
.. el:function:: cc:semaphore-interrupt-all
   :auto:

Signal
------

.. el:function:: cc:signal-channel
   :auto:
.. el:function:: cc:signal-connect
   :auto:
.. el:function:: cc:signal-send
   :auto:
.. el:function:: cc:signal-send-global
   :auto:
.. el:function:: cc:signal-disconnect
   :auto:
.. el:function:: cc:signal-disconnect-all
   :auto:

Dataflow
--------

.. el:function:: cc:dataflow-environment
   :auto:
.. el:function:: cc:dataflow-get
   :auto:
.. el:function:: cc:dataflow-get-sync
   :auto:
.. el:function:: cc:dataflow-set
   :auto:
.. el:function:: cc:dataflow-clear
   :auto:
.. el:function:: cc:dataflow-get-avalable-pairs
   :auto:
.. el:function:: cc:dataflow-get-waiting-keys
   :auto:
.. el:function:: cc:dataflow-clear-all
   :auto:
.. el:function:: cc:dataflow-connect
   :auto:
