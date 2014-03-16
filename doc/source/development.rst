=====================================================
 Resources for deferred.el/concurrent.el development
=====================================================

How to build document
=====================

Document of deferred.el and concurrent.el are build using Sphinx_ and
sphinxcontrib-emacs_.  It is available via `read the docs`_.

To build document, you need to install Sphinx_ and
sphinxcontrib-emacs_, for example, using `pip install`_:

.. sourcecode:: sh

   cd PATH/TO/emacs-deferred
   pip install Sphinx
   pip install -r doc/requirements.txt

Then, goto ``doc/`` directory and build document:

.. sourcecode:: sh

   cd doc/
   make html

You have html document in ``doc/build/html/``.

If autodocument does not fetch updated docstrings from ``*.el`` files,
use ``make clean html`` instead.

.. _sphinx: http://sphinx-doc.org/
.. _sphinxcontrib-emacs: https://github.com/flycheck/sphinxcontrib-emacs
.. _`read the docs`: http://emacs-deferred.readthedocs.org
.. _`pip install`:
   http://pip.readthedocs.org/en/latest/reference/pip_install.html
