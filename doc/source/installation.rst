==============
 Installation
==============

Installing from Melpa
=====================

If you have already used Melpa to install some other package then all
you have to do is::

  M-x package-install RET deferred RET

Installing from Marmalade
=========================

WRITE HERE

Installing from Git
===================

If you want to contribute to deferred and concurrent you should run it directly from
the Git reposiotry.

First get the repository::

  $ git clone git://github.com/kiwanami/emacs-deferred.git

Then add this to your init.el::

  (add-to-list 'load-path "/path/to/emacs-deferred")
  (require 'deferred)
  (require 'concurrent)

To build deferred and concurrent manually::

  WRITE HERE
