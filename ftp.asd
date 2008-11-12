;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; Minor modifications: pinterface <pix@kepibu.org>
;;;; See LICENSE file for copyright details.

(asdf:defsystem ftp
    :name "cl-ftp"
    :author "Matthew Danish <mdanish@andrew.cmu.edu>"
    :version "1.3.1"
    :maintainer "pinterface <pix@kepibu.org>"
    :licence "MIT/X style"
    :description "FTP library"
    :long-description "Provides FTP client functionality"
    :components ((:file "ftp"))
    :depends-on (split-sequence #-allegro acl-compat))

#+nil
(when (ignore-errors (find-class 'asdf:load-compiled-op))
  (defmethod perform :after ((op asdf:load-compiled-op) (c (eql (asdf:find-system 'ftp))))
             (pushnew :ftp cl:*features*)))
