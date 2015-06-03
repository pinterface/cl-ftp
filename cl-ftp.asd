;;;; -*- Mode: Lisp -*-
;;;; See LICENSE file for copyright details.

(asdf:defsystem "cl-ftp"
    :name "cl-ftp"
    :author "Matthew Danish <mdanish@andrew.cmu.edu>"
    :version "1.6.0"
    :maintainer "pinterface <pix@kepibu.org>"
    :licence "MIT/X style"
    :description "FTP library"
    :long-description "Provides FTP client functionality"
    :components ((:file "ftp"))
    :depends-on (split-sequence usocket))
