;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; See LICENSE file for copyright details.
;;;; Simple FTP client using CL-FTP

(defpackage #:org.mapcar.ftp.simple-client
  (:use #:common-lisp
        #:org.mapcar.ftp.client)
  (:nicknames #:simple-client)
  (:export #:connect))

(in-package #:org.mapcar.ftp.simple-client)

(defparameter *command-table*
  '(("quit" ftp-quit "Quit the client")
    ("ls" ftp-list "List files (-l option for long form)")
    ("dir" ftp-long-list "List files, long form")
    ("cd" ftp-cd "Change current directory: cd [dir]")
    ("get" ftp-get "Get file: get remote-file [local-name]")
    ("put" ftp-put "Put file: put local-file [remote-name]")
    ("pwd" ftp-pwd "Print working directory")
    ("help" ftp-help "Help!")))

(defun ftp-help (conn args)
  (declare (ignorable conn args))
  (dolist (c *command-table*)
    (format t "~&~A: ~A~%" (first c) (third c))))

(defun ftp-pwd (conn args)
  (declare (ignorable args))
  (write-line (send-pwd-command conn)))

(defun ftp-get (conn args)
  (let ((remote (first args))
        (local (or (second args) (first args))))
    (if (retrieve-file conn remote local)
        (write-line "File transferred")
        (write-line "Something went wrong"))))

(defun ftp-put (conn args)
  (let ((remote (or (second args) (first args)))
        (local (first args)))
    (if (store-file conn local remote)
        (write-line "File transferred")
        (write-line "Something went wrong"))))

(defun ftp-cd (conn args)
  (write-line
   (data-to-string
    (send-cwd-command conn
                      (if (and args (stringp (first args)))
                          (first args)
                          "/")))))

(defun ftp-list (conn args)
  (when (find "-l" args :test #'string-equal)
    (ftp-long-list conn args))
  (send-nlst-command conn t))

(defun ftp-long-list (conn args)
  (declare (ignorable args))
  (send-list-command conn t))

(defun ftp-quit (conn args)
  (declare (ignorable conn args))
  (throw 'ftp-quit t))

(defun process-line (command)
  ;; Kinda ugly, but easy
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable))
        (parts nil)
        (stream (make-string-input-stream command)))
    (setf (readtable-case *readtable*) :preserve)
    (handler-case
        (loop (push (string (read stream)) parts))
      (end-of-file () nil))
    (nreverse parts)))

(defun ftp-shell (conn)
  (loop
   (format t "~&CL-FTP > ")
   (let* ((command (read-line))
          (scommand (process-line command))
          (fn (second (assoc (first scommand) *command-table*
                             :test #'string-equal))))
     (if fn
         (handler-case (funcall fn conn (rest scommand))
           (ftp-error (c)
             (format t "~&~A: ~A~%"
                     (ftp-error-code c)
                     (error-message c))))
         (format t "~&Unknown command!~%")))))

(defun connect (hostname &key (port 21) (username "anonymous") (password "cl-ftp@cclan.net"))
  (catch 'ftp-quit
    (with-ftp-connection (conn :hostname hostname
                               :port port
                               :username username
                               :password password)
      (ftp-shell conn))))

