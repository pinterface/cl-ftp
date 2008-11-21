;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; See LICENSE file for copyright details.
;;;; FTP client functionality

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)) ; just in case

(defpackage #:org.mapcar.ftp.client
  (:use #:common-lisp
        #:split-sequence
        #+allegro #:socket
        #-allegro #:acl-socket)
  (:nicknames #:ftp.client #:ftp)
  (:export #:ftp-connection
           #:with-ftp-connection
           #:connect-to-server
           #:close-connection
           #:send-list-command
           #:send-nlst-command
           #:with-transfer-socket
           #:call-with-transfer-socket
           #:ftp-error
           #:invalid-code
           #:transient-negative-completion
           #:permanent-negative-completion
           #:ftp-error-code
           #:error-message
           #:expected
           #:received
           #:passive-ftp-p
           #:code-cut-off-p
           #:ftp-hostname
           #:ftp-port
           #:ftp-username
           #:ftp-password
           #:ftp-session-stream
           #:data-to-string
           #:retrieve-file
           #:store-file
           #:receive-response
           #:data-ready-p
           #:retrieve-filename-list
           #:retrieve-file-info-list))

(in-package #:org.mapcar.ftp.client)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition ftp-error ()
    ((ftp-error-code :initarg :ftp-error-code
                     :initform "\"unspecified\""
                     :reader ftp-error-code)
     (error-message :initarg :error-message
                    :initform "\"unspecified\""
                    :reader error-message))
    (:report (lambda (c s)
               (format s "FTP error ~A raised: ~A"
                       (ftp-error-code c)
                       (error-message c)))))

  (define-condition invalid-code (ftp-error)
    ((expected :reader expected :initarg :expected)
     (received :reader received :initarg :received))
    (:report (lambda (c s)
               (format s "Expected FTP code ~A, got FTP code ~A"
                       (expected c)
                       (received c)))))
  
  (define-condition transient-negative-completion (ftp-error)
    ()
    (:report (lambda (c s)
               (format s "Received transient error code ~A: ~A"
                       (ftp-error-code c)
                       (error-message c)))))

  (define-condition permanent-negative-completion (ftp-error)
    ()
    (:report (lambda (c s)
               (format s "Received permanent error code ~A: ~A"
                       (ftp-error-code c)
                       (error-message c)))))
  
  (defclass ftp-connection ()
    ((hostname :initarg :hostname
               :reader ftp-hostname)
     (port :initarg :port :initform 21
           :reader ftp-port)
     (username :initarg :username :initform "anonymous"
               :reader ftp-username)
     (password :initarg :password :initform "cl-ftp@cclan.net"
               :reader ftp-password)
     (session-stream :initarg :session-stream :initform nil
                     :reader ftp-session-stream)
     (passive-ftp-p :initarg :passive-ftp-p :initform nil
                    :accessor passive-ftp-p)
     (code-cut-off-p :initarg :code-cut-off-p :initform t
                     :accessor code-cut-off-p)
     (socket))))

(defmacro with-ftp-connection-slots ((conn) &body body)
  `(with-slots (socket hostname port username password session-stream passive-ftp-p code-cut-off-p) ,conn
    ,@body))
            
(defmethod print-object ((obj ftp-connection) stream)
  (with-ftp-connection-slots (obj)
    (print-unreadable-object (obj stream)
      (format stream "FTP connection to ~A:~A username: ~A"
              hostname port username))))

(defun raise-ftp-error (error-code error-msg &key (expected-code nil))
  (cond ((and (>= error-code 400)
              (< error-code 500))
         (error 'transient-negative-completion
                :ftp-error-code error-code
                :error-message error-msg))
        ((and (>= error-code 500)
              (< error-code 600))
         (error 'permanent-negative-completion
                :ftp-error-code error-code
                :error-message error-msg))
        (expected-code
         (error 'invalid-code
                :expected expected-code
                :received error-code
                :ftp-error-code error-code
                :error-message error-msg))
        (t
         (error 'ftp-error
                :ftp-error-code error-code
                :error-message error-msg))))

(defun data-to-string (data)
  (format nil "~{~A~%~}" data))

(defmethod expect-code-or-lose ((conn ftp-connection) (expected-code integer))
  (multiple-value-bind (data code)
      (receive-response conn :block t)
    (unless (eql code expected-code)
      (raise-ftp-error code (data-to-string data)
                       :expected-code expected-code))
    data))

(defmethod initialize-instance :after ((conn ftp-connection) &rest initargs)
  (declare (ignorable initargs))
  (connect-to-server conn))

(defmethod connect-to-server ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (unless (and hostname port (integerp port) (stringp hostname))
      (error "You must specify a hostname string and an integer port"))
    (when (and (slot-boundp conn 'socket) (streamp socket))
      (close socket))
    (setf socket (make-socket :remote-host hostname :remote-port port))
    (unless socket
      (error "Error connecting to ~A:~A" hostname port))
    (when (and username password (stringp username) (stringp password))
      (expect-code-or-lose conn 220)
      (send-raw-line conn
                     (format nil "USER ~A" username))
      (expect-code-or-lose conn 331)
      (send-raw-line conn
                     (format nil "PASS ~A" password))
      (expect-code-or-lose conn 230))
    (values)))

;; FIXME: Does this in any way interfere with FTP's Unix/DOS line-ending conversion?
#+clisp
(defmethod connect-to-server :around ((conn ftp-connection))
  "clisp considers #\Linefeed and #\Newline to be identical, including conversion
to CRLF for :DOS line-endings.  This is a hack to let us say #\Return #\Linefeed
without ending up with a CR/CR/LF sequence."
  ;; custom:*default-file-encoding* is a symbol-macro and thus can not be bound
  ;; by let, hence the use of clisp's letf, which binds places.
  (ext:letf ((custom:*default-file-encoding*
              (ext:make-encoding :charset (ext:encoding-charset custom:*default-file-encoding*)
                                 :line-terminator :unix)))
    (call-next-method)))

(defmacro with-ftp-connection ((conn &key hostname port username password passive-ftp-p session-stream (code-cut-off-p t code-cut-off-p-p) (if-failed :error)) &body body)
  `(let ((,conn (make-instance 'ftp-connection
                               ,@(if hostname `(:hostname ,hostname) ())
                               ,@(if port `(:port ,port) ())
                               ,@(if username `(:username ,username) ())
                               ,@(if password `(:password ,password) ())
                               ,@(if passive-ftp-p
                                     `(:passive-ftp-p ,passive-ftp-p) ())
                               ,@(if session-stream
                                     `(:session-stream ,session-stream) ())
                               ,@(if code-cut-off-p-p
                                     `(:code-cut-off-p ,code-cut-off-p) ()))))
    (if (null ,conn)
        (if (eql ,if-failed :error)
            (error "Connection to ~A:~A failed" ,hostname ,port)
            ,if-failed)
        (unwind-protect (progn ,@body)
          (close-connection ,conn)))))

(defmethod log-session ((conn ftp-connection) (data string))
  (with-ftp-connection-slots (conn)
    (when (and session-stream (streamp session-stream))
      (write-string data session-stream))
    (values)))

(defmethod log-session ((conn ftp-connection) (data list))
  (log-session conn (data-to-string data)))

(defmethod close-connection ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (close socket)))

(defmethod send-raw-line ((conn ftp-connection) (line string))
  (with-ftp-connection-slots (conn)
    (let ((line (format nil "~A~C~C" line #\Return #\Linefeed)))
      (log-session conn line)
      (write-string line socket))
    (force-output socket)
    (values)))

(defmethod data-ready-p ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (listen socket)))

(defun clean-ftp-response (data)
  (mapcar #'(lambda (line)
              (string-trim '(#\Return #\Linefeed #\Newline)
                           line))
          data))

(defun maybe-cut-off-code (cut-off-p data code)
  (if cut-off-p
      data
      (mapcar #'(lambda (x)
                  (if (and (> (length x) 3)
                           (eql (parse-integer x :end 3 :junk-allowed t)
                                code))
                      (subseq x 4)
                      x))
              data)))

(defmethod receive-response ((conn ftp-connection) &key (block nil))
  (with-ftp-connection-slots (conn)
    (when (and (not block) (not (data-ready-p conn)))
      (return-from receive-response nil))
    (loop :with initial-line = (read-line socket)
          :with ftp-code = (parse-integer initial-line :end 3)
          :for line = initial-line :then (read-line socket)
          :for line-code = ftp-code :then
                           (when (> (length line) 3)
                             (parse-integer line :end 3
                                            :junk-allowed t))
          :when (and code-cut-off-p (eql line-code ftp-code))
            :collect (subseq line 4) :into lines
          :else
            :collect line :into lines
          :end
          :until (and (eql line-code ftp-code)
                      (char= (char line 3) #\Space))
          :finally (let ((data (clean-ftp-response lines)))
                     (log-session conn data)
                     (return (values (maybe-cut-off-code code-cut-off-p
                                                         data
                                                         ftp-code)
                                     ftp-code))))))

(defmethod send-port-command ((conn ftp-connection) (ip string) (port-num integer))
  (multiple-value-bind (quot rem)
      (truncate port-num 256)
    (send-raw-line conn
                   (format nil "PORT ~A,~A,~A"
                           (substitute #\, #\. ip) quot rem))))

(defmethod receive-pasv-response ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (multiple-value-bind (data code)
        (receive-response conn :block t)
      (unless (eql code 227)
        (raise-ftp-error code (data-to-string data)
                         :expected-code 227))
      (let ((start (position #\( (first data) :from-end t))
            (end (position #\) (first data) :from-end t)))
        (unless (and start end)
          (error "Unable to parse PASV response"))
        (let ((numbers (split-sequence #\, (first data)
                                       :start (1+ start)
                                       :end end)))
          (values (format nil "~{~A~^.~}"
                          (subseq numbers 0 4))
                  (+ (ash (parse-integer (fifth numbers)) 8)
                     (parse-integer (sixth numbers)))))))))

(defmethod setup-port ((conn ftp-connection) &key (format :binary))
  (with-ftp-connection-slots (conn)
    (let ((server-socket
           (loop for p = (+ 1025 (random 10000))
                 for s = (ignore-errors
                           (make-socket :connect :passive
                                        :local-port p
                                        :format format))
                 when s return s))
          (local-ip (ipaddr-to-dotted (local-host socket))))
      (send-port-command conn local-ip (local-port server-socket))
      server-socket)))

(defmethod establish-data-transfer ((conn ftp-connection) (command string) &key (rest nil) (type :binary))
  (with-ftp-connection-slots (conn)
    (send-raw-line conn (format nil "TYPE ~A"
                                (ecase type
                                  ((:binary :image) "I")
                                  (:ascii "A"))))
    (expect-code-or-lose conn 200)
    (cond (passive-ftp-p
           (send-raw-line conn "PASV")
           (multiple-value-bind (dtp-hostname dtp-port)
               (receive-pasv-response conn)
             (let ((data-socket
                    (make-socket :remote-host dtp-hostname
                                 :remote-port dtp-port
                                 :format (ecase type
                                           ((:binary :image) :binary)
                                           (:ascii :text)))))
               (when (and rest (integerp rest))
                 (send-raw-line conn (format nil "REST ~A" rest)))
               (send-raw-line conn command)
               data-socket)))
          (t
           (let ((server-socket (setup-port conn
                                            :format (ecase type
                                                      ((:binary :image)
                                                       :binary)
                                                      (:ascii :text)))))
             (unwind-protect
                  (progn
                    (when (and rest (integerp rest))
                      (send-raw-line conn (format nil "REST ~A" rest)))
                    (expect-code-or-lose conn 200)
                    (send-raw-line conn command)
                    (accept-connection server-socket))
               (close server-socket)))))))

(defmethod flush-response ((conn ftp-connection))
  (loop while (receive-response conn)))

(defmethod call-with-transfer-socket ((conn ftp-connection) (command string) (fn function) &rest args)
  (flush-response conn)
  (let ((transfer-socket (apply #'establish-data-transfer
                                conn command args)))
    (unwind-protect
         (funcall fn transfer-socket)
      (progn
        (close transfer-socket)
        (loop
         (multiple-value-bind (data code)
             (receive-response conn)
           (declare (ignorable data))
           (when (and (integerp code) (eql code 226))
             (return-from call-with-transfer-socket t))
           (when (and (integerp code) (>= code 500))
             (return-from call-with-transfer-socket nil))))))))

(defmacro with-transfer-socket ((socket conn command &rest args) &body body)
  `(call-with-transfer-socket ,conn ,command
    #'(lambda (,socket) ,@body)
    ,@args))

(defmethod send-list-command ((conn ftp-connection) (output null) &optional (pathname "."))
  (with-output-to-string (s)
    (send-list-command conn s pathname)))

(defmethod send-list-command ((conn ftp-connection) (output t) &optional (pathname "."))
  (send-list-command conn *standard-output* pathname))

(defmethod send-list-command ((conn ftp-connection) (output stream) &optional (pathname "."))
  (flet ((read-all (s)
           (loop (handler-case (write-line (read-line s) output)
                   (end-of-file () (return (values)))))))
    (with-transfer-socket (s conn (format nil "LIST ~A" pathname)
                             :type :ascii)
      (read-all s))))

(defmethod send-nlst-command ((conn ftp-connection) (output null) &optional (pathname "."))
  (with-output-to-string (s)
    (send-nlst-command conn s pathname)))

(defmethod send-nlst-command ((conn ftp-connection) (output t) &optional (pathname "."))
  (send-nlst-command conn *standard-output* pathname))

(defmethod send-nlst-command ((conn ftp-connection) (output stream) &optional (pathname "."))
  (flet ((read-all (s)
           (loop (handler-case (write-line (read-line s) output)
                   (end-of-file () (return (values)))))))
    (with-transfer-socket (s conn (format nil "NLST ~A" pathname)
                             :type :ascii)
      (read-all s))))

(defmethod retrieve-filename-list ((conn ftp-connection) &optional (pathname "."))
  (let* ((data (send-nlst-command conn nil pathname))
         (split-data (split-sequence #\Newline data
                                     :remove-empty-subseqs t)))
    (mapcar #'(lambda (x) (string-trim '(#\Return) x)) split-data)))

(defmethod retrieve-file-info-list ((conn ftp-connection) &optional (pathname "."))
  (let ((names (retrieve-filename-list conn pathname))
        (file-info-list nil)
        (orig-dir (send-pwd-command conn))
        (base-dir nil))
    (send-cwd-command conn pathname)
    (setf base-dir (send-pwd-command conn))
    (unwind-protect
         (dolist (name names file-info-list)
           (handler-case
               (progn
                 (send-cwd-command conn name)
                 (push (list :directory name) file-info-list))
             (ftp-error ()
               (push (list :file name) file-info-list)))
           (send-cwd-command conn base-dir))
      (send-cwd-command conn orig-dir))))

(defmethod retrieve-file ((conn ftp-connection) (remote-filename string) local-filename &key (type :binary) (rest nil))
  (with-open-file (local-stream local-filename
                                :direction :output
                                :element-type (ecase type
                                                ((:binary :image)
                                                 '(unsigned-byte 8))
                                                (:ascii
                                                 'character)))
    (retrieve-file conn remote-filename local-stream :type type :rest rest)))

(defmethod retrieve-file ((conn ftp-connection) (remote-filename string) (local-stream stream) &key (type :binary) (rest nil))
  (with-transfer-socket (s conn (format nil "RETR ~A" remote-filename)
                           :type type :rest rest)
    (handler-case
        (ecase type
          ((:binary :image)
           (loop (write-byte (read-byte s) local-stream)))
          (:ascii
           (loop (write-char (read-char s) local-stream))))
      (end-of-file () (values)))))

(defmethod store-file ((conn ftp-connection) local-filename (remote-filename string) &key (type :binary) (rest nil))
  (with-open-file (local-stream local-filename
                                :direction :input
                                :element-type (ecase type
                                                ((:binary :image)
                                                 '(unsigned-byte 8))
                                                (:ascii
                                                 'character)))
    (store-file conn local-stream remote-filename :type type :rest rest)))

(defmethod store-file ((conn ftp-connection) (local-stream stream) (remote-filename string) &key (type :binary) (rest nil))
  (with-transfer-socket (s conn (format nil "STOR ~A" remote-filename)
                           :type type :rest rest)
    (handler-case
        (ecase type
          ((:binary :image)
           (loop (write-byte (read-byte local-stream) s)))
          (:ascii
           (loop (write-char (read-char local-stream) s))))
      (end-of-file () (values)))))

(defmacro def-simple-command (cmd (conn &rest args) &body body)
  (let ((name (intern (format nil "SEND-~A-COMMAND" cmd))))
    `(progn
      (defmethod ,name ((,conn ftp-connection) ,@args)
        (flush-response ,conn)
        ,@body)
      (export ',name))))

(def-simple-command dele (conn (remote-filename string))
  (send-raw-line conn (format nil "DELE ~A" remote-filename))
  (expect-code-or-lose conn 250))

(def-simple-command size (conn (remote-filename string))
  (send-raw-line conn (format nil "SIZE ~A" remote-filename))
  (parse-integer (first (expect-code-or-lose conn 213))))

(def-simple-command cwd (conn (remote-dir string))
  (send-raw-line conn (if (string-equal remote-dir "..")
                          "CDUP"
                          (format nil "CWD ~A" remote-dir)))
  (expect-code-or-lose conn 250))

(def-simple-command cdup (conn)
  (send-raw-line conn "CDUP")
  (expect-code-or-lose conn 250))

(defun parse-257-response (string)
  (let ((start (1+ (position #\" string)))
        (last (1- (length string))))
    (with-output-to-string (out)
      (do ((i start (1+ i)))
          ((>= i last) (values))
        (if (char= (char string i) #\")
            (cond ((char= (char string (1+ i)) #\")
                   (write-char #\" out)
                   (incf i))
                  (t (return (values))))
            (write-char (char string i) out))))))

(def-simple-command pwd (conn)
  (send-raw-line conn "PWD")
  (parse-257-response
   (data-to-string (expect-code-or-lose conn 257))))

(def-simple-command mkd (conn (dir-name string))
  (send-raw-line conn (format nil "MKD ~A" dir-name))
  (parse-257-response
   (data-to-string (expect-code-or-lose conn 257))))
