;;;; -*- Mode: Lisp -*-
;;;; Authors:
;;;; * Matthew Danish <mrd@debian.org>
;;;; * Hans Hübner
;;;; See LICENSE file for copyright details.
;;;; FTP client functionality

(defpackage #:org.mapcar.ftp.client
  (:use #:common-lisp
        #:split-sequence
        #:usocket)
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

(define-condition ftp-error ()
  ((ftp-error-code :initarg :ftp-error-code
                   :initform "\"unspecified\""
                   :reader ftp-error-code
                   :documentation "Code associated with message")
   (error-message :initarg :error-message
                  :initform "\"unspecified\""
                  :reader error-message
                  :documentation "FTP server's error message"))
  (:report (lambda (c s)
             (format s "FTP error ~A raised: ~A"
                     (ftp-error-code c)
                     (error-message c)))))

(define-condition invalid-code (ftp-error)
  ((expected :reader expected :initarg :expected
             :documentation "Expected code")
   (received :reader received :initarg :received
             :documentation "Received code"))
  (:report (lambda (c s)
             (format s "Expected FTP code ~A, got FTP code ~A"
                     (expected c)
                     (received c)))))

(define-condition transient-negative-completion (ftp-error)
  ()
  (:report (lambda (c s)
             (format s "Received transient error code ~A: ~A"
                     (ftp-error-code c)
                     (error-message c))))
  (:documentation "Signalled when a transient error is received from the FTP server.  This means that the input was fine, but something else went wrong.  Feel free to resend."))

(define-condition permanent-negative-completion (ftp-error)
  ()
  (:report (lambda (c s)
             (format s "Received permanent error code ~A: ~A"
                     (ftp-error-code c)
                     (error-message c))))
  (:documentation "Signalled when a permanent error is received from the FTP server.  This means that the input was not acceptable and should not be re-sent."))

(defclass ftp-connection ()
  ((hostname :initarg :hostname
             :reader ftp-hostname
             :documentation "The remote hostname")
   (port :initarg :port :initform 21
         :reader ftp-port
         :documentation "The remote port")
   (username :initarg :username :initform "anonymous"
             :reader ftp-username
             :documentation "The login username")
   (password :initarg :password :initform "cl-ftp@cclan.net"
             :reader ftp-password
             :documentation "The login password")
   (session-stream :initarg :session-stream :initform nil
                   :reader ftp-session-stream
                   :documentation "Send FTP session output to this stream, if non-nil")
   (passive-ftp-p :initarg :passive-ftp-p :initform nil
                  :accessor passive-ftp-p
                  :documentation "Use passive FTP if non-nil")
   (code-cut-off-p :initarg :code-cut-off-p :initform t
                   :accessor code-cut-off-p
                   :documentation "When non-nil, cut-off FTP codes in logging output")
   (socket))
  (:documentation "Represents an FTP connection and associated state.  The INITIALIZE-INSTANCE :AFTER method takes care of connection and login, if possible."))

(defmacro %doc-fns (&rest list)
  `(progn ,@(loop :for (sym doc) :on list :by #'cddr
                  :collect `(setf (documentation ',sym 'function) ',doc))))
(%doc-fns ftp-hostname "The remote hostname"
          ftp-port "The remote port"
          ftp-username "The login username"
          ftp-password "The login password"
          ftp-session-stream "The session stream for the FTP connection"
          passive-ftp-p "Non-nil iff given FTP connection is to use passive FTP for data transfers"
          (setf passive-ftp-p) "Value should be non-nil to use passive FTP for data transfers with the given FTP connection"
          code-cut-off-p "Non-nil iff FTP codes are to be cut-off when logging"
          (setf code-cut-off-p) "Alter value of code-cut-off-p")

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
  "Converts a list of strings, such as that produced by receive-response, to one string with newlines after each formerly-list-element."
  (format nil "~{~A~%~}" data))

(defgeneric expect-code-or-lose (conn expected-code))

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

(defgeneric connect-to-server (conn)
  (:documentation "Attempts to connect to the server using the information provided by connection-variable.  If connection-variable represents an existing connection, then that connection will be closed and a new one established."))

(defmethod connect-to-server ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (unless (and hostname port (integerp port) (stringp hostname))
      (error "You must specify a hostname string and an integer port"))
    (when (and (slot-boundp conn 'socket) (streamp (socket-stream socket)))
      (close (socket-stream socket)))
    (setf socket (socket-connect hostname port))
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
  "Opens and ensures proper close of an FTP connection.  Binds connection-variable to the FTP-CONNECTION object in the scope of body.  Arguments are similar to that of the initargs for the class FTP-CONNECTION."
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

(defgeneric log-session (conn data))

(defmethod log-session ((conn ftp-connection) (data string))
  (with-ftp-connection-slots (conn)
    (when (and session-stream (streamp session-stream))
      (write-string data session-stream))
    (values)))

(defmethod log-session ((conn ftp-connection) (data list))
  (log-session conn (data-to-string data)))

(defgeneric close-connection (conn)
  (:documentation "Closes the given FTP connection"))

(defmethod close-connection ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (close (socket-stream socket))))

(defgeneric send-raw-line (conn line))

(defmethod send-raw-line ((conn ftp-connection) (line string))
  (with-ftp-connection-slots (conn)
    (let ((line (format nil "~A~C~C" line #\Return #\Linefeed)))
      (log-session conn line)
      (write-string line (socket-stream socket)))
    (force-output (socket-stream socket))
    (values)))

(defgeneric data-ready-p (conn)
  (:documentation "Non-nil iff data is waiting to be read from the control connection."))

(defmethod data-ready-p ((conn ftp-connection))
  (with-ftp-connection-slots (conn)
    (listen (socket-stream socket))))

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

(defgeneric receive-response (conn &key block)
  (:documentation "Receives a response from the FTP server.  Returns a list of strings as the first value and the response code as the second.  If :BLOCK is T, then will block until response received.  Otherwise return NIL if nothing is available currently."))

(defmethod receive-response ((conn ftp-connection) &key (block nil))
  (with-ftp-connection-slots (conn)
    (when (and (not block) (not (data-ready-p conn)))
      (return-from receive-response nil))
    (loop :with initial-line = (read-line (socket-stream socket))
          :with ftp-code = (parse-integer initial-line :end 3)
          :for line = initial-line :then (read-line (socket-stream socket))
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

(defgeneric send-port-command (conn ip port-num))

(defmethod send-port-command ((conn ftp-connection) (ip string) (port-num integer))
  (multiple-value-bind (quot rem)
      (truncate port-num 256)
    (send-raw-line conn
                   (format nil "PORT ~A,~A,~A"
                           (substitute #\, #\. ip) quot rem))))

(defgeneric receive-pasv-response (conn))

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

(defgeneric setup-port (conn &key element-type))

(defmethod setup-port ((conn ftp-connection) &key (element-type '(unsigned-byte 8)))
  (with-ftp-connection-slots (conn)
    (let ((server-socket
            (socket-listen *wildcard-host* *auto-port* :element-type element-type))
          (local-ip (vector-quad-to-dotted-quad (get-local-name socket))))
      (send-port-command conn local-ip (get-local-port server-socket))
      server-socket)))

(defgeneric establish-data-transfer (conn command &key rest type))

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
                    (socket-connect dtp-hostname dtp-port
                                    :element-type (ecase type
                                                    ((:binary :image) '(unsigned-byte 8))
                                                    (:ascii 'character)))))
               (when (and rest (integerp rest))
                 (send-raw-line conn (format nil "REST ~A" rest)))
               (send-raw-line conn command)
               data-socket)))
          (t
           (let ((server-socket (setup-port conn :element-type (ecase type
                                                                 ((:binary :image)
                                                                  '(unsigned-byte 8))
                                                                 (:ascii 'character)))))
             (unwind-protect
                  (progn
                    (when (and rest (integerp rest))
                      (send-raw-line conn (format nil "REST ~A" rest)))
                    (expect-code-or-lose conn 200)
                    (send-raw-line conn command)
                    (socket-accept server-socket))
               (socket-close server-socket)))))))

(defgeneric flush-response (conn))

(defmethod flush-response ((conn ftp-connection))
  (loop while (receive-response conn)))

(defgeneric call-with-transfer-socket (conn command fn &rest args)
  (:documentation "Similar to WITH-TRANSFER-SOCKET, except that function is a function which accepts a single argument; namely the transfer-socket"))

(defmethod call-with-transfer-socket ((conn ftp-connection) (command string) (fn function) &rest args)
  (flush-response conn)
  (let ((transfer-socket (apply #'establish-data-transfer
                                conn command args)))
    (unwind-protect
         (funcall fn transfer-socket)
      (progn
        (close (socket-stream transfer-socket))
        (loop
         (multiple-value-bind (data code)
             (receive-response conn)
           (declare (ignorable data))
           (when (and (integerp code) (eql code 226))
             (return-from call-with-transfer-socket t))
           (when (and (integerp code) (>= code 500))
             (return-from call-with-transfer-socket nil))))))))

(defmacro with-transfer-socket ((socket conn command &rest args) &body body)
  "Opens a data transfer socket in the scope of body, using the given FTP connection and executing the given FTP command-string.  If :REST is specified, then the FTP \"REST\" command will be sent with the value of the argument.  :TYPE may be :BINARY or :ASCII.  Closes the transfer-socket upon dynamic exit of body."
  `(call-with-transfer-socket ,conn ,command
    #'(lambda (,socket) ,@body)
    ,@args))

(defgeneric send-list-command (conn output &optional pathname)
  (:documentation "Sends the FTP LIST command.  If OUTPUT is NIL, returns a string.  If OUTPUT is T, prints to *standard-output*.  Otherwise, it treats OUTPUT as the output stream."))

(defmethod send-list-command ((conn ftp-connection) (output null) &optional (pathname "."))
  (with-output-to-string (s)
    (send-list-command conn s pathname)))

;; FIXME: Should (output t) be (output (eq t))?  Running when output is
;; something like "ham" would be ... weird.
(defmethod send-list-command ((conn ftp-connection) (output t) &optional (pathname "."))
  (send-list-command conn *standard-output* pathname))

(defmethod send-list-command ((conn ftp-connection) (output stream) &optional (pathname "."))
  (flet ((read-all (s)
           (loop (handler-case (write-line (read-line (socket-stream s)) output)
                   (end-of-file () (return (values)))))))
    (with-transfer-socket (s conn (format nil "LIST ~A" pathname)
                             :type :ascii)
      (read-all s))))

(defgeneric send-nlst-command (conn output &optional pathname)
  (:documentation "Sends the FTP NLST command.  If OUTPUT is NIL, returns a string.  If OUTPUT is T, prints to *standard-output*.  Otherwise, it treats OUTPUT as the output stream."))

(defmethod send-nlst-command ((conn ftp-connection) (output null) &optional (pathname "."))
  (with-output-to-string (s)
    (send-nlst-command conn s pathname)))

;; FIXME: Should (output t) be (output (eq t))?  Running when output is
;; something like "ham" would be ... weird.
(defmethod send-nlst-command ((conn ftp-connection) (output t) &optional (pathname "."))
  (send-nlst-command conn *standard-output* pathname))

(defmethod send-nlst-command ((conn ftp-connection) (output stream) &optional (pathname "."))
  (flet ((read-all (s)
           (loop (handler-case (write-line (read-line (socket-stream s)) output)
                   (end-of-file () (return (values)))))))
    (with-transfer-socket (s conn (format nil "NLST ~A" pathname)
                             :type :ascii)
      (read-all s))))

(defgeneric retrieve-filename-list (conn &optional pathname)
  (:documentation "Retrieves a list of filenames for the given pathname."))

(defmethod retrieve-filename-list ((conn ftp-connection) &optional (pathname "."))
  (let* ((data (send-nlst-command conn nil pathname))
         (split-data (split-sequence #\Newline data
                                     :remove-empty-subseqs t)))
    (mapcar #'(lambda (x) (string-trim '(#\Return) x)) split-data)))

(defgeneric retrieve-file-info-list (conn &optional pathname)
  (:documentation "Retrieves a list of the form (type name) where type is :DIRECTORY or :FILE and name is a filename in the given directory named by pathname.  Note: this is implemented by attempting CWDs, and may break if the FTP server does strange things."))

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

(defgeneric retrieve-file (conn remote-filename local-file &key type rest &allow-other-keys)
  (:documentation "Retrieves a file given a remote filename, and a local filename or stream.  :TYPE is either :ASCII or :BINARY, and :REST specifies an integer amount to seek into the file before retrieving it."))

(defmethod retrieve-file ((conn ftp-connection) (remote-filename string) local-filename
                          &key (type :binary) (rest nil) (if-exists :error))
  (with-open-file (local-stream local-filename
                                :direction :output
                                :if-exists if-exists
                                :element-type (ecase type
                                                ((:binary :image)
                                                 '(unsigned-byte 8))
                                                (:ascii
                                                 'character)))
    ;; if-exists can be nil, so we have to check if local-stream is non-nil
    (when local-stream
      (retrieve-file conn remote-filename local-stream :type type :rest rest))))

(defmethod retrieve-file ((conn ftp-connection) (remote-filename string) (local-stream stream) &key (type :binary) (rest nil))
  (with-transfer-socket (s conn (format nil "RETR ~A" remote-filename)
                           :type type :rest rest)
    (handler-case
        (ecase type
          ((:binary :image)
           (loop (write-byte (read-byte (socket-stream s)) local-stream)))
          (:ascii
           (loop (write-char (read-char (socket-stream s)) local-stream))))
      (end-of-file () (values)))))

(defgeneric store-file (conn local-filename remote-filename &key type rest)
  (:documentation "Stores a file given a local filename or stream and a remote filename.  :TYPE is either :ASCII or :BINARY."))

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
           (loop (write-byte (read-byte local-stream) (socket-stream s))))
          (:ascii
           (loop (write-char (read-char local-stream) (socket-stream s)))))
      (end-of-file () (values)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-body (body)
    (if (stringp (first body))
        (values (first body) (rest body))
        (values nil body)))
  (defun %get-arg-name (arg)
    (if (symbolp arg)
        arg
        (first arg))))

(defmacro def-simple-command (cmd (conn &rest args) &body body)
  (let ((name (intern (format nil "SEND-~A-COMMAND" cmd))))
    (multiple-value-bind (doc body) (%parse-body body)
      `(progn
         (defgeneric ,name (,conn ,@(mapcar #'%get-arg-name args))
           ,@(if doc `((:documentation ,doc))))
         (defmethod ,name ((,conn ftp-connection) ,@args)
           (flush-response ,conn)
           ,@body)
         (export ',name)
         ',name))))

(def-simple-command dele (conn (remote-filename string))
  (send-raw-line conn (format nil "DELE ~A" remote-filename))
  (expect-code-or-lose conn 250))

(def-simple-command size (conn (remote-filename string))
  "Sends the FTP SIZE command on the given remote-filename.  Returns an integer size.  Signals error if no such file."
  (send-raw-line conn (format nil "SIZE ~A" remote-filename))
  (parse-integer (first (expect-code-or-lose conn 213))))

(def-simple-command cwd (conn (remote-dir string))
  "Sends the FTP CWD command, to change to the given remote-directory.  If remote-directory is \"..\", CDUP is sent instead.  Signals error if not possible."
  (send-raw-line conn (if (string-equal remote-dir "..")
                          "CDUP"
                          (format nil "CWD ~A" remote-dir)))
  (expect-code-or-lose conn 250))

(def-simple-command cdup (conn)
  "Sends the FTP CDUP command."
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
  "Sends the FTP PWD command and returns the current working directory as a string."
  (send-raw-line conn "PWD")
  (parse-257-response
   (data-to-string (expect-code-or-lose conn 257))))

(def-simple-command mkd (conn (dir-name string))
  "Sends the FTP MKD command to make a remote directory.  Returns directory name as string.  Signals error if not possible."
  (send-raw-line conn (format nil "MKD ~A" dir-name))
  (parse-257-response
   (data-to-string (expect-code-or-lose conn 257))))
