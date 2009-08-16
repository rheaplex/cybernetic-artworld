;; cybercollector.lisp -  The main collect-and-blog code.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:cybercollector)

(defclass cybercollector (microblog-bot:microblog-follower-bot)
  ())

(defun masterpiece-p (description)
  "Is the subject of the description a masterpiece?"
  (search "masterpiece" description))

(defun artwork-url (critique-text)
  "Get the artwork url from the message"
   (cl-ppcre:scan-to-strings "http://[^ ]+" critique-text))

(defmethod microblog-bot:response-for-post ((bot cybercollector) mention)
  "Respond to the artwork by buying it if it's a masterpiece."
  (let ((status-text 
	 (handler-case
	     (cl-twit:status-text mention)
	   (error (err) 
	     (microblog-bot:report-error "response-for-message ~a - ~a~%" 
					 bot err)))))
    (if (and status-text
	     (masterpiece-p (cl-twit:status-text mention)))
	(format nil "I just bought ~a" (artwork-url status-text))
	nil)))

(defvar *username* nil)
(defvar *password* nil)
(defvar *follow* nil)

(defun configure (username password follow
		  &optional (host "https://identi.ca/api"))
  "Configure the global state."
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service host "cybercollector")
  (setf *username* username)
  (setf *password* password)
  (setf *follow* follow))

(defun cli-configure ()
  "Configure from the command line arguments."
  (assert (>= (length sb-ext:*posix-argv*) 3))
  (configure (second sb-ext:*posix-argv*)
	     (third sb-ext:*posix-argv*)
	     (fourth sb-ext:*posix-argv*)))

(defun make-microblog-bot ()
  "Make the bot using the configuration"
  (assert (and *username* *password* *follow*))
  (make-instance 'cybercollector
		 :nickname *username*	    
		 :password *password*
		 :follow-screen-name *follow*
		 :source-url 
		 "http://robmyers.org/git/?p=cybernetic-artworld.git"))

(defun run-cybercollector ()
  "Configure and run the bot."
  (cli-configure)
  (microblog-bot:run-bot (make-microblog-bot)))

(defun test-run-cybercollector (username password follow)
  "Configure and run the bot in test mode"
  (require 'cybercollector)
  (configure username password follow "http://localhost/laconica/api")
  (microblog-bot:test-run-bot (make-microblog-bot) 10 :post t))
