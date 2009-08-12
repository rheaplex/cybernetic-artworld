;; cybercritic.lisp -  The main generate-and-blog code.
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

(in-package #:cybercritic)

(defclass cybercritic (microblog-bot:microblog-follower-bot)
  ((aesthetic :initarg :aesthetic
	      :accessor cybercritic-aesthetic)))

(defmethod microblog-bot:daily-task ((bot cybercritic))
  "Update the aesthetic and dent it."
  (update-aesthetic)
  (let ((description (describe-aesthetic)))
    ;; Handle description being longer than the microblogging limit of 140 chars
    (if (> (length description) 140)
	(setf description 
	      (format nil "~a..." (subseq description 0 137))))
	(microblog-bot:post description)))

(defmethod microblog-bot:response-for-message ((bot cybercritic) mention)
  "Respond to the artwork by critiquing it."
  (critique-artwork (cl-twit:status-text mention) 
		    (cybercritic-aesthetic bot)
		    (format nil
			    "http://identi.ca/notice/~a"
			    (cl-twit:status-id mention))))

(defvar *username* nil)
(defvar *password* nil)
(defvar *follow* nil)

(defun configure (username password follow)
  "Configure the global state."
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service "https://identi.ca/api" "cybercritic")
  (setf *username* username)
  (setf *password* password)
  (setf *follow* follow))

(defun cli-configure ()
  "Configure from the command line arguments."
  (assert (>= (length sb-ext:*posix-argv*) 3))
  (configure (second sb-ext:*posix-argv*)
	     (third sb-ext:*posix-argv*)
	     (fourth sb-ext:*posix-argv*)))

(defun debug-configure (username password follow)
  "Configure from the repl, and set the state to debugging."
  (microblog-bot:set-debug)
  (configure username password follow))

(defun make-microblog-bot ()
  (assert (and *username* *password* *follow*))
  (make-instance 'cybercritic
		 :nickname *username*	    
		 :password *password*
		 :follow-id *follow*
		 :source-url 
		 "http://robmyers.org/git/?p=cybernetic-artworld.git"
		 :aesthetic (aesthetic:make-aesthetic)))

(defun run ()
  "Configure and run the bot."
  (cli-configure)
  (microblog-bot:run-bot (make-microblog-bot)))

(defun run-test (username password follow)
  (require 'cybercritic)
  (microblog-bot:set-debug)
  (configure username password follow)
  (microblog-bot:run-bot (make-microblog-bot)))