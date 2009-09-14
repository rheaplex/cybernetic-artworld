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

(in-package :cybercritic)

(defclass cybercritic (microblog-bot:microblog-follower-bot)
  ((aesthetic :initarg :aesthetic
	      :accessor cybercritic-aesthetic)))

(defmethod post-aesthetic ((bot cybercritic))
  "Post a description of the bot's aesthetic"
  (multiple-value-bind (good bad) 
      (aesthetic:describe-aesthetic (cybercritic-aesthetic bot))
    (let ((desc (concatenate 'string good " " bad)))
      ;; If the description is short enough to post all in one go, do so
      (if (<= (length desc) 140)
	  (microblog-bot:post desc)
	  ;; Otherwise post in two sections, truncating if they are too long
	  (progn
	    (when (> (length good) 140)
	      (setf good 
		    (format nil "~a..." (subseq good 0 137))))
	    (microblog-bot:post good)
	    (when (> (length bad) 140)
	      (setf bad 
		    (format nil "~a..." (subseq bad 0 137))))
	    (microblog-bot:post bad))))))

(defmethod microblog-bot:daily-task ((bot cybercritic))
  "Update the aesthetic and dent it."
  (aesthetic:update-aesthetic (cybercritic-aesthetic bot))
  (post-aesthetic bot))

(defmethod microblog-bot:filter-posts ((bot cybercritic) posts)
  "Ignore @replies."
  (loop for post in posts
       if (not (response-p bot post))
       collect post))

(defmethod microblog-bot:response-for-post ((bot cybercritic) mention)
  "Respond to the artwork by critiquing it."
  (critique-artwork (cl-twit:status-text mention) 
		    (cybercritic-aesthetic bot)
		    (format nil
			    "http://identi.ca/notice/~a"
			    (cl-twit:status-id mention))))

(defvar *username* nil)
(defvar *password* nil)
(defvar *follow* nil)

(defun configure (username password follow
		  &optional (host "https://identi.ca/api"))
  "Configure the global state."
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service host "cybercritic")
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
  (assert (and *username* *password* *follow*))
  (let ((bot (make-instance 'cybercritic
		 :nickname *username*	    
		 :password *password*
		 :follow-screen-name *follow*
		 :source-url 
		 "http://robmyers.org/git/?p=cybernetic-artworld.git"
		 :aesthetic (aesthetic:make-aesthetic))))
    (microblog-bot:with-microblog-user bot
      (post-aesthetic bot))
    bot))

(defun run-cybercritic ()
  "Configure and run the bot"
  (cli-configure)
  (microblog-bot:run-bot (make-microblog-bot)))

(defun test-run-cybercritic (username password follow)
  (require 'cybercritic)
  (configure username password follow "http://localhost/laconica/api")
  (microblog-bot:test-run-bot (make-microblog-bot) 10 :post t))