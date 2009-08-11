;; cyberartist.lisp -  The main generate-and-blog code.
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

(in-package #:cyberartist)

(defclass cyberartist (microblog-bot:microblog-bot)
  ())

(defmethod microblog-bot:constant-task ((bot cyberartist))
  "Dent a possible artwork."
    (microblog-bot:post (generate-description)))

(defvar *username* nil)
(defvar *password* nil)

(defun configure (username password)
  "Configure the global state."
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service "https://identi.ca/api" "cybernetic")
  (setf *username* username)
  (setf *password* password))

(defun cli-configure ()
  "Configure from the command line arguments."
  (assert (>= (length sb-ext:*posix-argv*) 2))
  (configure (second sb-ext:*posix-argv*)
	     (third sb-ext:*posix-argv*)))

(defun debug-configure (username password)
  "Configure from the repl, and set the state to debugging."
  (microblog-bot:set-debug)
  (configure username password))

(defun make-microblog-bot ()
  "Make the bot."
  (assert (and *username* *password*))
  (make-instance 'cyberartist
		 :nickname *username*
		 :password *password*
		 :ignore '("cybercritic")
		 :source-url "http://robmyers.org/git/?p=cybernetic-artworld.git"))

(defun run ()
  "Configure and run the bot."
  (cli-configure)
  (microblog-bot:run-bot (make-microblog-bot)))

(defun run-test (username password)
  (require 'cyberartist)
  (microblog-bot:set-debug)
  (configure username password)
  (microblog-bot:run-bot (make-microblog-bot)))