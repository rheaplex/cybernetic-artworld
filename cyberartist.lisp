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
    (twit:update (generate-description)))

(defun make-microblog-bot ()
  "Make the bot."
  (assert (>= (length sb-ext:*posix-argv*) 2))
  (microblog-bot:set-microblog-service "https://identi.ca/api" "cybernetic")
  (make-instance 'cyberartist
		 :nickname (second sb-ext:*posix-argv*)
		 :password (third sb-ext:*posix-argv*)
		 :ignore '("cybercritic")
		 :source-url "http://robmyers.org/git/?p=cybernetic-artworld.git"))

(defun run ()
  "Configure and run the bot."
  (setf *random-state* (make-random-state t))
  (microblog-bot:run-bot (make-microblog-bot)))

(defun run-once ()
  "Configure and run the bot just once."
  (setf *random-state* (make-random-state t))
  (microblog-bot:run-bot-once (make-microblog-bot))
  (sb-ext:quit))

(defun run-once-randomly ()
  "Configure and run the bot just once, every few runs."
  (assert (>= (length sb-ext:*posix-argv*) 3))
  (setf *random-state* (make-random-state t))
  (let ((num (parse-integer (fourth sb-ext:*posix-argv*))))
    (when (< (random num) 1)
      (microblog-bot:run-bot-once (make-microblog-bot))))
  (sb-ext:quit))