;; cybertester.lisp - Tests for The Cybernetic Artworld.
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


;; Assumes a dedicated server with no other users
;; Do not, under any circumstances, modify to point at a live server


(in-package #:cybertester)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The bots for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *bots* nil
  "The bots to test")

(defvar *tester* nil
  "The test message user")

(defun initialize-bots ()
  "Make the bots we use"
  (setf *bots* '())
  ;; Add in reverse order so dolist goes from artist to collector
  (cybercollector::configure "cybercollector" "password" "cybercritic"
			     "http://localhost/laconica/api")
  (push (cybercollector::make-microblog-bot) *bots*)
  (cybercritic::configure "cybercritic" "password" "cybernetic"
			  "http://localhost/laconica/api")
  (push (cybercritic::make-microblog-bot) *bots*)
  (cyberartist::configure "cybernetic" "password" 
			  "http://localhost/laconica/api")
  (push (cyberartist::make-microblog-bot) *bots*)
  (setf *tester* (make-instance 'microblog-bot:microblog-user 
				:nickname "tester"
				:password "password")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @reply response testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tester-post-hi (bot-name)
  "Post hi to the bot"
  (with-microblog-user *tester*
    (microblog-bot:post (format nil "@~a hi" bot-name))))

(defun tester-request-source (bot-name)
  "Post a source request to the bots"
  (with-microblog-user *tester*
    (microblog-bot:post (format nil "@~a !source" bot-name))))

(defun tester-post-his ()
  (dolist (bot *bots*)
    (tester-post-hi (microblog-bot:user-nickname bot))))

(defun tester-request-sources ()
  (dolist (bot *bots*)
    (tester-request-source (microblog-bot:user-nickname bot))))

(defun tester-hi-or-request-source (bot-name i)
  "Alternate posting hi and source request to the named bot"
  (if (= (mod i 2) 0)
      (tester-post-hi bot-name)
      (tester-request-source bot-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catch posting errors and multiple posts of the same message text too fast
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod microblog-bot:post :around (message)
  "Check for posting too fast, etc."
  (assert message)
  (assert (<= (length message) 140))
  (handler-case
      (let* ((result (call-next-method))
	     (result-text (cl-twit:status-text result)))
	(assert result)
	(assert result-text)
	(assert (string= result-text message)))
    (cl-twit:http-error (err)
      (format t "POST ERROR: ~a - " (cl-twit:http-status-code err))
      (cond
	((search "duplicate" (cl-twit:http-body err))
	 (format t "multiple posts of '~a'~%" message))
	(t (format t "~a~%" (cl-twit:http-body err)))))
    (error (err)
      (format t "~a~%" err))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for duplicate responses to messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *posts-handled* (make-hash-table)
  "Persist for duration of session")
(defvar *replies-handled* (make-hash-table)
  "Persist for duration of session")

(defun check-and-add (hash id capacity)
  (if (gethash id hash nil)
      (error "Already handled id ~a for ~a" id capacity)
      (setf (gethash id hash) capacity)))

(defun check-and-add-list (hash items capacity)
  (dolist (item items)
    (check-and-add hash (cl-twit::id item) capacity)))

(defun check-and-update-handled-messages (bot)
  (with-microblog-user bot
    ;; May skip in tight timing situations, but should catch gross errors
    (check-and-add-list *replies-handled* (microblog-bot::new-replies bot)
			"replies")
    ;; Use of introspection rather than polymorphism. And may skip
    (when (typep bot 'microblog-follower-bot)
      (check-and-add-list *posts-handled* (microblog-bot::new-posts bot)
			  "posts"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running test sequences on bots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize ()
  "Set up the fixtures and state for tests"
  (microblog-bot:set-microblog-service "http://localhost/laconica/api" "tester")
  (initialize-bots)
  (with-microblog-user *tester*
    (microblog-bot:post "STARTING TEST RUN --------------------")))

(defun run-test (bot)
  "Test the bot, sending it a hi or source request and running it once"
  (check-and-update-handled-messages bot)
  (microblog-bot:test-run-bot-once bot :post t :msgs nil))

(defmethod deterministic-tests (&optional (count 10))
  "Test the newly made bots in order count times"
  (initialize)
  (dotimes (i count)
    (dolist (bot *bots*)
      (run-test bot))))

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defmethod response-tests (&optional (count 1))
  "Test response and source requests"
  (tester-post-his)
  (dolist (bot *bots*)
    (with-microblog-user bot
      (microblog-bot::respond-to-replies bot)))
  (tester-request-sources)
  (dolist (bot *bots*)
    (with-microblog-user bot
      (microblog-bot::respond-to-replies bot))))

(defmethod random-tests (&optional (count 10))
  "Test the bots in random order, with random hi and !source messages"
  (initialize)
  (dotimes (i count)
    (run-test (choose-randomly *bots*))))

(defmethod comprehensive-tests (&optional (count 1))
  "Run all test styles"
  (response-tests)
  (deterministic-tests)
  (random-tests))