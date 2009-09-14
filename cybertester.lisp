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


(in-package "CYBERTESTER")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spin ()
  (format t "."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The bots for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +test-host+ "http://laconica.local/api")

(defvar *bots* nil
  "The bots to test")

(defvar *tester* nil
  "The test message user")

(defun initialize-bots ()
  "Make the bots we use"
  (setf *bots* '())
  ;; Add in reverse order so dolist goes from artist to collector
  (cybercollector::configure "cybercollector" "password" "cybercritic"
			     +test-host+)
  (push (cybercollector::make-microblog-bot) *bots*)
  (cybercritic::configure "cybercritic" "password" "cybernetic"
			  +test-host+)
  (push (cybercritic::make-microblog-bot) *bots*)
  (cyberartist::configure "cybernetic" "password" 
			  +test-host+)
  (push (cyberartist::make-microblog-bot) *bots*)
  (setf *tester* (make-instance 'microblog-bot:microblog-user 
				:nickname "tester"
				:password "password")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post creation testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod microblog-bot:post :around (message &key (in-reply-to-status-id nil))
  "Check that the message really is posted and not repeated too fast, etc."
  (assert message)
  (assert (<= (length message) 140))
   (handler-case
      (let* ((result (call-next-method))
	     (result-text (cl-twit:status-text result)))
	(assert result)
	(assert result-text)
	(when (not (string= result-text message))
	  (format t "POST ERROR: ~a<-->~a~%" message result-text))
	result)
    (cl-twit:http-error (err)
      (format t "POST ERROR: ~a - " (cl-twit:http-status-code err))
      (cond
	((search "duplicate" (cl-twit:http-body err))
	 (format t "multiple posts of '~a'~%" message))
	(t (format t "(message: ~a) ~a~%" message (cl-twit:http-body err)))))
    (error (err)
      (format t "POST ERROR (message: ~a): ~a~%" message err))))

(defun posts-containing-after (after text posts)
  "return posts with id higher than after contain text"
  (let ((matches '()))
    (dolist (post posts)
      (let ((post-id (cl-twit::id post)))
	(when (and (search text (cl-twit:status-text post))
		   (> post-id after))
	  (push post matches))))))

(defun bot-user-posts (bot)
  "Get the bot's user's posts. This should be in microblog-bot"
  (microblog-bot:with-microblog-user bot
    (cl-twit:m-user-timeline)))

(defun test-post-response (bot test-fun) 
  "test-fun must return a message object"
  (posts-containing-after (cl-twit::get-newest-id (cl-twit:m-public-timeline))
			  (cl-twit:status-text (funcall test-fun))
			  (bot-user-posts bot)))

(defun call-with-bot (bot fun)
  "Wrap a call to fun with rest as args with-microblog-user as bot"
  (lambda ()
    (microblog-bot:with-microblog-user bot
      (funcall fun))))

(defun test-post-fun (bot fun)
  "Make sure the bot posts once. Fun must return a message object"
  (assert (= 1
	     (length (test-post-response bot 
					 (lambda () 
					   (funcall fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @reply response testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-no-responses (bot)
  "Make sure the bot doesn't post if there are no responses to post to"
  (let ((previous-max-id (cl-twit::get-newest-id (cl-twit:m-public-timeline))))
    (microblog-bot:with-microblog-user bot
      (microblog-bot::respond-to-replies bot))
    (assert (= previous-max-id
	       (cl-twit::get-newest-id (cl-twit:m-public-timeline))))))

(defun ensure-response (bot)
  "Make sure the bot posts in response to a @reply"
  (let ((previous-max-id (cl-twit::get-newest-id (cl-twit:m-public-timeline))))
    (microblog-bot:with-microblog-user bot
      (microblog-bot::respond-to-replies bot))
    (assert (= 1
	       (- (cl-twit::get-newest-id (cl-twit:m-public-timeline))
		  previous-max-id)))))

(defun tester-post-hi (bot-name)
  "Post hi to the bot"
  (microblog-bot:with-microblog-user *tester*
    (microblog-bot:post (format nil "@~a hi" bot-name))))

(defun tester-request-source (bot-name)
  "Post a source request to the bots"
  (microblog-bot:with-microblog-user *tester*
    (microblog-bot:post (format nil "@~a !source" bot-name))))

(defun test-his ()
  "Make sure the bots respond to a simple response"
  (dolist (bot *bots*)
    (spin)
    (tester-post-hi (microblog-bot:user-nickname bot))
    (ensure-response bot)))

(defun test-sources ()
  "Make sure the bots respond to a source request"
  (dolist (bot *bots*)
    (spin)
    (tester-request-source (microblog-bot:user-nickname bot))
    (ensure-response bot)))

(defun clear-responses ()
  "Respond to any responses"
  (dolist (bot *bots*)
    (spin)
    (microblog-bot:with-microblog-user bot
      (microblog-bot::respond-to-replies bot))))


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
  (microblog-bot:with-microblog-user bot
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
  (microblog-bot:set-microblog-service +test-host+ "tester")
  (initialize-bots))

(defmethod response-tests ()
  "Test response and source requests"
  (format t "~%response-tests")
  (clear-responses)
  (spin)
  (test-his)
  (spin)
  (test-sources)
  (dolist (bot *bots*)
      (ensure-no-responses bot)))

(defun run-test (bot i)
  "Test the bot, sending it a hi or source request and running it once"
  (check-and-update-handled-messages bot)
  (microblog-bot:test-run-bot-once bot :i i :post t :msgs nil))

(defmethod deterministic-tests (&optional (count 10))
  "Test the newly made bots in order count times"
  (dotimes (i count)
    (format t "~%deterministic-tests")
    (dolist (bot *bots*)
      (spin)
      (run-test bot i))))

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defmethod random-tests (&optional (count 10))
  "Test the bots in random order, with random hi and !source messages"
  (format t "~%random-tests")
  (dotimes (i count)
    (spin)
    (run-test (choose-randomly *bots*) i)))

(defmethod comprehensive-tests (&optional (count 1))
  "Run all test styles"
  (microblog-bot:with-microblog-user *tester*
    (microblog-bot:post "STARTING TEST RUN --------------------"))
  (response-tests)
  (deterministic-tests count)
  (random-tests count))

(initialize)