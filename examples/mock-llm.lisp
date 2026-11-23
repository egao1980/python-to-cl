;;; A local mock of the OpenAI and Anthropic HTTP APIs, for the python-to-cl
;;; LLM examples: no API keys, no network. (mock-llm:start-mock) serves
;;; /v1/chat/completions (plain, streamed, tool calls), /v1/embeddings and
;;; /v1/messages on 127.0.0.1:18080.
(asdf:load-system "hunchentoot")
(asdf:load-system "com.inuoe.jzon")
(asdf:load-system "flexi-streams")

(defpackage #:mock-llm (:use #:cl) (:export #:start-mock #:*requests*))
(in-package #:mock-llm)

(defvar *requests* '() "Request bodies the mock received, newest first.")

(defun obj (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun text-of (content)
  (if (stringp content)
      content
      (format nil "~{~a~}" (map 'list (lambda (p) (gethash "text" p "")) content))))

(defun read-body ()
  (let ((body (com.inuoe.jzon:parse (hunchentoot:raw-post-data :force-text t))))
    (push body *requests*)
    body))

(defun reply (object)
  (setf (hunchentoot:content-type*) "application/json")
  (com.inuoe.jzon:stringify object))

(defun chat ()
  (let* ((body (read-body))
         (messages (gethash "messages" body))
         (last (aref messages (1- (length messages))))
         (wants-tool (and (gethash "tools" body) (not (equal (gethash "role" last) "tool"))))
         (answer (if (equal (gethash "role" last) "tool")
                     (format nil "It is ~a in Paris." (text-of (gethash "content" last)))
                     (format nil "echo: ~a" (text-of (gethash "content" last))))))
    (if (gethash "stream" body)
        (progn
          (setf (hunchentoot:content-type*) "text/event-stream")
          (let ((out (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
            (flet ((event (delta finish)
                     (format out "data: ~a~%~%"
                             (com.inuoe.jzon:stringify
                              (obj "id" "chatcmpl-1" "object" "chat.completion.chunk" "model" (gethash "model" body)
                                   "choices" (vector (obj "index" 0 "delta" delta "finish_reason" (or finish :null))))))
                     (force-output out)))
              (if wants-tool
                  (progn
                    (event (obj "role" "assistant"
                                "tool_calls" (vector (obj "index" 0 "id" "call_1" "type" "function"
                                                          "function" (obj "name" "get_weather"
                                                                          "arguments" "{\"city\":\"Paris\"}"))))
                           nil)
                    (event (obj) "tool_calls"))
                  (progn
                    (loop for i from 0 below (length answer) by 4
                          do (event (obj "content" (subseq answer i (min (length answer) (+ i 4)))) nil))
                    (event (obj) "stop")))
              (format out "data: [DONE]~%~%")
              (finish-output out)))
          nil)
        (reply
         (obj "id" "chatcmpl-1" "object" "chat.completion" "model" (gethash "model" body)
              "choices" (vector
                         (obj "index" 0
                              "finish_reason" (if wants-tool "tool_calls" "stop")
                              "message" (if wants-tool
                                            (obj "role" "assistant" "content" :null
                                                 "tool_calls" (vector (obj "id" "call_1" "type" "function"
                                                                           "function" (obj "name" "get_weather"
                                                                                           "arguments" "{\"city\":\"Paris\"}"))))
                                            (obj "role" "assistant" "content" answer))))
              "usage" (obj "prompt_tokens" 12 "completion_tokens" 5 "total_tokens" 17))))))

(defun embeddings ()
  (let* ((body (read-body))
         (input (gethash "input" body))
         (inputs (if (stringp input) (vector input) input)))
    (reply (obj "object" "list" "model" (gethash "model" body)
                "data" (map 'vector (lambda (text i)
                                      (obj "object" "embedding" "index" i
                                           "embedding" (vector (float (length text)) 0.5 0.25 0.125)))
                            inputs (loop for i below (length inputs) collect i))
                "usage" (obj "prompt_tokens" 3 "total_tokens" 3)))))

(defun messages ()
  (let* ((body (read-body))
         (msgs (gethash "messages" body))
         (last (aref msgs (1- (length msgs)))))
    (reply (obj "id" "msg_1" "type" "message" "role" "assistant" "model" (gethash "model" body)
                "content" (vector (obj "type" "text" "text" (format nil "echo: ~a" (text-of (gethash "content" last)))))
                "stop_reason" "end_turn"
                "usage" (obj "input_tokens" 10 "output_tokens" 3)))))

(defun start-mock (&key (port 18080))
  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-prefix-dispatcher "/v1/chat/completions" 'chat)
              (hunchentoot:create-prefix-dispatcher "/v1/embeddings" 'embeddings)
              (hunchentoot:create-prefix-dispatcher "/v1/messages" 'messages)))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port port
                                    :access-log-destination nil :message-log-destination nil)))
