HTTPX / aiohttp
---------------

https://www.python-httpx.org/

https://docs.aiohttp.org/

HTTPX is a fully featured HTTP client for Python 3, which provides sync and async APIs, and support for both HTTP/1.1 and HTTP/2.
aiohttp is an asynchronous HTTP client/server framework for asyncio.

Features
~~~~~~~~

- A broadly requests-compatible API

- Standard synchronous interface, but with async support if you need it

- HTTP/1.1 and HTTP/2 support

- Strict timeouts everywhere

- Calling into WSGI / ASGI applications directly

- aiohttp: client and server, client and server WebSockets, middlewares

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

See :doc:`requests <requests>` for the synchronous clients (Dexador, Drakma).

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `http2 <https://github.com/zellerin/http2>`__
     - 2026-06
     - HTTP/2 client and server, threaded or polling. cl-repo carries the egao1980 fork (2.1.8).
   * - `cl-async <https://github.com/orthecreedence/cl-async>`__
     - 2024-11
     - libuv event loop, TCP, timers. Occasional fixes only. Not in cl-repo.
   * - `carrier <https://github.com/orthecreedence/carrier>`__
     - 2023-11
     - Async HTTP client on cl-async. Minimal, dormant. Not in cl-repo.
   * - `blackbird <https://github.com/orthecreedence/blackbird>`__
     - 2024-03
     - Promises. The usual glue for async code.
   * - `Dexador <https://github.com/fukamachi/dexador>`__ + threads
     - 2026-10
     - No async API. Run requests on threads.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `cl-stack-http <https://github.com/egao1980/cl-stack-http>`__
     - 0.1.12
     - Every verb has an ``-async`` twin returning a Blackbird promise.
   * - `http-backend-async <https://github.com/egao1980/http-backend-async>`__
     - 0.2.28
     - Non-blocking HTTP/1.1 + HTTP/2 (ALPN, multiplexed streams on one pooled connection) on ``event-protocol``.
   * - `ws-protocol <https://github.com/egao1980/ws-protocol>`__
     - 0.4.2
     - WebSocket client and server, HTTP/1.1 Upgrade and HTTP/2 (RFC 8441).

Gaps: no ``asyncio.run`` / ``gather`` helper (`event-protocol#26 <https://github.com/egao1980/event-protocol/issues/26>`__); the async backend needs ``*event-backend-maker*`` set (`cl-stack-http#27 <https://github.com/egao1980/cl-stack-http/issues/27>`__).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://www.python-httpx.org/async/

.. code:: common-lisp

    (asdf:load-system "cl-stack-ssl")
    (asdf:load-system "dexador")
    (asdf:load-system "bordeaux-threads")
    (asdf:load-system "http2")

Concurrent requests
^^^^^^^^^^^^^^^^^^^

.. code:: python

    async with httpx.AsyncClient() as client:
        rs = await asyncio.gather(*[client.get("https://httpbin.org/delay/1") for _ in range(3)])
    [r.status_code for r in rs]

Dexador on threads:

.. code:: common-lisp

    (let* ((start (get-internal-real-time))
           (threads (loop repeat 3
                          collect (bt2:make-thread
                                   (lambda ()
                                     (nth-value 1 (dex:get "https://httpbin.org/delay/1")))))))
      (list (mapcar #'bt2:join-thread threads)
            (round (- (get-internal-real-time) start) internal-time-units-per-second)))

::

    ((200 200 200) 1)

carrier on cl-async (from its README, not in cl-repo):

.. code:: common-lisp

    (as:with-event-loop ()
      (blackbird:alet ((res (carrier:request "http://httpbin.org/get" :return-body t)))
        (format t "got response: ~a~%" (babel:octets-to-string res))))

::

    not run: cl-async and carrier are not in cl-repo

HTTP/2
^^^^^^

.. code:: python

    with httpx.Client(http2=True) as client:
        client.get("https://httpbin.org/get").http_version

.. code:: common-lisp

    (http2/client:retrieve-url "https://httpbin.org/get")

::

    { "args": {}, "headers": { "Host": "httpbin.org" }, "method": "GET", "origin": "127.0.0.1", "url": "https://httpbin.o...

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/http-client.md

.. code:: common-lisp

    (asdf:load-system "cl-stack-http")
    (asdf:load-system "http-backend-async")
    (asdf:load-system "event-backend-libuv")
    (asdf:load-system "event-protocol/promises")

    (defpackage #:httpx-demo
      (:use #:cl)
      (:local-nicknames (#:http #:cl-stack-http)
                        (#:hp #:http-protocol)
                        (#:ev #:event-protocol)
                        (#:evp #:stack-event-promises)))
    (in-package #:httpx-demo)

    (setf http-backend-async:*event-backend-maker*
          (lambda () (event-backend-libuv:make-libuv-backend)))
    (setf http:*preferred-backend* :async)

``asyncio.run`` for a promise, until event-protocol has one:

.. code:: common-lisp

    (defun run-promise (make-promise)
      "Make a libuv loop, call MAKE-PROMISE with it, run the loop until the promise settles."
      (let* ((eb (event-backend-libuv:make-libuv-backend))
             (el (ev:make-event-loop eb)))
        (ev:with-event-backend (eb)
          (ev:with-event-loop-var (el)
            (evp:await
             (lambda (resolve reject)
               (blackbird:catcher (blackbird:attach (funcall make-promise el) resolve)
                 (error (e) (funcall reject e)))))))))

Concurrent requests (one HTTP/2 connection):

.. code:: common-lisp

    (let ((start (get-internal-real-time)))
      (list (mapcar #'hp:response-status
                    (run-promise
                     (lambda (el)
                       (declare (ignore el))
                       (blackbird:all
                        (loop repeat 3 collect (http:get-async "https://httpbin.org/delay/1"))))))
            (round (- (get-internal-real-time) start) internal-time-units-per-second)))

::

    ((200 200 200) 2)

The sync API on the same backend speaks HTTP/2:

.. code:: common-lisp

    (hp:response-http-version (http:get "https://httpbin.org/get"))

::

    :HTTP/2

Strict timeouts:

.. code:: python

    httpx.get("https://httpbin.org/delay/5", timeout=2.0)  # httpx.ReadTimeout

.. code:: common-lisp

    (handler-case (http:get "https://httpbin.org/delay/5" :timeout 2.0)
      (hp:http-timeout-error (e) (type-of e)))

::

    HP:HTTP-TIMEOUT-ERROR

Streaming:

.. code:: python

    with httpx.stream("GET", "https://httpbin.org/stream/3") as r:
        for line in r.iter_lines(): ...

.. code:: common-lisp

    (let ((n 0))
      (http:with-stream (r :get "https://httpbin.org/stream/3")
        (http:map-response-lines r (lambda (line) (declare (ignore line)) (incf n))))
      n)

::

    3
