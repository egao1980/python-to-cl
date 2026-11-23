Requests
--------

https://requests.readthedocs.io/en/latest/

Requests is an elegant and simple HTTP library for Python, built for human beings.

Features
~~~~~~~~

- Keep-Alive & Connection Pooling

- International Domains and URLs

- Sessions with Cookie Persistence

- Browser-style TLS/SSL Verification

- Basic & Digest Authentication

- Familiar dict-like Cookies

- Automatic Content Decompression and Decoding

- Multi-part File Uploads

- SOCKS Proxy Support

- Connection Timeouts

- Streaming Downloads

- Automatic honoring of .netrc

- Chunked HTTP Requests

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Dexador <https://github.com/fukamachi/dexador>`__
     - 2026-10
     - 0.9.16 in cl-repo. Pooling, cookies, redirects, multipart, conditions with restarts. HTTP/1.1 only.
   * - `Drakma <https://edicl.github.io/drakma/>`__
     - 2026-09
     - v2.0.10. The old default. Streams, cookies, proxies. HTTP/1.1 only. Not in cl-repo.
   * - `http2 <https://github.com/zellerin/http2>`__
     - 2026-06
     - HTTP/2 client and server. Low level.

On macOS, Homebrew's ``openssl`` is OpenSSL 4. It removed ``ASN1_STRING_data``, which cl+ssl calls: Dexador fails on the first HTTPS request. Load ``cl-stack-ssl`` (OpenSSL 3.5 LTS from cl-repo) before Dexador.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `cl-stack-http <https://github.com/egao1980/cl-stack-http>`__
     - 0.1.12
     - requests / httpx-shaped facade: sessions, params, JSON, files, auth, streaming, hooks, download / upload.
   * - `http-protocol <https://github.com/egao1980/http-protocol>`__
     - 0.4.3
     - Wire client under it (urllib3 role). Timeouts, retries, proxies, ``.netrc``, gzip / br / zstd.
   * - ``http-backend-async``
     - 0.2.28
     - libuv event loop backend: HTTP/1.1 and HTTP/2, pooling, timeouts on HTTPS. Also ``http-backend-dexador``, ``-winhttp`` (Windows), ``-java`` (ABCL).
   * - `cl-stack-ssl <https://github.com/egao1980/cl-stack-ssl>`__
     - 3.5.9.4
     - OpenSSL 3.5 LTS natives for stock cl+ssl, OS trust store.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://requests.readthedocs.io/en/latest/user/quickstart/

Let's start with loading Dexador. ``cl-stack-ssl`` goes first so cl+ssl gets OpenSSL 3.5. ``jzon`` parses JSON.

.. code:: common-lisp

    (asdf:load-system "cl-stack-ssl")
    (asdf:load-system "quri")
    (asdf:load-system "dexador")
    (asdf:load-system "com.inuoe.jzon")

Simple examples
^^^^^^^^^^^^^^^

Simple GET request, note that Dexador uses multiple return values to return request status, headers, etc.

.. code:: common-lisp

    (dexador:get "https://api.github.com/events")

::

    [{"id":"28048180148","type":"PushEvent","actor":{"id":286562763,"login":"YoheiMiyazaki","display_login":"YoheiMiyazak...

This is how you make an HTTP POST request:

.. code:: common-lisp

    (dexador:post "https://httpbin.org/post" :content '(("key" . "value")))

::

    {
      "args": {},
      "headers": {
        "Accept": "*/*",
        "Content-Length": "9",
        "Content-Type": "application/x-www-form-urlencoded",
        "Host": "httpbin.org",
        "User-Agent": "Dexador/0.9.16 (SBCL 2.4.11.roswell); Darwin; 25.6.0"
      },
      "method": "POST",
      "origin": "127.0.0.1",
      "url": "https://httpbin.org/post",
      "data": "",
      "files": {},
      "form": {
        "key": "value"
      },
      "json": null
    }

Other HTTP methods

.. code:: common-lisp

    (dexador:put "https://httpbin.org/put" :content '(("key" . "value")))

::

    { "args": {}, "headers": { "Accept": "*/*", "Content-Length": "9", "Content-Type": "application/x-www-form-urlencoded...

.. code:: common-lisp

    (dexador:delete "https://httpbin.org/delete")

::

    { "args": {}, "headers": { "Accept": "*/*", "Content-Length": "0", "Host": "httpbin.org", "User-Agent": "Dexador/0.9....

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dexador:head "https://httpbin.org/get")
      (declare (ignore body uri connection))
      (list status (gethash "content-type" headers)))

::

    (200 "application/json")

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dexador:request "https://httpbin.org/get" :method :options)
      (declare (ignore body uri connection))
      (list status (gethash "access-control-allow-methods" headers)))

::

    (200 "GET, POST, HEAD, PUT, DELETE, PATCH, QUERY, OPTIONS")

Passing parameters in URLs
^^^^^^^^^^^^^^^^^^^^^^^^^^

If you wanted to pass key1=value1 and key2=value2 to httpbin.org/get, you would use the following code:

.. code:: common-lisp

    (let ((payload '(("key1" . "value1") ("key2" . "value2"))))
      (multiple-value-bind (body status headers uri connection)
          (dexador:get (quri:make-uri :defaults "https://httpbin.org/get" :query payload))
        (declare (ignore body status headers connection))
        uri))

::

    #<QURI.URI.HTTP:URI-HTTPS https://httpbin.org/get?key1=value1&key2=value2>

You can also pass a list of items as a value:

.. code:: common-lisp

    (let ((payload '(("key1" . "value1") ("key2" . "value2") ("key2" . "value3"))))
      (multiple-value-bind (body status headers uri connection)
          (dexador:get (quri:make-uri :defaults "https://httpbin.org/get" :query payload))
        (declare (ignore body status headers connection))
        uri))

::

    #<QURI.URI.HTTP:URI-HTTPS https://httpbin.org/get?key1=value1&key2=value2&key2=value3>

Response content
^^^^^^^^^^^^^^^^

Dexador decodes text bodies using the charset from ``Content-Type``.

It is possible to get the guessed charset:

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dexador:get "https://api.github.com/events" :force-binary t)
      (declare (ignore status uri connection))
      (dexador.encoding:detect-charset (gethash "content-type" headers) body))

::

    :UTF-8

To manually fix encoding issues you can resort to getting raw binary data for further processing.

.. code:: common-lisp

    (dexador:get "https://api.github.com/events" :force-binary t)

::

    #(91 123 34 105 100 34 58 34 50 56 48 52 56 49 56 48 49 52 56 34 44 34 116 121 112 101 34 58 34 80 117 115 104 69 118...

Binary response content
^^^^^^^^^^^^^^^^^^^^^^^

You can also access the response body as bytes, for non-text requests:

.. code:: common-lisp

    (dexador:get "https://httpbin.org/image/jpeg")

::

    #(255 216 255 224 0 16 74 70 73 70 0 1 1 2 0 28 0 28 0 0 255 254 0 53 69 100 105 116 101 100 32 98 121 32 80 97 117 1...

The gzip and deflate transfer-encodings are automatically decoded for you.

To create an image from binary data, use an image library, e.g. `opticl <https://github.com/slyrus/opticl>`__:

.. code:: common-lisp

    (opticl:read-image-stream
      (flexi-streams:make-in-memory-input-stream
        (dexador:get "https://httpbin.org/image/jpeg"))
      "jpeg")

::

    not run: opticl is not in cl-repo

JSON response contents
^^^^^^^^^^^^^^^^^^^^^^

Dexador doesn't provide built-in support for decoding JSON. Use a JSON library, see :doc:`json <json>`.

.. code:: common-lisp

    (gethash "url" (com.inuoe.jzon:parse (dexador:get "https://httpbin.org/get")))

::

    https://httpbin.org/get

Raw response content
^^^^^^^^^^^^^^^^^^^^

Dexador doesn't provide access to raw socket streams. But you can get binary stream for decompressed body data.

.. code:: common-lisp

    (dexador:get "https://httpbin.org/get" :force-binary t :want-stream t)

::

    #<DEXADOR.KEEP-ALIVE-STREAM:KEEP-ALIVE-STREAM {700AD016B3}>

Custom headers
^^^^^^^^^^^^^^

If you’d like to add HTTP headers to a request, simply pass in an alist to the ``headers`` parameter.

For example, let's specify user-agent:

.. code:: common-lisp

    (dexador:get "https://httpbin.org/headers" :headers '(("user-agent" . "my-app/0.0.1") (:foo . :bar)))

::

    {
      "headers": {
        "Accept": "*/*",
        "Content-Length": "0",
        "Foo": "BAR",
        "Host": "httpbin.org",
        "User-Agent": "my-app/0.0.1"
      }
    }

Note how Dexador automatically converts header names to capitalised kebab case.

More complicated POST requests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Typically, you want to send some form-encoded data — much like an HTML form.
To do this, simply pass an alist to the ``content`` argument.
Your alist of data will automatically be form-encoded when the request is made:

.. code:: common-lisp

    (com.inuoe.jzon:stringify
     (gethash "form"
              (com.inuoe.jzon:parse
               (dexador:post "https://httpbin.org/post"
                             :content '(("key1" . "value1") ("key2" . "value2"))))))

::

    {"key1":"value1","key2":"value2"}

The ``content`` argument can also have multiple values for each key.
This is particularly useful when the form has multiple elements that use the same key:

.. code:: common-lisp

    (com.inuoe.jzon:stringify
     (gethash "form"
              (com.inuoe.jzon:parse
               (dexador:post "https://httpbin.org/post"
                             :content '(("key1" . "value1") ("key1" . "value2") ("key2" . "value3"))))))

::

    {"key1":["value1","value2"],"key2":"value3"}

There are times that you may want to send data that is not form-encoded.
If you pass in a string instead of an alist, that data will be posted directly.

.. code:: common-lisp

    (com.inuoe.jzon:stringify
     (gethash "json"
              (com.inuoe.jzon:parse
               (dexador:post "https://httpbin.org/post"
                             :content "{\"key\": \"value\"}"
                             :headers '((:content-type . "application/json"))))))

::

    {"key":"value"}

POST a Multipart-Encoded File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Dexador directly supports sending Multipart-encoded files.

.. code:: common-lisp

    (com.inuoe.jzon:stringify
     (gethash "files"
              (com.inuoe.jzon:parse
               (dexador:post "https://httpbin.org/post"
                             :content '(("hello.txt" . #p"hello.txt"))))))

::

    {"hello.txt":"Hello world!\n"}

Response Status codes
^^^^^^^^^^^^^^^^^^^^^

Status code is returned as one of the multiple values from Dexador request call:

.. code:: common-lisp

    (nth-value 1 (dexador:get "https://httpbin.org/get"))

::

    200

Bad requests will signal a ``http-request-failed`` condition

.. code:: common-lisp

    (handler-case (dex:get "https://httpbin.org/status/404")
      (dex:http-request-failed (e)
        (format nil "The server returned ~D" (dex:response-status e))))

::

    The server returned 404

You can handle more specialized conditions

.. code:: common-lisp

    (handler-case (dex:get "https://httpbin.org/status/400")
      (dex:http-request-bad-request (e)
        (format nil "Bad request was sent to server: ~D" (dex:response-status e)))
      (dex:http-request-failed (e)
        (format nil "The server returned ~D" (dex:response-status e))))

::

    Bad request was sent to server: 400

You can ignore specific conditions

.. code:: common-lisp

    (handler-bind ((dexador:http-request-not-found #'dexador:ignore-and-continue))
      (dexador:get "https://httpbin.org/status/404"))

Or retry the request.

.. code:: common-lisp

    (handler-case
        (let ((retry-request (dex:retry-request 2 :interval 1)))
          (handler-bind ((dex:http-request-failed retry-request))
            (dex:get "https://httpbin.org/status/404")))
      (dex:http-request-failed (e)
        (format nil "Gave up after retries: ~D" (dex:response-status e))))

::

    Gave up after retries: 404

Response headers
^^^^^^^^^^^^^^^^

We can view the server’s response headers:

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dexador:head "https://httpbin.org/get")
      (declare (ignore body status uri connection))
      (sort (alexandria:hash-table-keys headers) #'string<))

::

    ("access-control-allow-credentials" "access-control-allow-origin" "access-control-expose-headers"
     "content-length" "content-type" "date" "x-content-type-options")

Since header names are case insensitive keys in the headers hash table are converted to lower case.

Cookies
^^^^^^^

Dexador adopts `https://github.com/fukamachi/cl-cookie <https://github.com/fukamachi/cl-cookie>`__ for its cookie management. All functions takes a ``cookie-jar`` instance at ``:cookie-jar``.

.. code:: common-lisp

    (defvar *cookie-jar* (cl-cookie:make-cookie-jar))

    ;; setting cookies
    (dex:get "https://httpbin.org/cookies/set?flavour=oatmeal" :cookie-jar *cookie-jar*)

    ;; getting cookies
    (mapcar (lambda (c) (cons (cl-cookie:cookie-name c) (cl-cookie:cookie-value c)))
            (cl-cookie:cookie-jar-cookies *cookie-jar*))

::

    (("flavour" . "oatmeal"))

Redirection and History
^^^^^^^^^^^^^^^^^^^^^^^

Dexador automatically follows redirects on GET and HEAD requests.
You can limit the count of redirection by specifying ``:max-redirects`` with an integer. The default value is 5.

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dex:get "https://httpbin.org/redirect/2")
      (declare (ignore body headers connection))
      (list status uri))

::

    (200 #<QURI.URI.HTTP:URI-HTTPS https://httpbin.org/get>)

.. code:: common-lisp

    (multiple-value-bind (body status headers uri connection)
        (dex:get "https://httpbin.org/redirect/3" :max-redirects 2)
      (declare (ignore body headers connection))
      (list status uri))

::

    (302 #<QURI.URI.HTTP:URI-HTTPS https://httpbin.org/relative-redirect/1>)

You can use fourth returned parameter to get the URL of the final redirect location.

Dexador doesn't track the history of responses.

Timeouts
^^^^^^^^

You can tell Dexador to stop waiting for a connection after ``connect-timeout`` and waiting to read a response after ``read-timeout`` number of seconds.

.. code:: common-lisp

    (handler-case (dex:get "http://httpbin.org/delay/5" :read-timeout 2)
      (error (c)
        (type-of c)))

::

    SB-SYS:IO-TIMEOUT

``read-timeout`` is not applied to HTTPS: the same call on ``https://`` waits the full 5 seconds.

Drakma
^^^^^^

Drakma is not in cl-repo. Examples from its documentation, not run.

.. code:: common-lisp

    (ql:quickload :drakma)

    (drakma:http-request "https://httpbin.org/get")

    (drakma:http-request "https://httpbin.org/post"
                         :method :post
                         :parameters '(("key" . "value")))

::

    not run: Drakma is not in cl-repo

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/http-client.md

``cl-stack-http`` returns a response object. Response readers and conditions live in ``http-protocol`` (not re-exported yet, `cl-stack-http#26 <https://github.com/egao1980/cl-stack-http/issues/26>`__).

``:auto`` picks Dexador on Unix. The async backend needs its event loop set by hand (`cl-stack-http#27 <https://github.com/egao1980/cl-stack-http/issues/27>`__); it gives HTTP/2 and working HTTPS timeouts.

.. code:: common-lisp

    (asdf:load-system "cl-stack-http")
    (asdf:load-system "http-backend-async")
    (asdf:load-system "event-backend-libuv")

    (defpackage #:requests-demo
      (:use #:cl)
      (:local-nicknames (#:http #:cl-stack-http)
                        (#:hp #:http-protocol)))
    (in-package #:requests-demo)

    (setf http-backend-async:*event-backend-maker*
          (lambda () (event-backend-libuv:make-libuv-backend)))
    (setf http:*preferred-backend* :async)

GET with parameters:

.. code:: python

    r = requests.get("https://httpbin.org/get", params={"key1": "value1", "key2": ["value2", "value3"]})
    r.status_code, r.url

.. code:: common-lisp

    (let ((r (http:get "https://httpbin.org/get"
                       :params '(("key1" . "value1") ("key2" . ("value2" "value3"))))))
      (list (hp:response-status r) (hp:response-url r)))

::

    (200 "https://httpbin.org/get?key1=value1&key2=value2&key2=value3")

JSON in and out:

.. code:: python

    requests.post("https://httpbin.org/post", json={"key": "value"}).json()["json"]

.. code:: common-lisp

    (let ((r (http:post "https://httpbin.org/post" :json '(("key" . "value")))))
      (alexandria:hash-table-alist (gethash "json" (http:response-json r))))

::

    (("key" . "value"))

Form data, request headers, response headers:

.. code:: common-lisp

    (let ((r (http:post "https://httpbin.org/post"
                        :form-data '(("key1" . "value1"))
                        :headers '(("X-Demo" . "1")))))
      (list (hp:response-header r "content-type")
            (gethash "X-Demo" (gethash "headers" (http:response-json r)))
            (alexandria:hash-table-alist (gethash "form" (http:response-json r)))))

::

    ("application/json" "1" (("key1" . "value1")))

``raise_for_status()`` signals ``http-status-error``:

.. code:: common-lisp

    (handler-case (http:get "https://httpbin.org/status/404" :raise-for-status t)
      (hp:http-status-error (e)
        (hp:http-status-error-status e)))

::

    404

Sessions keep cookies, base URL and the connection pool:

.. code:: python

    with requests.Session() as s:
        s.get("https://httpbin.org/cookies/set/flavour/oatmeal")
        s.get("https://httpbin.org/cookies").json()

.. code:: common-lisp

    (http:with-session (s :base-url "https://httpbin.org/")
      (http:session-get s "cookies/set/flavour/oatmeal")
      (alexandria:hash-table-alist
       (gethash "cookies" (http:response-json (http:session-get s "cookies")))))

::

    (("flavour" . "oatmeal"))

Basic auth, timeout:

.. code:: common-lisp

    (hp:response-status
     (http:get "https://httpbin.org/basic-auth/user/passwd"
               :auth '(:basic "user" "passwd")
               :timeout 10.0))

::

    200

HTTP/2 when the server offers it:

.. code:: common-lisp

    (hp:response-http-version (http:get "https://httpbin.org/get" :http-version :auto))

::

    :HTTP/2

Redirect history:

.. code:: common-lisp

    (let ((r (http:get "https://httpbin.org/redirect/2")))
      (list (hp:response-status r) (length (hp:response-history r))))

::

    (200 2)
