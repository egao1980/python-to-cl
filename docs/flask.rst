Flask / FastAPI
---------------

https://flask.palletsprojects.com/

https://fastapi.tiangolo.com/

Flask is a lightweight WSGI web application framework.
FastAPI is a modern, fast (high-performance), web framework for building APIs with Python based on standard Python type hints.

Features
~~~~~~~~

- Routing with URL parameters, ``@app.get("/items/{id}")``

- Request / response objects, JSON bodies, sessions, cookies

- Middleware, blueprints / routers

- FastAPI: validation from type hints (Pydantic), automatic OpenAPI and docs UI, async handlers, dependency injection

- WSGI (Flask) / ASGI (FastAPI) servers: gunicorn, uvicorn

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

The 2024 community survey. Web server: Hunchentoot 36%, Clack/Woo 7%, Clack 2%. Framework: Caveman2 7%, home-grown 4%, Ningle 3%, CLOG 3%, Radiance 3%, easy-routes 2%; two thirds use none.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Hunchentoot <https://github.com/edicl/hunchentoot>`__
     - 2026-05
     - v1.3.1, in cl-repo. Threaded server, ``define-easy-handler``, sessions, TLS. The default.
   * - `easy-routes <https://github.com/mmontone/easy-routes>`__
     - 2026-02
     - Routes with URL parameters and decorators on Hunchentoot. The Flask feel. Not in cl-repo.
   * - `Clack <https://github.com/fukamachi/clack>`__ / Lack
     - 2026-02
     - 2.1.0 in cl-repo. WSGI-style app interface and middleware over Hunchentoot or Woo.
   * - `Woo <https://github.com/fukamachi/woo>`__
     - 2026-10
     - Fast libev server for Clack. Unix. In cl-repo.
   * - `Ningle <https://github.com/fukamachi/ningle>`__
     - 2024-04
     - Sinatra-style micro framework on Clack. Quiet. Not in cl-repo.
   * - `Caveman2 <https://github.com/fukamachi/caveman>`__
     - 2024-05
     - Full-stack framework on Ningle + Mito + Djula. Quiet. Not in cl-repo.
   * - `Snooze <https://github.com/joaotavora/snooze>`__
     - 2024-06
     - REST routes as generic functions, content negotiation. Not in cl-repo.
   * - `Radiance <https://shirakumo.org/projects/radiance>`__
     - 2026-11
     - Web application environment with interfaces and modules. Moved to Codeberg. Not in cl-repo.
   * - `Reblocks <https://github.com/40ants/reblocks>`__
     - 2026-11
     - v0.66.0. Server-side widgets (Weblocks revival). Not in cl-repo.
   * - `CLOG <https://github.com/rabbibotton/clog>`__
     - 2026-10
     - v2.4. GUI and web apps over a websocket, builder IDE. Not in cl-repo.

No CL framework derives validation and OpenAPI from types like FastAPI.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `http-server-protocol <https://github.com/egao1980/http-server-protocol>`__
     - 0.2.0
     - ``serve`` / ``with-server`` for Clack-env apps; backends Hunchentoot (default), Woo, HTTP/2 over TLS. No router.
   * - `openapi-protocol <https://github.com/egao1980/openapi-protocol>`__
     - 0.1.0
     - OpenAPI 3.1 documents from ``defschema`` models, JSON or YAML, served next to the app (``wrap-app``). No request validation or codegen.
   * - ``sse-backend-clack``, ``ws-protocol``
     - 
     - Server-sent events and WebSocket servers.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    app = Flask(__name__)

    @app.get("/hello")
    def hello():
        return {"hello": request.args.get("name", "world")}

.. code:: common-lisp

    (asdf:load-system "cl-stack-ssl")
    (asdf:load-system "hunchentoot")
    (asdf:load-system "clack")
    (asdf:load-system "dexador")
    (asdf:load-system "com.inuoe.jzon")

Hunchentoot
^^^^^^^^^^^

.. code:: common-lisp

    (hunchentoot:define-easy-handler (hello :uri "/hello") ((name :init-form "world"))
      (setf (hunchentoot:content-type*) "application/json")
      (com.inuoe.jzon:stringify (alexandria:plist-hash-table (list "hello" name) :test 'equal)))

    (defvar *acceptor*
      (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                        :address "127.0.0.1" :port 4242
                                        :access-log-destination nil)))

.. code:: common-lisp

    (list (dex:get "http://127.0.0.1:4242/hello")
          (dex:get "http://127.0.0.1:4242/hello?name=lisp"))

::

    ("{\"hello\":\"world\"}" "{\"hello\":\"lisp\"}")

.. code:: common-lisp

    (hunchentoot:stop *acceptor*)

Clack
^^^^^

An app is a function from the environment plist to ``(status headers body)``:

.. code:: common-lisp

    (defvar *handler*
      (clack:clackup (lambda (env)
                       (list 200 '(:content-type "text/plain")
                             (list (format nil "~a ~a" (getf env :request-method) (getf env :path-info)))))
                     :server :hunchentoot :address "127.0.0.1" :port 4243 :silent t))

.. code:: common-lisp

    (dex:post "http://127.0.0.1:4243/items/1" :content "x")

::

    POST /items/1

.. code:: common-lisp

    (clack:stop *handler*)

easy-routes
^^^^^^^^^^^

Not in cl-repo. From its documentation, not run:

.. code:: common-lisp

    (easy-routes:defroute item ("/items/:id" :method :get) (&get q)
      (format nil "item ~a, q=~a" id q))

    (hunchentoot:start (make-instance 'easy-routes:easy-routes-acceptor :port 8080))

::

    not run: easy-routes is not in cl-repo

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/http-server.md

.. code:: common-lisp

    (asdf:load-system "http-server-backend-hunchentoot")
    (asdf:load-system "json-backend-jzon")
    (asdf:load-system "openapi-protocol")

A Clack-env app served by the protocol:

.. code:: common-lisp

    (defun api (env)
      (cond ((string= (getf env :path-info) "/pets/1")
             (list 200 '(:content-type "application/json")
                   (list (stack-json:encode '(("name" . "Rex"))))))
            (t '(404 (:content-type "text/plain") ("not found")))))

FastAPI's ``/openapi.json``, from a model:

.. code:: python

    class Pet(BaseModel):
        name: str

    @app.get("/pets/{id}", response_model=Pet, operation_id="getPet")
    def get_pet(id: int): ...

.. code:: common-lisp

    (schema-protocol:defschema pet ()
      (name string))

    (defparameter *doc*
      (stack-openapi:make-document
       :title "Pets" :version "1.0.0"
       :paths (list (cons "/pets/{id}"
                          (stack-openapi:path-item
                           :get (stack-openapi:operation
                                 :operation-id "getPet"
                                 :responses (list (cons 200 (stack-openapi:response
                                                             :description "ok" :schema 'pet)))))))
       :schemas '(pet)))

.. code:: common-lisp

    (stack-http-server:with-server (s (stack-openapi:wrap-app #'api *doc*)
                                      :host "127.0.0.1" :port 4244)
      (list (dex:get "http://127.0.0.1:4244/pets/1")
            (com.inuoe.jzon:stringify
             (gethash "paths" (com.inuoe.jzon:parse (dex:get "http://127.0.0.1:4244/openapi.json"))))))

::

    ("{\"name\":\"Rex\"}"
     "{\"/pets/{id}\":{\"get\":{\"operationId\":\"getPet\",\"responses\":{\"200\":{\"content\":{\"application/json\":{\"schema\":{\"$ref\":\"#/components/schemas/pet\"}}},\"description\":\"ok\"}}}}}")

The same document as YAML:

.. code:: common-lisp

    (stack-openapi:encode *doc* :format :yaml)

::

    components: 
      schemas: 
        pet: 
          additionalProperties: false
          properties: 
            name: 
              type: string
          required: 
            - name
          title: pet
          type: object
    info: 
      title: Pets
      version: 1.0.0
    openapi: 3.1.0
    paths: 
      "/pets/{id}": 
        get: 
          operationId: getPet
          responses: 
            "200": 
              content: 
                application/json: 
                  schema: 
                    $ref: "#/components/schemas/pet"
              description: ok
