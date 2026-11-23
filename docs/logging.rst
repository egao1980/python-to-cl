logging / structlog
-------------------

https://docs.python.org/3/library/logging.html

https://www.structlog.org/

This module defines functions and classes which implement a flexible event logging system for applications and libraries.
structlog makes logging in Python faster, less painful, and more powerful by adding structure to your log entries.

Features
~~~~~~~~

- Hierarchical named loggers, levels

- Handlers (stream, file, rotating, syslog) and formatters

- ``extra=`` / ``LoggerAdapter`` context

- ``dictConfig`` / ``fileConfig``

- structlog: key-value events, bound context, JSON output

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `log4cl <https://github.com/sharplispers/log4cl>`__
     - 2023-03
     - 1.1.4 in cl-repo. log4j-style: category hierarchy from packages, appenders (console, file, daily rolling), patterns, SLIME/Sly integration. Quiet upstream.
   * - `log4cl-extras <https://github.com/40ants/log4cl-extras>`__
     - 2026-11
     - Adds JSON output, context fields (``with-fields``), tracebacks to log4cl. Not in cl-repo.
   * - `verbose <https://shinmera.com/projects/verbose>`__
     - 2026-07
     - Threaded, faceted logging with categories. Moved to Codeberg. Not in cl-repo.
   * - `vom <https://github.com/orthecreedence/vom>`__
     - 2024-08
     - Tiny: levels per package, one stream. In cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `log-protocol <https://github.com/egao1980/log-protocol>`__
     - 0.1.2
     - One call site: levels, ``with-context`` fields, filters, async queue, text or structured (JSON / sexp) layout.
   * - `log-backend-log4cl <https://github.com/egao1980/log-backend-log4cl>`__
     - 0.1.1
     - Default backend. Also ``log-backend-vom``.
   * - `telemetry-protocol <https://github.com/egao1980/telemetry-protocol>`__
     - 0.2.0
     - Traces and spans, OTLP/HTTP export (OpenTelemetry role).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://docs.python.org/3/howto/logging.html

.. code:: python

    logging.basicConfig(level=logging.INFO)
    log = logging.getLogger(__name__)
    log.info("user %s logged in", "ada")
    log.debug("hidden")

.. code:: common-lisp

    (asdf:load-system "log4cl")
    (asdf:load-system "vom")

log4cl
^^^^^^

The logger is the current package (here ``CL-USER``). ``:sane`` resets to one console appender.

.. code:: common-lisp

    (log:config :sane :info :pattern "%p %c - %m%n" :stream *standard-output*)
    (log:info "user ~a logged in" "ada")
    (log:debug "hidden")

::

    INFO CL-USER - user ada logged in

Per-category levels:

.. code:: common-lisp

    (log:config :sane :warn :pattern "%p %c - %m%n" :stream *standard-output*)
    (log:config '(cl-user) :debug)
    (log:debug "now visible")

::

    DEBUG CL-USER - now visible

vom
^^^

.. code:: common-lisp

    (vom:config t :info)
    (let ((vom:*log-stream* *standard-output*))
      (vom:info "user ~a logged in" "ada")
      (vom:debug "hidden"))

::

      <INFO> [15:46:25] common-lisp-user - user ada logged in

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/logging.md

.. code:: common-lisp

    (asdf:load-system "log-backend-log4cl")
    (asdf:load-system "json-backend-jzon")

Text with fields:

.. code:: common-lisp

    (stack-log:configure :level :info :layout :text :stream *standard-output*)
    (stack-log:with-context (:request-id "abc")
      (stack-log:info "handled" :status 200))

::

    2026-11-23T15:46:25Z INFO COMMON-LISP-USER - handled request-id=abc status=200

structlog-style JSON:

.. code:: python

    structlog.get_logger().bind(request_id="abc").info("handled", status=200)

.. code:: common-lisp

    (stack-log:configure :level :info :layout :structured :format :json :stream *standard-output*)
    (stack-log:with-context (:request-id "abc")
      (stack-log:info "handled" :status 200))

::

    {"fields":{"request-id":"abc","status":200},"level":"INFO","logger":"COMMON-LISP-USER","msg":"handled","ts":"2026-11-23T15:46:25Z"}

Filters:

.. code:: common-lisp

    (stack-log:configure :level :info :layout :text :stream *standard-output*)
    (stack-log:add-filter (lambda (level logger msg fields)
                            (declare (ignore level logger fields))
                            (not (search "password" msg)))
                          :name :no-secrets)
    (stack-log:info "password reset")
    (stack-log:info "kept")

::

    2026-11-23T15:46:25Z INFO COMMON-LISP-USER - kept
