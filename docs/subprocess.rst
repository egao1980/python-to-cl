subprocess
----------

https://docs.python.org/3/library/subprocess.html

The subprocess module allows you to spawn new processes, connect to their input/output/error pipes, and obtain their return codes.

Features
~~~~~~~~

- ``run`` with ``capture_output``, ``input``, ``timeout``, ``check`` (``CalledProcessError``)

- ``Popen`` for streaming pipes, ``wait``, ``kill``

- ``cwd``, ``env``, argument lists or ``shell=True``

- psutil (separate): process and system information

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `UIOP <https://asdf.common-lisp.dev/uiop.html#UIOP_002fRUN_002dPROGRAM>`__
     - ships with ASDF
     - ``run-program`` (sync, capture to string, error on status) and ``launch-program`` (streams, ``wait-process``, ``terminate-process``). Portable. Use this first.
   * - `cmd <https://github.com/ruricolist/cmd>`__
     - 2026-03
     - Shell-like DSL over ``launch-program``, no shell interpolation, redirections, Windows. Not in cl-repo.
   * - `inferior-shell <https://github.com/fare/inferior-shell>`__
     - 2024-04
     - Pipelines as s-expressions, local or ssh. Not in cl-repo.
   * - `async-process <https://github.com/lem-project/async-process>`__
     - 2025-12
     - Non-blocking processes with a C helper (Lem uses it). Not in cl-repo.
   * - `external-program <https://github.com/sellout/external-program>`__
     - 2023-12
     - Old portable wrapper. Dormant. Not in cl-repo.
   * - ``sb-ext:run-program``
     - SBCL
     - Implementation API, full control (pty, environment).

No psutil equivalent. `osicat <https://github.com/osicat/osicat>`__ covers a little of POSIX.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `process-protocol <https://github.com/egao1980/process-protocol>`__
     - 0.4.1
     - ``run`` (octets out / err, ``:input``, ``:env`` merge, ``:timeout`` → ``process-timeout-error``), ``launch`` / ``wait`` / ``kill :tree``, process groups, signals.
   * - `process-backend-uiop <https://github.com/egao1980/process-backend-uiop>`__
     - 0.3.4
     - Default backend; Windows tree kill via job objects.
   * - `pty-protocol <https://github.com/egao1980/pty-protocol>`__
     - 0.1.0
     - Programs on pseudo-terminals (Unix pty, Windows ConPTY).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

run and capture
^^^^^^^^^^^^^^^

.. code:: python

    subprocess.run(["echo", "hello"], capture_output=True, text=True).stdout

.. code:: common-lisp

    (uiop:run-program '("echo" "hello") :output :string)

::

    hello

.. code:: python

    subprocess.run(["false"], check=True)  # CalledProcessError

.. code:: common-lisp

    (handler-case (uiop:run-program '("false"))
      (uiop:subprocess-error (e) (uiop:subprocess-error-code e)))

::

    1

.. code:: common-lisp

    (multiple-value-list
     (uiop:run-program '("sh" "-c" "tr a-z A-Z; echo oops >&2; exit 3")
                       :input (make-string-input-stream "hi")
                       :output :string :error-output :string
                       :ignore-error-status t))

::

    ("HI" "oops
    "
     3)

Popen
^^^^^

.. code:: common-lisp

    (let ((p (uiop:launch-program '("cat") :input :stream :output :stream)))
      (write-line "ping" (uiop:process-info-input p))
      (close (uiop:process-info-input p))
      (list (read-line (uiop:process-info-output p))
            (uiop:wait-process p)))

::

    ("ping" 0)

No ``timeout=``: wait in a thread and ``uiop:terminate-process``.

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/process.md

.. code:: common-lisp

    (asdf:load-system "process-backend-uiop")

    (defpackage #:subprocess-demo
      (:use #:cl)
      (:local-nicknames (#:proc #:stack-process)))
    (in-package #:subprocess-demo)

Output is octets; exit code, stdout and stderr are three values:

.. code:: common-lisp

    (multiple-value-bind (code out err)
        (proc:run '("sh" "-c" "echo $GREETING; echo oops >&2")
                  :env '(("GREETING" . "hello")))
      (list code
            (babel:octets-to-string out :encoding :utf-8)
            (babel:octets-to-string err :encoding :utf-8)))

::

    (0 "hello
    "
     "oops
    ")

.. code:: python

    subprocess.run(["sleep", "5"], timeout=1)  # TimeoutExpired

.. code:: common-lisp

    (handler-case (proc:run '("sleep" "5") :timeout 1)
      (proc:process-timeout-error (e) (type-of e)))

::

    PROC:PROCESS-TIMEOUT-ERROR

.. code:: common-lisp

    (let ((h (proc:launch '("cat") :input :stream :output :stream)))
      (write-line "ping" (proc:stdin h))
      (finish-output (proc:stdin h))
      (close (proc:stdin h))
      (list (read-line (proc:stdout h)) (proc:wait h)))

::

    ("ping" 0)
