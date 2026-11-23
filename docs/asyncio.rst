asyncio
-------

https://docs.python.org/3/library/asyncio.html

asyncio is a library to write concurrent code using the async/await syntax.

Features
~~~~~~~~

- Event loop, coroutines and tasks: ``asyncio.run``, ``create_task``, ``gather``

- Timeouts and cancellation: ``wait_for``, ``timeout``

- Synchronization: locks, events, semaphores, queues

- Async network streams and subprocesses

- ``run_in_executor`` / ``to_thread`` for blocking code

- Pluggable loops (uvloop)

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

CL has no async/await. Concurrency is threads, futures, promises or actors.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `bordeaux-threads <https://github.com/sionescu/bordeaux-threads>`__
     - 2026-10
     - 0.9.4 in cl-repo. Portable threads, locks, condition variables, semaphores; ``bt2`` API with atomics. The base layer.
   * - `lparallel <https://github.com/sharplispers/lparallel>`__
     - 2026-04
     - Futures, promises, ``pmap``, channels, task queues. The usual ``concurrent.futures``. Not in cl-repo.
   * - `blackbird <https://github.com/orthecreedence/blackbird>`__
     - 2024-03
     - Promises (``attach``, ``all``, ``catcher``, ``alet``). No loop of its own.
   * - `cl-async <https://github.com/orthecreedence/cl-async>`__
     - 2024-11
     - libuv event loop: timers, TCP, DNS, signals. Occasional fixes only. Not in cl-repo.
   * - `sento <https://github.com/mdbergmann/cl-gserver>`__
     - 2026-09
     - Actors and agents for state and async operations. Active. Not in cl-repo.
   * - `chanl <https://github.com/zkat/chanl>`__
     - 2025-01
     - CSP channels. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `event-protocol <https://github.com/egao1980/event-protocol>`__
     - 0.2.3
     - Event loop protocol: ``defer``, ``sleep*``, ``register-io``, ``submit``, ``wake-call``. ``event-protocol/promises``: ``await``, ``sleep-promise``, ``submit-promise``.
   * - `event-backend-libuv <https://github.com/egao1980/event-backend-libuv>`__
     - 0.2.3
     - Default backend, libuv 1.53 natives through cl-repo. Also ``event-backend-libev`` (Unix), ``event-backend-nio`` (ABCL).
   * - `cl-stack-executors <https://github.com/egao1980/cl-stack-executors>`__
     - 0.1.0
     - Thread pools for ``submit`` (``run_in_executor``). No futures.

Gap: ``await`` takes a ``(resolve reject)`` starter, not a promise; no ``gather`` (`event-protocol#26 <https://github.com/egao1980/event-protocol/issues/26>`__).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: common-lisp

    (asdf:load-system "bordeaux-threads")
    (asdf:load-system "blackbird")

Run things concurrently
^^^^^^^^^^^^^^^^^^^^^^^

.. code:: python

    async def square(i):
        await asyncio.sleep(1)
        return i * i

    asyncio.run(asyncio.gather(*(square(i) for i in (1, 2, 3))))

Threads:

.. code:: common-lisp

    (let* ((start (get-internal-real-time))
           (threads (loop for i from 1 to 3
                          collect (let ((i i))
                                    (bt2:make-thread (lambda () (sleep 1) (* i i)))))))
      (list (mapcar #'bt2:join-thread threads)
            (round (- (get-internal-real-time) start) internal-time-units-per-second)))

::

    ((1 4 9) 1)

lparallel futures (not in cl-repo, not run):

.. code:: common-lisp

    (setf lparallel:*kernel* (lparallel:make-kernel 4))
    (lparallel:pmapcar (lambda (i) (sleep 1) (* i i)) '(1 2 3))

::

    not run: lparallel is not in cl-repo

Locks and queues
^^^^^^^^^^^^^^^^

.. code:: python

    lock = asyncio.Lock()
    async with lock: counter += 1

.. code:: common-lisp

    (let ((lock (bt2:make-lock))
          (counter 0))
      (mapc #'bt2:join-thread
            (loop repeat 10
                  collect (bt2:make-thread
                           (lambda ()
                             (dotimes (_ 1000)
                               (bt2:with-lock-held (lock) (incf counter)))))))
      counter)

::

    10000

Promises
^^^^^^^^

Blackbird chains values and errors like awaited coroutines:

.. code:: common-lisp

    (let (result)
      (blackbird:catcher
       (blackbird:alet ((a (blackbird:promisify 20))
                        (b (blackbird:promisify 22)))
         (setf result (+ a b)))
       (error (e) (setf result e)))
      result)

::

    42

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/event-protocol.md

.. code:: common-lisp

    (asdf:load-system "event-backend-libuv")
    (asdf:load-system "event-protocol/promises")

    (defpackage #:asyncio-demo
      (:use #:cl)
      (:local-nicknames (#:ev #:event-protocol)
                        (#:evp #:stack-event-promises)))
    (in-package #:asyncio-demo)

``asyncio.run`` for a promise:

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

``gather`` of three one-second sleeps on one thread:

.. code:: common-lisp

    (let ((start (get-internal-real-time)))
      (list (run-promise
             (lambda (el)
               (blackbird:all
                (loop for i from 1 to 3
                      collect (let ((i i))
                                (blackbird:attach (evp:sleep-promise el 1)
                                                  (lambda (&rest _) (declare (ignore _)) (* i i))))))))
            (round (- (get-internal-real-time) start) internal-time-units-per-second)))

::

    ((1 4 9) 1)

``run_in_executor``: blocking work on the backend's pool, result back on the loop:

.. code:: common-lisp

    (run-promise (lambda (el)
                   (evp:submit-promise el (lambda () (sleep 0.5) (expt 2 64)))))

::

    18446744073709551616

Errors reject the promise and come back as conditions:

.. code:: common-lisp

    (handler-case
        (run-promise (lambda (el) (evp:defer-promise el (lambda () (error "boom")))))
      (error (e) (princ-to-string e)))

::

    boom
