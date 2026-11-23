pytest
------

https://docs.pytest.org/

The pytest framework makes it easy to write small, readable tests, and can scale to support complex functional testing for applications and libraries.

Features
~~~~~~~~

- Plain ``assert`` with detailed failure introspection

- Auto-discovery of test modules and functions

- Fixtures with scopes (function, module, session)

- ``@pytest.mark.parametrize``

- Markers and ``-m`` / ``-k`` selection

- JUnit XML reports, plugins (xdist, timeout, cov)

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

The 2024 community survey: FiveAM 28%, Parachute 11%, Rove/Prove 7%, Fiasco 2%, lisp-unit2 2% of respondents.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `FiveAM <https://github.com/lispci/fiveam>`__
     - 2024-09
     - v1.4.3, in cl-repo. Suites, ``is`` with introspection, fixtures, random testing. Most used. Slow releases.
   * - `Parachute <https://shinmera.com/projects/parachute>`__
     - 2026-07
     - 1.5.0, in cl-repo. Moved to Codeberg (2025-08). Extensible reports, fixtures, timing, compatible layers for other frameworks.
   * - `Rove <https://github.com/fukamachi/rove>`__
     - 2026-10
     - Prove's successor. ``ok`` / ``ng`` / ``signals``, hooks, reporters. Marks, per-test timeout and sharding merged upstream (`#77 <https://github.com/fukamachi/rove/pull/77>`__); parametrize, fixtures, JUnit XML still open PRs.
   * - `Fiasco <https://github.com/joaotavora/fiasco>`__
     - 2020-05
     - Stefil successor, interactive debugging. Dormant. In cl-repo.
   * - `lisp-unit2 <https://github.com/AccelerationNet/lisp-unit2>`__
     - 2022-12
     - Tags, fixtures via context. Dormant. Not in cl-repo.
   * - `Prove <https://github.com/fukamachi/prove>`__
     - 2019-12
     - Deprecated by its author. Archived.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `egao1980/rove <https://github.com/egao1980/rove>`__
     - 0.11.0
     - The ``rove`` in cl-repo. Rove plus pytest features: ``deftest-parametrize``, ``deffixture`` (test / suite / session scope), ``:marks`` selection, per-test ``:timeout``, failure diffs, JUnit XML, CI shards. Open upstream as fukamachi/rove #76, #78, #79.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://docs.pytest.org/en/stable/getting-started.html

.. code:: python

    def inc(x):
        return x + 1

    def test_answer():
        assert inc(3) == 5

.. code:: common-lisp

    (asdf:load-system "fiveam")
    (asdf:load-system "parachute")

    (defun inc (x) (1+ x))

FiveAM
^^^^^^

.. code:: common-lisp

    (fiveam:def-suite inc-suite)
    (fiveam:in-suite inc-suite)

    (fiveam:test answer
      (fiveam:is (= 4 (inc 3)))
      (fiveam:is (= 5 (inc 3))))

    (fiveam:run! 'inc-suite)

::


    Running test suite INC-SUITE
     Running test ANSWER .f
     Did 2 checks.
        Pass: 1 (50%)
        Skip: 0 ( 0%)
        Fail: 1 (50%)
     Failure Details:
     --------------------------------
     ANSWER in INC-SUITE []: 

    (INC 3)

     evaluated to 

    4

     which is not 

    =

     to 

    5


     --------------------------------

Parachute
^^^^^^^^^

.. code:: common-lisp

    (parachute:define-test inc-test
      (parachute:is = 4 (inc 3))
      (parachute:is = 5 (inc 3)))

    (parachute:test 'inc-test)

::

            ？ COMMON-LISP-USER::INC-TEST
      0.001 ✔   (is = 4 (inc 3))
      0.000 ✘   (is = 5 (inc 3))
      0.003 ✘ COMMON-LISP-USER::INC-TEST

    ;; Summary:
    Passed:     1
    Failed:     1
    Skipped:    0

    ;; Failures:
       1/   2 tests failed in COMMON-LISP-USER::INC-TEST
    The test form   (inc 3)
    evaluated to    4
    when            5
    was expected to be equal under =.

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/ROVE-GAPS.md

In a project: ``:depends-on ("rove")`` in the ``/tests`` system and ``cl-repo test`` runs it.

.. code:: common-lisp

    (asdf:load-system "rove")
    (setf rove:*enable-colors* nil)

    (defpackage #:inc-tests (:use #:cl #:rove))
    (in-package #:inc-tests)

Parametrize

.. code:: python

    @pytest.mark.parametrize("x,expected", [(1, 2), (2, 3), (3, 5)])
    def test_inc(x, expected):
        assert inc(x) == expected

.. code:: common-lisp

    (deftest-parametrize inc-cases
        ((x expected)
         (1 2)
         (2 3)
         (3 5))
      (ok (= (cl-user::inc x) expected)))

Fixtures

.. code:: python

    @pytest.fixture
    def tmp_file(tmp_path):
        p = tmp_path / "x.txt"
        p.write_text("hi")
        yield p

.. code:: common-lisp

    (deffixture tmp-file ()
      (let ((p (uiop:tmpize-pathname (uiop:merge-pathnames* "x.txt" (uiop:temporary-directory)))))
        (with-open-file (s p :direction :output :if-exists :supersede) (write-string "hi" s))
        (unwind-protect (yield p)
          (uiop:delete-file-if-exists p))))

    (deftest reads-fixture
      (with-fixture (p tmp-file)
        (ok (string= "hi" (uiop:read-file-string p)))))

Markers and timeouts

.. code:: common-lisp

    (deftest (slow-case :marks (:slow) :timeout 5)
      (sleep 0.1)
      (pass "slow but in time"))

Run

.. code:: common-lisp

    (rove:run-suite :inc-tests)

::


    ;; testing 'inc-tests'
    inc-cases/0
      ✓ Expect (= (COMMON-LISP-USER::INC X) EXPECTED) to be true.
    inc-cases/1
      ✓ Expect (= (COMMON-LISP-USER::INC X) EXPECTED) to be true.
    inc-cases/2
      × 0) Expect (= (COMMON-LISP-USER::INC X) EXPECTED) to be true.
    reads-fixture
      ✓ Expect (STRING= "hi" (UIOP/STREAM:READ-FILE-STRING P)) to be true.
    slow-case
      ✓ slow but in time

    × 1 of 5 tests failed

    0) INC-CASES/2
       Expect (= (COMMON-LISP-USER::INC X) EXPECTED) to be true.
           at REPL
         (= #1=(COMMON-LISP-USER::INC X) EXPECTED)
             X = 3
             #1# = 4
             EXPECTED = 5

    Summary:
      1 test failed.
        - INC-CASES/2

For a test system: ``(rove:run :my-app/tests :marks '(not :slow))`` is ``pytest -m "not slow"``; ``:style :junit :junit-file #p"report.xml"`` writes JUnit XML. ``run-suite`` (a package) takes no ``:marks``.
