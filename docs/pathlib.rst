pathlib / os.path / shutil
--------------------------

https://docs.python.org/3/library/pathlib.html

pathlib offers classes representing filesystem paths with semantics appropriate for different operating systems.

Features
~~~~~~~~

- ``Path`` objects, ``/`` to join, ``parent``, ``name``, ``stem``, ``suffix``

- ``exists``, ``is_file``, ``stat``, ``iterdir``, ``glob``, ``rglob``, ``walk``

- ``read_text`` / ``write_text`` / ``read_bytes``

- ``mkdir(parents=True)``, ``rename``, ``unlink``, ``resolve``

- shutil: ``copytree``, ``rmtree``, ``which``; tempfile: temporary files and directories

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

CL has pathnames in the standard. They are portable and awkward (``:name`` / ``:type`` / ``:directory`` split, wildcards, implementation-specific namestrings).

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `UIOP <https://asdf.common-lisp.dev/uiop.html>`__
     - ships with ASDF
     - ``subpathname``, ``merge-pathnames*``, ``directory-files``, ``subdirectories``, ``read-file-string``, ``with-temporary-file``, ``delete-directory-tree``, ``getcwd``. Use this first.
   * - `cl-fad <https://github.com/edicl/cl-fad>`__
     - 2026-08
     - v0.7.6, in cl-repo. ``walk-directory``, ``copy-file``, ``delete-directory-and-files``. Mostly superseded by UIOP.
   * - `pathname-utils <https://shinmera.com/projects/pathname-utils>`__
     - 2026-06
     - 1.1.0, in cl-repo. Pure pathname operations: ``parent``, ``to-directory``, ``relative-pathname``. Moved to Codeberg.
   * - `filepaths <https://github.com/fosskers/filepaths>`__
     - 2026-01
     - v1.1.0. pathlib-like ``join``, ``extension``, ``parent`` on strings or pathnames. Not in cl-repo.
   * - `osicat <https://github.com/osicat/osicat>`__
     - 2025-10
     - v0.7.0. POSIX: ``stat``, symlinks, permissions, environment. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `cl-stack-pathlib <https://github.com/egao1980/cl-stack-pathlib>`__
     - 0.10.0
     - pathlib as CLOS: path values bound to a filesystem (local, memory, read-only ``zip://``). ``join``, ``stem``, ``suffix``, ``glob``, ``rglob``, ``walk`` (gitignore aware), ``read-text``, atomic writes, ``stat``, SHA-256, ``copytree``, ``rmtree``, ``which``, temp files. Restarts such as ``create-parents``.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: common-lisp

    (asdf:load-system "cl-fad")
    (asdf:load-system "pathname-utils")

    (defparameter *tmp* (uiop:ensure-directory-pathname
                         (uiop:merge-pathnames* (format nil "p2cl-~36r/" (random (expt 36 8)))
                                                (uiop:temporary-directory))))

Join and parts
^^^^^^^^^^^^^^

.. code:: python

    p = Path("/opt/pkg") / "lib" / "libssl.so.3"
    p.parent, p.name, p.stem, p.suffix

.. code:: common-lisp

    (let ((p (uiop:subpathname #p"/opt/pkg/" "lib/libssl.so.3")))
      (list p (uiop:pathname-directory-pathname p) (file-namestring p)
            (pathname-name p) (pathname-type p)))

::

    (#P"/opt/pkg/lib/libssl.so.3" #P"/opt/pkg/lib/" "libssl.so.3" "libssl.so" "3")

pathname-utils:

.. code:: common-lisp

    (list (pathname-utils:parent #p"/opt/pkg/lib/")
          (pathname-utils:to-directory #p"/opt/pkg/lib")
          (pathname-utils:relative-pathname #p"/opt/pkg/" #p"/opt/pkg/lib/x.so"))

::

    (#P"/opt/pkg/" #P"/opt/pkg/" #P"lib/x.so")

Write, read, list
^^^^^^^^^^^^^^^^^

.. code:: python

    (tmp / "a" / "b").mkdir(parents=True)
    (tmp / "a" / "b" / "x.txt").write_text("hi")
    sorted(p.name for p in tmp.rglob("*.txt"))

.. code:: common-lisp

    (let ((f (uiop:subpathname *tmp* "a/b/x.txt")))
      (ensure-directories-exist f)
      (with-open-file (s f :direction :output :if-exists :supersede)
        (write-string "hi" s))
      (with-open-file (s (uiop:subpathname *tmp* "a/y.txt") :direction :output :if-exists :supersede)
        (write-string "there" s))
      (list (uiop:read-file-string f)
            (mapcar #'file-namestring (uiop:directory-files (uiop:subpathname *tmp* "a/")))))

::

    ("hi" ("y.txt"))

Recursive walk with cl-fad:

.. code:: common-lisp

    (let (names)
      (cl-fad:walk-directory *tmp* (lambda (p) (push (file-namestring p) names))
                             :test (lambda (p) (equal (pathname-type p) "txt")))
      (sort names #'string<))

::

    ("x.txt" "y.txt")

Temporary files, rmtree, which
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: common-lisp

    (list (uiop:with-temporary-file (:pathname p :stream s :direction :output)
            (write-string "tmp" s)
            (finish-output s)
            (pathname-type p))
          (not (null (uiop:delete-directory-tree *tmp* :validate t)))
          (probe-file *tmp*))

::

    ("tmp" T NIL)

No ``which`` in UIOP; ``uiop:run-program`` searches ``PATH`` itself.

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack-pathlib

.. code:: common-lisp

    (asdf:load-system "cl-stack-pathlib")

    (defpackage #:pathlib-demo
      (:use #:cl)
      (:local-nicknames (#:p #:stack-pathlib)))
    (in-package #:pathlib-demo)

.. code:: common-lisp

    (let ((x (p:join "/opt/pkg" "lib" "libssl.so.3")))
      (list (p:to-string x) (p:to-string (p:parent x)) (p:name x) (p:stem x) (p:suffix x)))

::

    ("/opt/pkg/lib/libssl.so.3" "/opt/pkg/lib" "libssl.so.3" "libssl.so" ".3")

.. code:: common-lisp

    (p:with-temp-directory (tmp)
      (p:with-auto-create-parents
        (p:write-text (p:join tmp "a" "b" "x.txt") "hi"))
      (p:write-text (p:join tmp "a" "y.txt") "there")
      (list (p:read-text (p:join tmp "a" "b" "x.txt"))
            (sort (mapcar #'p:name (p:rglob tmp "*.txt")) #'string<)
            (p:content-hash (p:join tmp "a" "y.txt"))))

::

    ("hi" ("x.txt" "y.txt") "sha256:e244f187f696561d5fd7e00f618e7ba641dc52e3c137380f6fa23a854b773aac")

Missing parents are a condition with restarts, not an exception to retry by hand:

.. code:: common-lisp

    (p:with-temp-directory (tmp)
      (handler-case (p:write-text (p:join tmp "no" "such" "f.txt") "x")
        (p:missing-parent (e) (type-of e))))

::

    P:MISSING-PARENT

In-memory filesystem (tests), ``which``:

.. code:: common-lisp

    (list (p:with-filesystem ((p:make-memory-filesystem))
            (p:with-auto-create-parents (p:write-text "/a/b.txt" "in memory"))
            (p:read-text "/a/b.txt"))
          (not (null (p:which "sh"))))

::

    ("in memory" T)
