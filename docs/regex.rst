re / regex
----------

https://docs.python.org/3/library/re.html

https://github.com/mrabarnett/mrab-regex

This module provides regular expression matching operations similar to those found in Perl.
regex is a backwards-compatible replacement with additional functionality.

Features
~~~~~~~~

- ``match``, ``search``, ``findall``, ``finditer``, ``sub``, ``split``

- Groups, named groups, backreferences, lookaround

- Flags: ``IGNORECASE``, ``MULTILINE``, ``DOTALL``, ``VERBOSE``

- ``re.compile``

- regex: Unicode properties, atomic groups, possessive quantifiers, fuzzy matching

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `CL-PPCRE <https://github.com/edicl/cl-ppcre>`__
     - 2025-06
     - v2.1.2, in cl-repo. Perl-compatible, fast, regexes as strings or s-expressions, named registers, compiler macros for constant patterns. The standard. ``cl-ppcre-unicode`` adds ``\p{...}``.
   * - `one-more-re-nightmare <https://github.com/telekons/one-more-re-nightmare>`__
     - 2025-10
     - Compiles regexes to DFAs, often faster than CL-PPCRE. No backreferences or lookaround. Not in cl-repo.
   * - `str <https://github.com/vindarel/cl-str>`__
     - 2025-09
     - 0.21 in cl-repo. String helpers (``split``, ``replace-all``, ``starts-with-p``) over CL-PPCRE.

cl-stack
^^^^^^^^

No regex protocol. cl-stack pins CL-PPCRE 2.1.2 and uses it (e.g. ``cl-stack-pathlib:grep`` takes a matcher function such as ``(lambda (line) (ppcre:scan re line))``).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://docs.python.org/3/howto/regex.html

.. code:: common-lisp

    (asdf:load-system "cl-ppcre")
    (asdf:load-system "cl-ppcre-unicode")

search and groups
^^^^^^^^^^^^^^^^^

.. code:: python

    m = re.search(r"(\d+)-(\d+)", "call 555-1234 now")
    m.group(0), m.groups()

.. code:: common-lisp

    (multiple-value-list (ppcre:scan-to-strings "(\\d+)-(\\d+)" "call 555-1234 now"))

::

    ("555-1234" #("555" "1234"))

.. code:: common-lisp

    (ppcre:register-groups-bind ((#'parse-integer area number))
        ("(\\d+)-(\\d+)" "call 555-1234 now")
      (list area number))

::

    (555 1234)

Named groups:

.. code:: common-lisp

    (setf ppcre:*allow-named-registers* t)

    (ppcre:register-groups-bind (year month)
        ("(?<year>\\d{4})-(?<month>\\d{2})" "released 2026-11")
      (list year month))

::

    ("2026" "11")

findall, sub, split
^^^^^^^^^^^^^^^^^^^

.. code:: python

    re.findall(r"\b\w+@\w+\.\w+\b", "ada@x.org, grace@y.com")
    re.sub(r"(\w+)@", lambda m: m.group(1).upper() + "@", "ada@x.org")
    re.split(r"\s*,\s*", "a , b,c")

.. code:: common-lisp

    (list (ppcre:all-matches-as-strings "\\b\\w+@\\w+\\.\\w+\\b" "ada@x.org, grace@y.com")
          (ppcre:regex-replace-all "(\\w+)@" "ada@x.org"
                                   (lambda (match name) (declare (ignore match))
                                     (format nil "~:@(~a~)@" name))
                                   :simple-calls t)
          (ppcre:split "\\s*,\\s*" "a , b,c"))

::

    (("ada@x.org" "grace@y.com") "ADA@x.org" ("a" "b" "c"))

Flags and compile
^^^^^^^^^^^^^^^^^

``(?i)``, ``(?m)``, ``(?s)``, ``(?x)`` inline, or keyword arguments to ``create-scanner``:

.. code:: common-lisp

    (let ((scanner (ppcre:create-scanner "^hello" :case-insensitive-mode t :multi-line-mode t)))
      (ppcre:all-matches-as-strings scanner (format nil "Hello~%HELLO~%bye")))

::

    ("Hello" "HELLO")

Regexes as s-expressions, no escaping:

.. code:: common-lisp

    (ppcre:scan-to-strings '(:sequence (:register (:greedy-repetition 1 nil :digit-class))
                             "px")
                           "width: 120px")

::

    120px

Unicode properties with ``cl-ppcre-unicode``:

.. code:: common-lisp

    (ppcre:all-matches-as-strings "\\p{Lu}\\p{Ll}+" "Ärger in Zürich")

::

    ("Ärger" "Zürich")
