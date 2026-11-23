json / orjson
-------------

https://docs.python.org/3/library/json.html

https://github.com/ijl/orjson

json exposes an API familiar to users of the standard library marshal and pickle modules.
orjson is a fast, correct JSON library for Python.

Features
~~~~~~~~

- ``loads`` / ``dumps`` between JSON text and dicts, lists, strings, numbers, ``None``

- ``load`` / ``dump`` on files

- Pretty printing: ``indent``, ``sort_keys``

- Hooks: ``object_hook``, ``default``

- ``JSONDecodeError`` with line and column

- orjson: native dataclass, datetime, UUID and numpy serialization, RFC 8259 strict

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

The 2024 community survey: cl-json 23%, jzon 19%, yason 7%, shasht 4%, jonathan 2%, st-json 2%, jsown 1% of respondents.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `jzon <https://github.com/Zulu-Inuoe/jzon>`__
     - 2026-03
     - v1.1.4, in cl-repo. Correct, safe, fast (1.0 noted in the 2023–2024 review). Hash tables + vectors, streaming writer.
   * - `shasht <https://github.com/yitzchak/shasht>`__
     - 2025-11
     - Fast, configurable mapping, streaming. Not in cl-repo.
   * - `yason <https://github.com/phmarek/yason>`__
     - 2026-10
     - Maintained. Alist / plist / hash-table mapping, stream encoder.
   * - `jsown <https://github.com/madnificent/jsown>`__
     - 2026-07
     - Fast reader, ``(:obj ...)`` lists. Not in cl-repo.
   * - `st-json <https://github.com/marijnh/ST-JSON>`__
     - 2026-10
     - Small, strict, ``jso`` objects. Not in cl-repo.
   * - `cl-json <https://github.com/sharplispers/cl-json>`__
     - 2022-05
     - Most used in the survey. Decodes ``[]``, ``{}``, ``null`` and ``false`` all to ``nil``. Not in cl-repo.
   * - `jonathan <https://github.com/Rudolph-Miller/jonathan>`__
     - 2020-09
     - Fast but dormant. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `json-protocol <https://github.com/egao1980/json-protocol>`__ + ``json-backend-jzon``
     - 0.2.4
     - One ``encode`` / ``decode`` API over jzon (default) or yason. Also used by HTTP, logging, serdes.
   * - `serdes-protocol <https://github.com/egao1980/serdes-protocol>`__
     - 0.2.2
     - JSONL, pull events, one ``:format`` switch for JSON / YAML / CSV / XML.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: common-lisp

    (asdf:load-system "com.inuoe.jzon")
    (asdf:load-system "yason")

    (defpackage #:json-demo
      (:use #:cl)
      (:local-nicknames (#:jzon #:com.inuoe.jzon)))
    (in-package #:json-demo)

Decoding
^^^^^^^^

.. code:: python

    json.loads('["foo", {"bar": ["baz", null, 1.0, 2]}]')

jzon: objects are ``equal`` hash tables, arrays are vectors, ``null`` is the symbol ``null``, ``false`` is ``nil``.

.. code:: common-lisp

    (let ((v (jzon:parse "[\"foo\", {\"bar\": [\"baz\", null, 1.0, 2]}]")))
      (list (aref v 0) (gethash "bar" (aref v 1))))

::

    ("foo" #("baz" NULL 1.0d0 2))

yason can return alists:

.. code:: common-lisp

    (yason:parse "{\"a\": 1, \"b\": [true, null]}" :object-as :alist)

::

    (("a" . 1) ("b" T NIL))

Encoding
^^^^^^^^

.. code:: python

    json.dumps({"key": "value", "list": [1, 2, 3], "ok": True})

.. code:: common-lisp

    (jzon:stringify (alexandria:plist-hash-table
                     '("key" "value" "list" #(1 2 3) "ok" t)
                     :test 'equal))

::

    {"key":"value","list":[1,2,3],"ok":true}

.. code:: common-lisp

    (with-output-to-string (s)
      (yason:encode-alist '(("key" . "value") ("ok" . t)) s))

::

    {"key":"value","ok":true}

Pretty printing
^^^^^^^^^^^^^^^

.. code:: python

    print(json.dumps({"a": 1, "b": [1, 2]}, indent=2))

.. code:: common-lisp

    (jzon:stringify (jzon:parse "{\"a\": 1, \"b\": [1, 2]}") :pretty t)

::

    {
      "a": 1,
      "b": [
        1,
        2
      ]
    }

Errors
^^^^^^

.. code:: python

    try:
        json.loads("{")
    except json.JSONDecodeError as e:
        print(e)

.. code:: common-lisp

    (handler-case (jzon:parse "{")
      (jzon:json-parse-error (e) (princ-to-string e)))

::

    End of input when reading object, expecting key or object close, at line 1, column 1.

cl-stack
^^^^^^^^

Same calls over any backend. Load a backend system; it sets ``stack-json:*json-backend*``.

.. code:: common-lisp

    (asdf:load-system "json-backend-jzon")

.. code:: common-lisp

    (stack-json:encode '(("a" . 1) ("b" . :null) ("ok" . t)))

::

    {"a":1,"b":null,"ok":true}

.. code:: common-lisp

    (let ((v (stack-json:decode "{\"a\": false, \"xs\": [1, 2]}")))
      (list (gethash "a" v) (aref (gethash "xs" v) 1)))

::

    (NIL 2)

JSON Lines through serdes:

.. code:: common-lisp

    (let ((rows '()))
      (stack-serdes:map-jsonl (lambda (v) (push (gethash "n" v) rows))
                              (format nil "{\"n\": 1}~%{\"n\": 2}~%")
                              :format :json)
      (nreverse rows))

::

    (1 2)

.. code:: common-lisp

    (handler-case (stack-json:decode "{")
      (stack-json:json-parse-error (e) (stack-json:json-error-message e)))

::

    jzon parse failed: End of input when reading object, expecting key or object close, at line 1, column 1.
