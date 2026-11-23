PyYAML
------

https://pyyaml.org/wiki/PyYAMLDocumentation

PyYAML is a YAML parser and emitter for Python.

Features
~~~~~~~~

- Complete YAML 1.1 parser and emitter

- Unicode support

- ``safe_load`` / ``safe_dump`` for plain data; ``load`` for Python objects

- Anchors, aliases and merge keys

- Custom tags (constructors and representers)

- Optional LibYAML (C) speedups

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `cl-yaml <https://github.com/eudoxia0/cl-yaml>`__
     - 2022-10
     - libyaml binding; parse and emit. Archived on GitHub. Needs the C library. Not in cl-repo.
   * - `nyaml <https://github.com/jasom/nyaml>`__
     - 2026-05
     - Pure Lisp YAML 1.2 parser. Parse only. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `yaml-protocol <https://github.com/egao1980/yaml-protocol>`__
     - 0.2.1
     - Pure Lisp YAML 1.2 parse and emit. Passes the yaml-test-suite. Anchors keep identity, ``<<`` merges. Same Lisp mapping as ``json-protocol``. No custom tags.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://pyyaml.org/wiki/PyYAMLDocumentation

cl-yaml (not in cl-repo, from its README, not run):

.. code:: common-lisp

    (ql:quickload :cl-yaml)
    (yaml:parse "[1, 2, 3]")
    (yaml:emit-to-string (list 1 2 3))

::

    not run: cl-yaml is not in cl-repo

cl-stack
^^^^^^^^

.. code:: common-lisp

    (asdf:load-system "yaml-protocol")

Loading

.. code:: python

    yaml.safe_load("""
    name: Ada
    langs: [lisp, ml]
    active: yes
    """)

Mappings are ``equal`` hash tables, sequences are vectors. YAML 1.2: ``yes`` is a string, not a boolean.

.. code:: common-lisp

    (let ((v (yaml-protocol:decode "name: Ada
    langs: [lisp, ml]
    active: yes
    ")))
      (list (gethash "name" v) (gethash "langs" v) (gethash "active" v)))

::

    ("Ada" #("lisp" "ml") "yes")

Several documents:

.. code:: python

    list(yaml.safe_load_all("a: 1\n---\na: 2\n"))

.. code:: common-lisp

    (map 'list (lambda (d) (gethash "a" d))
         (yaml-protocol:decode-all "a: 1
    ---
    a: 2
    "))

::

    (1 2)

Anchors and merge keys

.. code:: common-lisp

    (let* ((v (yaml-protocol:decode "base: &b {host: db, port: 5432}
    dev:
      <<: *b
      port: 5433
    ")))
      (list (gethash "host" (gethash "dev" v))
            (gethash "port" (gethash "dev" v))))

::

    ("db" 5433)

Aliases are the same object:

.. code:: common-lisp

    (let ((v (yaml-protocol:decode "a: &x [1, 2]
    b: *x
    ")))
      (eq (gethash "a" v) (gethash "b" v)))

::

    T

Dumping

.. code:: python

    print(yaml.safe_dump({"name": "Ada", "langs": ["lisp", "ml"]}))

.. code:: common-lisp

    (yaml-protocol:encode (yaml-protocol:decode "{name: Ada, langs: [lisp, ml]}"))

::

    langs: 
      - lisp
      - ml
    name: Ada

Errors

.. code:: common-lisp

    (handler-case (yaml-protocol:decode "a: [1, 2")
      (yaml-protocol:yaml-parse-error (e) (princ-to-string e)))

::

    YAML error: expected , or ] in flow sequence (pos 8)
