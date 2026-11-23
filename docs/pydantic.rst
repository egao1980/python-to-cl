Pydantic
--------

https://docs.pydantic.dev/latest/

Pydantic is the most widely used data validation library for Python.

Features
~~~~~~~~

- Models from type hints: ``class User(BaseModel)``

- Validation and coercion on construction, ``model_validate`` / ``model_validate_json``

- ``ValidationError`` listing every failing field

- Serialization: ``model_dump`` / ``model_dump_json``, aliases, camelCase

- JSON Schema generation

- Enums, unions, discriminated (tagged) unions, nested models

- Custom field and model validators, computed fields

- pydantic-settings for configuration

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `json-mop <https://github.com/gschjetne/json-mop>`__
     - 2026-09
     - CLOS metaclass: JSON keys and types per slot, on jzon. Checks types, no constraints. Not in cl-repo.
   * - `clavier <https://github.com/mmontone/clavier>`__
     - 2026-11
     - Validator objects and combinators: and, or, ``len``, ``valid-email``. No models. Not in cl-repo.
   * - `ratify <https://shinmera.com/projects/ratify>`__
     - 2025-11
     - Parse and validate strings (URLs, emails, dates). Moved to Codeberg. Not in cl-repo.
   * - `json-schema <https://github.com/fisxoj/json-schema>`__
     - 2022-10
     - JSON Schema validator (drafts 4–2019-09). Dormant. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `schema-protocol <https://github.com/egao1980/schema-protocol>`__
     - 0.2.2
     - ``defschema``: CLOS models with types, constraints, defaults, enums, tagged unions, computed fields, ``:extra :forbid``, per-field restarts. ``parse`` / ``dump``.
   * - `schema-protocol-json <https://github.com/egao1980/schema-protocol-json>`__
     - 0.1.1
     - JSON Schema (draft-07) emit and compile. Also ``-xsd``, ``-arrow``, ``-avro``.
   * - `cl-stack-config <https://github.com/egao1980/cl-stack-config>`__
     - 0.2.0
     - Settings from env, TOML, INI (pydantic-settings role).

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://docs.pydantic.dev/latest/concepts/models/

json-mop (from its README, not in cl-repo, not run):

.. code:: common-lisp

    (defclass book ()
      ((title :initarg :title :json-type :string :json-key "title")
       (year :initarg :year :json-type :number :json-key "year_published"))
      (:metaclass json-mop:json-serializable-class))

    (json-mop:json-to-clos "{\"title\":\"Vision\",\"year_published\":2015}" 'book)

::

    not run: json-mop is not in cl-repo

clavier (from its README, not run):

.. code:: common-lisp

    (funcall (clavier:&& (clavier:is-a-string) (clavier:len :min 10)) "short")

::

    not run: clavier is not in cl-repo

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/schema.md

.. code:: common-lisp

    (asdf:load-system "schema-protocol")
    (asdf:load-system "schema-protocol-json")
    (asdf:load-system "json-backend-jzon")

    (defpackage #:pydantic-demo
      (:use #:cl)
      (:local-nicknames (#:s #:stack-schema)))
    (in-package #:pydantic-demo)

Models

.. code:: python

    class Address(BaseModel):
        city: str
        country: str = "GB"

    class User(BaseModel):
        model_config = ConfigDict(extra="forbid")
        name: str = Field(min_length=1)
        age: int | None = Field(default=None, ge=0)
        address: Address
        tags: list[str]

.. code:: common-lisp

    (s:defschema address ()
      (city string)
      (country string :default "GB"))

    (s:defschema user ()
      (name string :min-length 1)
      (age (integer 0) :optional t)
      (address address)
      (tags (vector string))
      (:extra :forbid))

Validate and dump

.. code:: python

    User.model_validate({"name": "Ada", "address": {"city": "London"}, "tags": ["lisp"]}).model_dump()

.. code:: common-lisp

    (let ((u (s:parse 'user '(:name "Ada" :address (:city "London") :tags #("lisp")))))
      (list (country (address u)) (s:dump u)))

::

    ("GB" #<HASH-TABLE :TEST EQUAL :COUNT 3 {700A438573}>)

From JSON text:

.. code:: common-lisp

    (name (s:parse 'user "{\"name\": \"Grace\", \"address\": {\"city\": \"NYC\"}, \"tags\": []}"
                   :format :json))

::

    Grace

Validation errors

.. code:: python

    User.model_validate({"name": "", "age": -1, "address": {}, "tags": [], "x": 1})
    # ValidationError: 4 validation errors for User ...

.. code:: common-lisp

    (handler-case (s:parse 'user '(:name "" :age -1 :address () :tags #() :x 1))
      (s:schema-validation-error (e) (princ-to-string e)))

::

    Schema validation failed
      name: string length not in [1,NIL]
      age: expected (INTEGER 0)
      address.city: required
      x: unexpected field

JSON Schema

.. code:: python

    Address.model_json_schema()

.. code:: common-lisp

    (com.inuoe.jzon:stringify (s:emit-schema 'address :format :json) :pretty t)

::

    {
      "type": "object",
      "properties": {
        "city": {
          "type": "string"
        },
        "country": {
          "type": "string"
        }
      },
      "required": [
        "city"
      ],
      "additionalProperties": false,
      "title": "address",
      "$schema": "http://json-schema.org/draft-07/schema#"
    }

Enums and tagged unions

.. code:: common-lisp

    (s:defenum color (:red 1) (:blue 2))

    (s:defschema shape ()
      (kind color)
      (:tag kind))

    (s:defschema circle (shape)
      (kind (eql :red) :default :red)
      (r number))

.. code:: common-lisp

    (let ((c (s:parse 'shape '(:kind "RED" :r 1.5))))
      (list (type-of c) (r c)))

::

    (CIRCLE 1.5)
