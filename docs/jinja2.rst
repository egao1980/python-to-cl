Jinja
-----

https://jinja.palletsprojects.com/

Jinja is a fast, expressive, extensible templating engine.

Features
~~~~~~~~

- Template inheritance and inclusion

- Automatic HTML escaping

- Filters, tests, macros

- Sandboxed execution

- Templates compiled to Python code, cached

- i18n (Babel), async rendering

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Djula <https://github.com/mmontone/djula>`__
     - 2026-09
     - Django templates: inheritance, filters, autoescape, gettext i18n. Closest to Jinja. Not in cl-repo.
   * - `TEN <https://github.com/mmontone/ten>`__
     - 2024-07
     - Jinja-like syntax with Lisp expressions, compiled to functions. Not in cl-repo.
   * - `Spinneret <https://github.com/ruricolist/spinneret>`__
     - 2026-08
     - HTML5 from s-expressions, escapes by default, Markdown, Parenscript. Popular. Not in cl-repo.
   * - `CL-WHO <https://github.com/edicl/cl-who>`__
     - 2025-11
     - Classic HTML from s-expressions. No auto-escaping: call ``esc``. Not in cl-repo.
   * - `cl-mustache <https://github.com/kanru/cl-mustache>`__
     - 2024-09
     - Mustache templates. Not in cl-repo.
   * - `Clip <https://shinmera.com/projects/clip>`__
     - 2025-11
     - Attribute-driven HTML templates on Plump. Moved to Codeberg. Not in cl-repo.

cl-stack
^^^^^^^^

No template engine. ``CL:FORMAT`` covers text templates; ``html-protocol`` builds and serializes HTML trees but has no inheritance or filters.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://jinja.palletsprojects.com/en/stable/templates/

.. code:: python

    Template("Hello {{ name }}! {% for i in items %}{{ i|upper }}{% if not loop.last %}, {% endif %}{% endfor %}").render(
        name="World", items=["a", "b"])

CL:FORMAT
^^^^^^^^^

The standard has a small template language:

.. code:: common-lisp

    (format nil "Hello ~a! ~{~:@(~a~)~^, ~}" "World" '("a" "b"))

::

    Hello World! A, B

Djula
^^^^^

Not in cl-repo. From its documentation, not run.

``templates/base.html``:

.. code:: html

    <html><body>{% block content %}{% endblock %}</body></html>

``templates/hello.html``:

.. code:: html

    {% extends "base.html" %}
    {% block content %}
    <h1>Hello {{ name }}!</h1>
    <ul>{% for item in items %}<li>{{ item | upper }}</li>{% endfor %}</ul>
    {% endblock %}

.. code:: common-lisp

    (djula:add-template-directory #p"templates/")
    (defparameter +hello+ (djula:compile-template* "hello.html"))
    (djula:render-template* +hello+ nil :name "World" :items '("a" "<b>"))

::

    not run: Djula is not in cl-repo

Spinneret
^^^^^^^^^

Not in cl-repo, not run:

.. code:: common-lisp

    (spinneret:with-html-string
      (:h1 "Hello " name "!")
      (:ul (dolist (item items) (:li item))))

::

    not run: Spinneret is not in cl-repo

cl-mustache
^^^^^^^^^^^

Not in cl-repo, not run:

.. code:: common-lisp

    (mustache:render* "Hello {{name}}!" '((:name . "World")))

::

    not run: cl-mustache is not in cl-repo
