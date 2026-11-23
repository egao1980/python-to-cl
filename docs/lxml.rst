lxml / Beautiful Soup
---------------------

https://lxml.de/

https://www.crummy.com/software/BeautifulSoup/bs4/doc/

lxml is the most feature-rich and easy-to-use library for processing XML and HTML in the Python language.
Beautiful Soup is a Python library for pulling data out of HTML and XML files.

Features
~~~~~~~~

- Lenient HTML parsing of real-world markup

- Tree navigation and search: ``find_all``, ``select`` (CSS), attributes, text

- ElementTree API, XPath, XSLT

- XML Schema / Relax NG / DTD validation

- Incremental (event) parsing, serialization

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

The 2024 community survey (XML parser): Plump 14%, CXML 6%, xmls 4%, s-xml 4%; 70% did not need one.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Plump <https://shinmera.com/projects/plump>`__
     - 2026-04
     - 2.0.0, in cl-repo. Lenient HTML / XML parser and DOM. The Beautiful Soup role. Moved to Codeberg (2025-08).
   * - `CLSS <https://shinmera.com/projects/clss>`__
     - 2025-08
     - CSS selectors over Plump. Not in cl-repo.
   * - `lQuery <https://shinmera.com/projects/lquery>`__
     - 2025-08
     - jQuery-style queries and edits over Plump and CLSS. Not in cl-repo.
   * - `CXML <https://github.com/sharplispers/cxml>`__
     - 2025-04
     - Conforming, validating XML parser (DTD), SAX and pull (klacks) APIs; XPath via Plexippus, Relax NG via cxml-rng. Not in cl-repo.
   * - `xmls <https://github.com/rpgoldman/xmls>`__
     - 2026-10
     - 3.4.0. Small XML ↔ s-expression parser. Not in cl-repo.
   * - `cl-html5-parser <https://github.com/rotatef/cl-html5-parser>`__
     - 2019-03
     - HTML5 spec parsing algorithm. Archived. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `html-protocol <https://github.com/egao1980/html-protocol>`__ + ``html-backend-plump``
     - 0.1.2
     - Lenient HTML parse / serialize and a small CSS ``select`` (tag, ``#id``, ``.class``, descendant, ``>``). No XPath.
   * - `xml-protocol <https://github.com/egao1980/xml-protocol>`__ + ``xml-backend-native``
     - 0.1.0
     - Well-formed XML 1.0 + namespaces, XXE-safe, pull events, event writer, serdes ``:xml``. No XPath / XSLT.
   * - `schema-protocol-xsd <https://github.com/egao1980/schema-protocol-xsd>`__
     - 0.1.4
     - XSD 1.0 / 1.1 emit, parse, ``validate-instance``.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    soup = BeautifulSoup(html, "html.parser")
    [a["href"] for a in soup.find_all("a")]
    soup.find(id="title").get_text()

.. code:: common-lisp

    (asdf:load-system "plump")

    (defparameter *html* "<html><body><h1 id='title'>Hello <b>CL</b></h1>
    <ul class='links'><li><a href='/a'>A</a><li><a href='/b' class='ext'>B</a></ul>
    <p>Unclosed paragraph<br></body></html>")

Plump
^^^^^

.. code:: common-lisp

    (let ((root (plump:parse *html*)))
      (list (mapcar (lambda (a) (plump:attribute a "href"))
                    (plump:get-elements-by-tag-name root "a"))
            (plump:text (plump:get-element-by-id root "title"))))

::

    (("/a" "/b") "Hello CL")

Lenient input, normalized output:

.. code:: common-lisp

    (plump:serialize (plump:parse "<p>one<p>two <b>bold") nil)

::

    <p>one<p>two <b>bold</b></p></p>

Plump does not apply the HTML5 tree-construction rules: the second ``<p>`` nests inside the first.

CLSS and lQuery (not in cl-repo, from their docs, not run):

.. code:: common-lisp

    (clss:select "ul.links a.ext" (plump:parse *html*))
    (lquery:$ (initialize *html*) "a" (attr :href))

::

    not run: CLSS and lQuery are not in cl-repo

CXML (not in cl-repo, not run):

.. code:: common-lisp

    (cxml:parse "<a><b x='1'/></a>" (cxml-dom:make-dom-builder))

::

    not run: CXML is not in cl-repo

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/xml.md

.. code:: common-lisp

    (asdf:load-system "html-backend-plump")
    (asdf:load-system "xml-backend-native")

HTML with CSS selectors:

.. code:: python

    [a.get_text() for a in soup.select("ul.links a")]

.. code:: common-lisp

    (let ((doc (stack-html:parse *html*)))
      (mapcar #'stack-html:element-text (stack-html:select doc "ul.links a")))

::

    ("A" "B")

XML with namespaces:

.. code:: python

    root = etree.fromstring("<root xmlns='urn:x'><n a='1'>hi</n></root>")
    root[0].text, root[0].get("a")

.. code:: common-lisp

    (let* ((doc (stack-xml:decode "<root xmlns='urn:x'><n a='1'>hi</n></root>"))
           (n (stack-xml:xml-child (stack-xml:document-root-element doc) "n")))
      (list (stack-xml:xml-element-text n) (stack-xml:xml-attr n "a")))

::

    ("hi" "1")

.. code:: common-lisp

    (stack-xml:encode (stack-xml:make-xml-element "n" '(("a" . "1")) "hi")
                      :pretty nil :declaration nil)

::

    <n a="1">hi</n>

Pull events (lxml ``iterparse``):

.. code:: common-lisp

    (let ((p (stack-serdes:make-event-parser "<a><b>x</b></a>" :format :xml))
          events)
      (loop (multiple-value-bind (ev val) (stack-serdes:parse-next-event p)
              (declare (ignore val))
              (unless ev (return))
              (push ev events)))
      (nreverse events))

::

    (:START-DOCUMENT :START-ELEMENT :START-ELEMENT :CHARACTERS :END-ELEMENT :END-ELEMENT :END-DOCUMENT)

No external entities (XXE-safe):

.. code:: common-lisp

    (handler-case
        (stack-xml:decode "<!DOCTYPE x [<!ENTITY e SYSTEM 'file:///etc/passwd'>]><x>&e;</x>")
      (error (e) (type-of e)))

::

    XML-PROTOCOL:XML-ENTITY-ERROR
