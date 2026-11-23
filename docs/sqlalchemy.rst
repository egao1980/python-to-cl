SQLAlchemy / DB drivers
-----------------------

https://docs.sqlalchemy.org/

SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that gives application developers the full power and flexibility of SQL.

Features
~~~~~~~~

- Engine, connection pool, transactions over DB-API drivers (sqlite3, psycopg, mysqlclient)

- Core: SQL expression language, DDL, dialects

- ORM: declarative models, sessions, relationships, unit of work

- Reflection of existing schemas

- Alembic: migrations generated from model diffs

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `CL-DBI <https://github.com/fukamachi/cl-dbi>`__
     - 2026-02
     - 0.11.1 in cl-repo. DB-API: SQLite3, PostgreSQL, MySQL drivers, prepared statements, transactions, ``connect-cached``.
   * - `SxQL <https://github.com/fukamachi/sxql>`__
     - 2025-11
     - SQL builder as s-expressions. In cl-repo.
   * - `Mito <https://github.com/fukamachi/mito>`__
     - 2026-04
     - ORM on CL-DBI + SxQL: ``deftable``, relations, migrations, inflation. The usual pick. In cl-repo.
   * - `Postmodern <https://github.com/marijnh/Postmodern>`__
     - 2025-07
     - v1.33.12, in cl-repo. PostgreSQL only: fast protocol client, S-SQL, DAOs, pooling, LISTEN/NOTIFY.
   * - `cl-sqlite <https://github.com/TeMPOraL/cl-sqlite>`__
     - 2019-08
     - SQLite binding (system ``sqlite``). In cl-repo.
   * - `CLSQL <https://github.com/sharplispers/clsql>`__
     - 2025-06
     - Old multi-database toolkit with CommonSQL syntax. Heavy. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `sql-protocol <https://github.com/egao1980/sql-protocol>`__
     - 0.1.0
     - Connections, transactions, pools over SQLite3 and PostgreSQL backends (Engine role).
   * - `sql-query <https://github.com/egao1980/sql-query>`__
     - 0.2.0
     - CLOS SQL AST (Core role): DML, DDL, fragments; dialects for SQLite3, PostgreSQL, CSV files.
   * - `sql-orm <https://github.com/egao1980/sql-orm>`__
     - 0.3.0
     - ``defmodel``, ``persist``, ``select-instances``, relations, computed fields, schema snapshots and diffs, reflection. No identity map.
   * - `sql-migrate <https://github.com/egao1980/sql-migrate>`__
     - 0.2.1
     - Alembic-style revisions over sql-orm schema ops; linear history.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://docs.sqlalchemy.org/en/20/tutorial/

.. code:: common-lisp

    (asdf:load-system "dbd-sqlite3")
    (asdf:load-system "sxql")
    (asdf:load-system "mito")

CL-DBI
^^^^^^

.. code:: python

    with engine.begin() as conn:
        conn.execute(text("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"))
        conn.execute(text("INSERT INTO users (name) VALUES (:n)"), [{"n": "ada"}, {"n": "grace"}])
        conn.execute(text("SELECT * FROM users WHERE name = :n"), {"n": "ada"}).all()

.. code:: common-lisp

    (dbi:with-connection (conn :sqlite3 :database-name ":memory:")
      (dbi:do-sql conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
      (dbi:with-transaction conn
        (dolist (n '("ada" "grace"))
          (dbi:do-sql conn "INSERT INTO users (name) VALUES (?)" (list n))))
      (dbi:fetch-all (dbi:execute (dbi:prepare conn "SELECT * FROM users WHERE name = ?")
                                  (list "ada"))))

::

    ((:|id| 1 :|name| "ada"))

SxQL
^^^^

.. code:: common-lisp

    (multiple-value-list
     (sxql:yield
      (sxql:select (:id :name)
        (sxql:from :users)
        (sxql:where (:and (:= :active 1) (:like :name "a%")))
        (sxql:order-by :name)
        (sxql:limit 10))))

::

    ("SELECT id, name FROM users WHERE ((active = ?) AND (name LIKE ?)) ORDER BY name LIMIT 10"
     (1 "a%"))

Mito
^^^^

.. code:: python

    class User(Base):
        __tablename__ = "users"
        id: Mapped[int] = mapped_column(primary_key=True)
        name: Mapped[str] = mapped_column(String(64))

.. code:: common-lisp

    (mito:deftable user ()
      ((name :col-type (:varchar 64))))

.. code:: common-lisp

    (mito:connect-toplevel :sqlite3 :database-name ":memory:")
    (mito:ensure-table-exists 'user)
    (mito:create-dao 'user :name "ada")
    (mito:create-dao 'user :name "grace")
    (list (mito:count-dao 'user)
          (slot-value (mito:find-dao 'user :name "grace") 'name)
          (mapcar (lambda (u) (slot-value u 'name))
                  (mito:select-dao 'user (sxql:where (:like :name "a%")))))

::

    (2 "grace" ("ada"))

.. code:: common-lisp

    (mito:disconnect-toplevel)

Postmodern
^^^^^^^^^^

Needs a PostgreSQL server. From its documentation, not run:

.. code:: common-lisp

    (postmodern:with-connection '("app" "app" "secret" "localhost")
      (postmodern:query (:select 'id 'name :from 'users :where (:= 'name "ada"))))

::

    not run: needs a PostgreSQL server

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/sql.md

.. code:: common-lisp

    (asdf:load-system "sql-backend-sqlite3")
    (asdf:load-system "sql-query-sqlite3")
    (asdf:load-system "sql-orm")

    (defpackage #:sqlalchemy-demo
      (:use #:cl)
      (:local-nicknames (#:sql #:stack-sql)
                        (#:q #:sql-query)
                        (#:orm #:stack-sql-orm)))
    (in-package #:sqlalchemy-demo)

Connections and raw SQL:

.. code:: common-lisp

    (sql:with-connection (c :driver :sqlite3 :database-name ":memory:")
      (sql:execute c "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
      (sql:with-transaction (c)
        (sql:execute c "INSERT INTO t (name) VALUES (?)" '("ada")))
      (sql:fetch-all (sql:execute c "SELECT * FROM t")))

::

    ((:ID 1 :NAME "ada"))

Core-style query objects. Bare values compile to SQL literals; ``bindparam`` gives placeholders:

.. code:: common-lisp

    (multiple-value-list
     (q:compile-sql
      (q:select (q:columns :id (q:label (q:count :*) :n))
                (q:from :users)
                (q:where (q:sql-and (:= :active 1) (:= :name (q:bindparam :name "ada"))))
                (q:order-by :name)
                (q:limit 10))
      :dialect (sql-query-sqlite3:make-sqlite3-dialect)))

::

    ("SELECT \"id\", count(*) AS \"n\" FROM \"users\" WHERE ((\"active\" = 1) AND (\"name\" = ?)) ORDER BY \"name\" LIMIT 10"
     ("ada"))

ORM:

.. code:: common-lisp

    (orm:defmodel person ()
      (id :integer :primary-key t :autoincrement t)
      (name :text :not-null t)
      (email :text)
      (:table people)
      (:compute label (self)
        (format nil "~A <~A>" (name self) (email self))))

.. code:: common-lisp

    (orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (orm:ensure-schema c 'person)
      (orm:persist (make-instance 'person :name "ada" :email "ada@example.org"))
      (orm:persist (make-instance 'person :name "grace" :email "grace@example.org"))
      (mapcar #'label (orm:select-instances 'person :where (:= :name "ada"))))

::

    ("ada <ada@example.org>")
