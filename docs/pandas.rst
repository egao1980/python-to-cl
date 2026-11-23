pandas / pyarrow
----------------

https://pandas.pydata.org/docs/

https://arrow.apache.org/docs/python/

pandas is a fast, powerful, flexible and easy to use open source data analysis and manipulation tool.

Features
~~~~~~~~

- ``DataFrame`` / ``Series`` with labelled, typed columns

- ``read_csv``, ``read_parquet``, ``read_sql``, ``to_csv`` ...

- Selection, filtering, ``groupby``, joins, pivots

- Missing data, time series

- pyarrow: Arrow columnar memory, IPC, Parquet

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Lisp-Stat <https://lisp-stat.dev/>`__ (`data-frame <https://github.com/Lisp-Stat/data-frame>`__)
     - 2026-06
     - Data frames, ``read-csv``, ``defdf``, column access, types heuristics, plotting through Vega-Lite. Active. Not in cl-repo.
   * - `vellum <https://github.com/sirherrbatka/vellum>`__
     - 2026-06
     - 1.6.1. Data frames with transformations, joins, aggregation. Not in cl-repo.
   * - `cl-duckdb <https://github.com/ak-coram/cl-duckdb>`__
     - 2026-11
     - DuckDB binding: SQL over CSV, Parquet, Excel. Fastest route for big files. Not in cl-repo.
   * - `cl-csv <https://github.com/AccelerationNet/cl-csv>`__
     - 2026-03
     - CSV read / write, ``:row-fn`` streaming. With `data-table <https://github.com/AccelerationNet/data-table>`__ (2023-07) for typed columns. Not in cl-repo.

The `Lisp Journey CSV and data frames post <https://lisp-journey.gitlab.io/blog/csv-parsing-in-common-lisp/>`__ (2026-11) recommends cl-csv for simple use, DuckDB for speed, Lisp-Stat for data frames.

cl-stack
^^^^^^^^

No data frame. Columnar and tabular I/O only:

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `csv-protocol <https://github.com/egao1980/csv-protocol>`__
     - 0.1.0
     - RFC 4180 CSV / TSV with dialects, header rows as hash tables, streaming and pull events.
   * - `arrow-protocol <https://github.com/egao1980/arrow-protocol>`__
     - 0.1.2
     - Pure Lisp Arrow IPC and Parquet (read and write, snappy / zstd / gzip). No compute kernels.
   * - `sql-query-csv <https://github.com/egao1980/sql-query-csv>`__
     - 0.1.0
     - A sql-query dialect that treats CSV files as tables.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    df = pd.read_csv("persons.csv")
    df["age"].tolist(), df[df.age > 30].name.tolist()

Lisp-Stat
^^^^^^^^^

Not in cl-repo. From the Lisp Journey post, not run:

.. code:: common-lisp

    (defdf persons (read-csv #p"persons.csv"))
    persons:age
    (head persons)
    (select persons '(0 1) 'age)

::

    not run: Lisp-Stat is not in cl-repo

cl-stack
^^^^^^^^

.. code:: common-lisp

    (asdf:load-system "csv-protocol")
    (asdf:load-system "arrow-protocol")

    (defparameter *csv* "name,age
    Alice,7
    Bob,77
    Carol,42
    ")

Rows with a header are hash tables:

.. code:: common-lisp

    (let ((rows (stack-csv:decode *csv*)))
      (list (map 'list (lambda (r) (parse-integer (gethash "age" r))) rows)
            (loop for r across rows
                  when (> (parse-integer (gethash "age" r)) 30)
                    collect (gethash "name" r))))

::

    ((7 77 42) ("Bob" "Carol"))

Write CSV with a dialect:

.. code:: common-lisp

    (stack-csv:encode '(#("name" "age") #("Alice" "7")) :header nil :dialect :excel-eu)

::

    name;age
    Alice;7

Parquet round trip:

.. code:: python

    pq.write_table(pa.Table.from_pylist(rows), "p.parquet")

.. code:: common-lisp

    (let* ((schema (stack-arrow:make-arrow-schema
                    (list (stack-arrow:make-arrow-field :name "name" :type :utf8)
                          (stack-arrow:make-arrow-field :name "age" :type :int64))))
           (rows (list (alexandria:alist-hash-table '(("name" . "Alice") ("age" . 7)) :test #'equal)
                       (alexandria:alist-hash-table '(("name" . "Bob") ("age" . 77)) :test #'equal)))
           (octets (stack-arrow:encode (stack-arrow:table-from-rows rows :schema schema)
                                       :format :parquet)))
      (list (subseq octets 0 4)
            (map 'list (lambda (r) (list (gethash "name" r) (gethash "age" r)))
                 (stack-arrow:table-to-rows (stack-arrow:decode octets :format :parquet)))))

::

    (#(80 65 82 49) (("Alice" 7) ("Bob" 77)))
