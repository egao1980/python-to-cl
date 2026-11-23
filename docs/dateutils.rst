Dateutil
--------

`https://dateutil.readthedocs.io/en/stable/ <https://dateutil.readthedocs.io/en/stable/>`__

 The dateutil module provides powerful extensions to the standard datetime module, available in Python.

Features
~~~~~~~~

- Computing of relative deltas (next month, next year, next monday, last week of month, etc);

- Computing of relative deltas between two given date and/or datetime objects;

- Computing of dates based on very flexible recurrence rules, using a superset of the iCalendar specification. Parsing of RFC strings is supported as well.

- Generic parsing of dates in almost any string format;

- Timezone (tzinfo) implementations for tzfile(5) format files (/etc/localtime, /usr/share/zoneinfo, etc), TZ environment string (in all known formats), iCalendar format files, given ranges (with help from relative deltas), local machine timezone, fixed offset timezone, UTC timezone, and Windows registry-based time zones.

- Internal up-to-date world timezone information based on Olson's database.

- Computing of Easter Sunday dates for any given year, using Western, Orthodox or Julian algorithms.

CL alternatives
~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `local-time <https://github.com/sharplispers/local-time>`__
     - 2026-05
     - v1.0.6, in cl-repo. The de facto standard. Timestamps, tzfile zones, RFC 3339, ``adjust-timestamp``. No recurrence, no fuzzy parsing, no durations.
   * - `local-time-duration <https://github.com/enaeher/local-time-duration>`__
     - 2018-04
     - Durations on top of local-time. Dormant. Not in cl-repo.
   * - `calendar-times <https://github.com/copyleft/calendar-times>`__
     - 2025-11
     - Calendar dates, times and zoned datetimes on local-time. Not in cl-repo.
   * - `periods <https://github.com/jwiegley/periods>`__
     - 2025-08
     - Time ranges and recurring periods. Not in cl-repo.
   * - `chronicity <https://github.com/chaitanyagupta/chronicity>`__
     - 2019-01
     - Natural language parsing ("next monday"). Dormant. Not in cl-repo.
   * - `cl-date-time-parser <https://github.com/tkych/cl-date-time-parser>`__
     - 2014-05
     - Parses RFC 822, ISO 8601, W3CDTF strings. Dormant. Not in cl-repo.
   * - `https://gist.github.com/perusio/6687883 <https://gist.github.com/perusio/6687883>`__
     - 2013
     - Easter dates, a gist.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `datetime-protocol <https://github.com/egao1980/datetime-protocol>`__
     - 0.1.3
     - Instants, durations, periods, dates, zones, RFC 3339 / HTTP-date, RFC 5545 recurrence, natural-language ranges, Easter. Not local-time based.
   * - `cl-stack-tzdata <https://github.com/egao1980/cl-stack-tzdata>`__
     - 2026.6.0.1
     - IANA tzdb bundled. No ``/usr/share/zoneinfo`` needed (Windows works).
   * - `cl-stack-calendars <https://github.com/egao1980/cl-stack-calendars>`__
     - 0.4.1
     - Holidays, business days, exchange sessions.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

`https://dateutil.readthedocs.io/en/stable/examples.html <https://dateutil.readthedocs.io/en/stable/examples.html>`__

Similary to Python let's start with importing required libraries:

.. code:: common-lisp

    (asdf:load-system "local-time")

    (use-package :local-time)
    ;; print in UTC, whatever the machine's zone
    (setf *default-timezone* +utc-zone+)

Store some values:

.. code:: common-lisp

    ;; use this moment for demo
    (defparameter *now* (now))
    (defparameter *today* (today))

    (list *now* *today*)

::

    (@2026-11-23T15:54:44.256660Z @2026-11-23T00:00:00.000000Z)

Date and time manipulation
^^^^^^^^^^^^^^^^^^^^^^^^^^

Next month

.. code:: common-lisp

    (timestamp+ *now* 1 :month)
    ;; or
    (adjust-timestamp *now* (offset :month 1))

::

    @2026-12-23T15:54:44.256660Z

Next month, plus one week

.. code:: common-lisp

    (adjust-timestamp *now* (offset :month 1) (offset :day 7))

::

    @2026-12-30T15:54:44.256660Z

Next month, plus one week, at 10am.

.. code:: common-lisp

    (adjust-timestamp *now* (offset :month 1) (offset :day 7) (set :hour 10))

::

    @2026-12-30T10:54:44.256660Z

Setting specific time fields similar to absolute relativedelta:

.. code:: common-lisp

    (adjust-timestamp *now* (set :year 1) (set :month 1))

::

    @0001-01-23T15:54:44.256660Z

Get the relative delta

local-time-duration is not in cl-repo. From its documentation, not run:

.. code:: common-lisp

    (ltd:timestamp-difference (encode-timestamp 0 0 0 0 1 1 2018) *now*)

::

    not run: local-time-duration is not in cl-repo

local-time alone gives the difference in seconds:

.. code:: common-lisp

    (timestamp-difference *now* (encode-timestamp 0 0 0 0 1 1 2018))

::

    2.8068448425666d8

One month before one year and other offsets.

.. code:: common-lisp

    (adjust-timestamp *now* (offset :year 1) (offset :month -1))

::

    @2027-10-23T15:54:44.256660Z

How does it handle months with different numbers of days? Notice that adding one month will never cross the month boundary.

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 27 1 2003) (offset :month 1))

::

    @2003-02-27T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 31 1 2003) (offset :month 1))

::

    @2003-02-28T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 31 1 2003) (offset :month 2))

::

    @2003-03-31T00:00:00.000000Z

The logic for years is the same, even on leap years.

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 2000) (offset :year 1))

::

    @2001-02-28T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 29 2 2000) (offset :year 1))

::

    @2001-02-28T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 1999) (offset :year 1))

::

    @2000-02-28T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 1 3 1999) (offset :year 1))

::

    @2000-03-01T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 2001) (offset :year -1))

::

    @2000-02-28T00:00:00.000000Z

.. code:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 1 3 2001) (offset :year -1))

::

    @2000-03-01T00:00:00.000000Z

Next Friday

.. code:: common-lisp

    (adjust-timestamp *today* (offset :day-of-week :friday))

::

    @2026-11-27T00:00:00.000000Z

Last Friday of the month

.. code:: common-lisp

    (defun set-day-of-week (time day-of-week)
      "Adjust the timestamp to be the specifed day of the week, selects corresponding preceeding date if timestamp's day of the week do not match the requirement."
      (let ((adjusted (adjust-timestamp time (offset :day-of-week day-of-week))))
        (if (timestamp>= time adjusted)
            adjusted
            (adjust-timestamp adjusted (offset :day -7)))))

    (set-day-of-week (timestamp-maximize-part *today* :day) :friday)

::

    @2026-11-27T23:59:59.999999Z

Next Wednesday (it's today!)

.. code:: common-lisp

    (defun next-day-of-week (time day-of-week)
      "Adjust the timestamp to be the next specifed day of the week, selects corresponding future date if timestamp's day of the week do not match the requirement."
      (let ((adjusted (adjust-timestamp time (offset :day-of-week day-of-week))))
        (if (timestamp>= adjusted time)
            adjusted
            (adjust-timestamp adjusted (offset :day 7)))))

    (let ((*today* (encode-timestamp 0 0 0 0 3 1 2018)))
      (next-day-of-week *today* :wednesday))

::

    @2018-01-03T00:00:00.000000Z

Next wednesday, but not today.

.. code:: common-lisp

    (let ((*today* (encode-timestamp 0 0 0 0 3 1 2018)))
      (next-day-of-week (adjust-timestamp *today* (offset :day 1)) :wednesday))

::

    @2018-01-10T00:00:00.000000Z

Following `ISO year week number notation <http://www.cl.cam.ac.uk/~mgk25/iso-time.html>`__ find the first day of the 15th week of 1997.

.. code:: common-lisp

    (set-day-of-week
     (adjust-timestamp
         (next-day-of-week
          (encode-timestamp 0 0 0 0 1 1 1997)
          :thursday)
       (offset :day (* 7 14)))
     :monday)

::

    @1997-04-07T00:00:00.000000Z

How long ago has the millennium changed?

.. code:: common-lisp

    (floor (timestamp-difference *now* (encode-timestamp 0 0 0 0 1 1 2001))
           (* 24 60 60))

::

    9457

Obtain a date using the yearday:

.. code:: common-lisp

    (adjust-timestamp (timestamp-minimize-part *now* :month) (offset :day 259))

::

    @2026-09-17T00:00:00.000000Z

Leap year vs non-leap year:

.. code:: common-lisp

    (let ((leap (encode-timestamp 0 0 0 0 1 1 2000))
          (non-leap (encode-timestamp 0 0 0 0 1 1 2002)))

      (list (adjust-timestamp leap (offset :day 259))
            (adjust-timestamp non-leap (offset :day 259))))

::

    (@2000-09-16T00:00:00.000000Z @2002-09-17T00:00:00.000000Z)

Date parsing
^^^^^^^^^^^^

Simple examples

local-time parses RFC 3339 / ISO 8601:

.. code:: common-lisp

    (parse-timestring "2003-09-25T10:49:41-03:00")

::

    @2003-09-25T13:49:41.000000Z

It does not parse free-form strings:

.. code:: common-lisp

    (parse-timestring "Thu Sep 25 10:36:28 2003" :fail-on-error nil)

::

    NIL

For those use chronicity or cl-date-time-parser (not in cl-repo, not run):

.. code:: common-lisp

    (chronicity:parse "Thu Sep 25 10:36:28 2003")
    (chronicity:parse "next monday")
    (cl-date-time-parser:parse-date-time "Thu, 25 Sep 2003 10:49:41 -0300")

::

    not run: chronicity and cl-date-time-parser are not in cl-repo

Time zones
^^^^^^^^^^

local-time reads tzfile(5) data:

.. code:: common-lisp

    (reread-timezone-repository)
    (format-timestring nil (encode-timestamp 0 0 0 12 1 7 2026 :timezone +utc-zone+)
                       :timezone (find-timezone-by-location-name "America/New_York"))

::

    2026-07-01T08:00:00.000000-04:00

cl-stack
^^^^^^^^

`https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/datetime.md <https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/datetime.md>`__

``datetime-protocol`` shadows ``+``, ``-``, ``<``; use it through a local nickname.

.. code:: common-lisp

    (asdf:load-system "datetime-protocol")
    (asdf:load-system "datetime-protocol/calendars")
    (asdf:load-system "cl-stack-tzdata")

    (defpackage #:dateutil-demo
      (:use #:cl)
      (:local-nicknames (#:dt #:stack-datetime)))
    (in-package #:dateutil-demo)

Month arithmetic. Overflow is an error unless you choose ``:clamp`` (dateutil's behaviour) or ``:carry``:

.. code:: python

    date(2003, 1, 31) + relativedelta(months=+1)

.. code:: common-lisp

    (list (dt:date-add (dt:make-date 2003 1 31) :months 1 :overflow :clamp)
          (dt:date-add (dt:make-date 2003 1 31) :months 1 :overflow :carry)
          (handler-case (dt:date-add (dt:make-date 2003 1 31) :months 1)
            (dt:datetime-arithmetic-error () :error)))

::

    (#<DT:DATE 2003-02-28> #<DT:DATE 2003-03-03> :ERROR)

Difference between dates:

.. code:: common-lisp

    (dt:- (dt:make-date 2026 11 23) (dt:make-date 2001 1 1))

::

    9457

Recurrence rules:

.. code:: python

    list(rrule(WEEKLY, interval=2, byweekday=(MO, WE), count=4, dtstart=datetime(2024, 1, 1)))

.. code:: common-lisp

    (dt:occurrences (dt:weekly :on '(:monday :wednesday) :every 2
                               :from (dt:make-date 2024 1 1) :count 4))

::

    (#<DT:DATE 2024-01-01> #<DT:DATE 2024-01-03> #<DT:DATE 2024-01-15> #<DT:DATE 2024-01-17>)

.. code:: common-lisp

    (dt:occurrences (dt:parse-rrule "FREQ=MONTHLY;BYDAY=-1FR;COUNT=3"
                                    :from (dt:make-date 2026 1 1)))

::

    (#<DT:DATE 2026-01-30> #<DT:DATE 2026-02-27> #<DT:DATE 2026-03-27>)

Parsing:

.. code:: common-lisp

    (list (dt:print-rfc3339 (dt:parse-rfc3339 "2003-09-25T10:49:41-03:00"))
          (dt:print-http-date (dt:parse-http-date "Thu, 25 Sep 2003 13:49:41 GMT")))

::

    ("2003-09-25T10:49:41-03:00" "Thu, 25 Sep 2003 13:49:41 GMT")

Natural-language ranges (English, past-oriented: "yesterday", "last week", "last friday"), relative to a fixed "now":

.. code:: common-lisp

    (let* ((now (dt:zoned-moment-to-instant (dt:parse-rfc3339 "2026-11-23T12:00:00Z")))
           (range (dt:parse-time-range "last friday" :now now)))
      (mapcar (lambda (i) (dt:print-rfc3339 (dt:instant-in-zone i dt:+utc+)))
              (list (dt:interval-start range) (dt:interval-end range))))

::

    ("2026-11-20T00:00:00Z" "2026-11-21T00:00:00Z")

Time zones from the bundled IANA database; DST gaps are explicit:

.. code:: common-lisp

    (let ((ny (dt:resolve-zone-id "America/New_York")))
      (list (dt:print-rfc3339 (dt:moment-in-zone (dt:parse-moment "2026-07-01T12:00:00") ny))
            (handler-case (dt:moment-in-zone (dt:parse-moment "2026-03-08T02:30:00") ny
                                             :on-gap :strict)
              (dt:nonexistent-local-time () :nonexistent))))

::

    ("2026-07-01T12:00:00-04:00" :NONEXISTENT)

Easter:

.. code:: common-lisp

    (list (dt:easter-western 2026) (dt:easter-orthodox 2026))

::

    (#<DT:DATE 2026-04-05> #<DT:DATE 2026-04-12>)
