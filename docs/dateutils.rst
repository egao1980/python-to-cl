1 Dateutils
-----------

`https://dateutil.readthedocs.io/en/stable/ <https://dateutil.readthedocs.io/en/stable/>`_

The dateutil module provides powerful extensions to the standard datetime module, available in Python.

1.1 Features
~~~~~~~~~~~~

- Computing of relative deltas (next month, next year, next monday, last week of month, etc);

- Computing of relative deltas between two given date and/or datetime objects;

- Computing of dates based on very flexible recurrence rules, using a superset of the iCalendar specification. Parsing of RFC strings is supported as well.

- Generic parsing of dates in almost any string format;

- Timezone (tzinfo) implementations for tzfile(5) format files (/etc/localtime, /usr/share/zoneinfo, etc), TZ environment string (in all known formats), iCalendar format files, given ranges (with help from relative deltas), local machine timezone, fixed offset timezone, UTC timezone, and Windows registry-based time zones.

- Internal up-to-date world timezone information based on Olson?s database.

- Computing of Easter Sunday dates for any given year, using Western, Orthodox or Julian algorithms.

1.2 CL alternatives
~~~~~~~~~~~~~~~~~~~

- `https://github.com/dlowe-net/local-time <https://github.com/dlowe-net/local-time>`_

- `https://github.com/enaeher/local-time-duration <https://github.com/enaeher/local-time-duration>`_

- `https://gist.github.com/perusio/6687883 <https://gist.github.com/perusio/6687883>`_

1.3 Python to CL examples
~~~~~~~~~~~~~~~~~~~~~~~~~

`https://dateutil.readthedocs.io/en/stable/examples.html <https://dateutil.readthedocs.io/en/stable/examples.html>`_

Similary to Python let's start with importing required libraries:

.. code-block:: common-lisp

   (ql:quickload :local-time)
   (ql:quickload :local-time-duration)

   (use-package :local-time)

Store some values:

.. code-block:: common-lisp

    (defparameter *now* (now))
    (defparameter *today* (today))

    (list *now* *today*)

.. table::

Next month

.. code-block:: common-lisp

    (timestamp+ *now* 1 :month)
    ;; or 
    (adjust-timestamp *now* (offset :month 1))

::

    @2018-02-03T13:55:03\.459745Z

Next month, plus one week

.. code-block:: common-lisp

    (adjust-timestamp *now* (offset :month 1) (offset :day 7))

::

    @2018-02-10T13:55:03\.459745Z

Next month, plus one week, at 10am.

.. code-block:: common-lisp

    (adjust-timestamp *now* (offset :month 1) (offset :day 7) (set :hour 10))

::

    @2018-02-10T10:55:03\.459745Z

Setting specific time fields similar to absolute relativedelta:

.. code-block:: common-lisp

    (adjust-timestamp *now* (set :year 1) (set :month 1))

::

    @0001-01-03T13:55:03\.459745+01:00

Get the relative delta

.. code-block:: common-lisp

    (ltd:timestamp-difference (encode-timestamp 0 0 0 0 1 1 2018) *now*)

::

    #<LOCAL-TIME-DURATION:DURATION [-2/-50103/-459745000]  -2 days -13 hours -55 minutes -3 seconds -459745000 nsecs>

One month before one year.

.. code-block:: common-lisp

    (adjust-timestamp *now* (offset :year 1) (offset :month -1))

::

    @2018-12-03T13:55:03\.459745Z

How does it handle months with different numbers of days? Notice that adding one month will never cross the month boundary.

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 27 1 2003) (offset :month 1))

::

    @2003-02-27T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 31 1 2003) (offset :month 1))

::

    @2003-02-28T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 31 1 2003) (offset :month 2))

::

    @2003-03-31T00:00:00\.000000+01:00

The logic for years is the same, even on leap years.

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 2000) (offset :year 1))

::

    @2001-02-28T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 29 2 2000) (offset :year 1))

::

    @2001-02-28T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 1999) (offset :year 1))

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 1 3 1999) (offset :year 1))

::

    @2000-03-01T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 28 2 2001) (offset :year -1))

::

    @2000-02-28T00:00:00\.000000Z

.. code-block:: common-lisp

    (adjust-timestamp (encode-timestamp 0 0 0 0 1 3 2001) (offset :year -1))

::

    @2000-03-01T00:00:00\.000000Z

Next Friday

.. code-block:: common-lisp

    (adjust-timestamp *today* (offset :day-of-week :friday))

::

    @2018-01-05T00:00:00\.000000Z

Last Friday of the month

.. code-block:: common-lisp

    (defun set-day-of-week (time day-of-week) 
      "Adjust the timestamp to be the specifed day of the week, selects corresponding preceeding date if timestamp's day of the week do not match the requirement."
      (let ((adjusted (adjust-timestamp time (offset :day-of-week day-of-week))))
        (if (timestamp>= time adjusted)
            adjusted
            (adjust-timestamp adjusted (offset :day -7)))))

    (set-day-of-week (timestamp-maximize-part *today* :day) :friday)

::

    @2018-01-26T23:59:59\.999999Z

Next Wednesday (it's today!)

.. code-block:: common-lisp

    (defun next-day-of-week (time day-of-week) 
      "Adjust the timestamp to be the next specifed day of the week, selects corresponding future date if timestamp's day of the week do not match the requirement."
      (let ((adjusted (adjust-timestamp time (offset :day-of-week day-of-week))))
        (if (timestamp>= adjusted time)
            adjusted
            (adjust-timestamp adjusted (offset :day 7)))))

    (let ((*today* (encode-timestamp 0 0 0 0 3 1 2018)))
      (next-day-of-week *today* :wednesday))

::

    @2018-01-03T00:00:00\.000000Z

Next wednesday, but not today.

.. code-block:: common-lisp

    (let ((*today* (encode-timestamp 0 0 0 0 3 1 2018)))
      (next-day-of-week (adjust-timestamp *today* (offset :day 1)) :wednesday))

::

    @2018-01-10T00:00:00\.000000Z

Following `ISO year week number notation <http://www.cl.cam.ac.uk/~mgk25/iso-time.html>`_ find the first day of the 15th week of 1997.

.. code-block:: common-lisp

    (set-day-of-week
     (adjust-timestamp
         (next-day-of-week
          (encode-timestamp 0 0 0 0 1 1 1997)
          :thursday)
       (offset :day (* 7 14)))
     :monday)

::

    @1997-04-07T00:00:00\.000000+01:00

How long ago has the millennium changed?

.. code-block:: common-lisp

    (ltd:timestamp-difference *now* (encode-timestamp 0 0 0 0 1 1 2001))

::

    #<LOCAL-TIME-DURATION:DURATION [6211/50103/459745000] 887 weeks 2 days 13 hours 55 minutes 3 seconds 459745000 nsecs>

It works with dates too.

.. code-block:: common-lisp

    (ltd:timestamp-difference *today* (encode-timestamp 0 0 0 0 1 1 2001))

::

    #<LOCAL-TIME-DURATION:DURATION [6211/0/0] 887 weeks 2 days>

Obtain a date using the yearday:

.. code-block:: common-lisp

    (adjust-timestamp (timestamp-minimize-part *now* :day) (offset :day 260))

::

    @2018-09-18T00:00:00\.000000+01:00

Leap year vs non-leap year:

.. code-block:: common-lisp

    (let ((leap (encode-timestamp 0 0 0 0 1 1 2000))
          (non-leap (encode-timestamp 0 0 0 0 1 1 2002)))

      (list (adjust-timestamp (timestamp-minimize-part leap :day) (offset :day 260))
            (adjust-timestamp (timestamp-minimize-part non-leap :day) (offset :day 260))))

.. table::
