Click / argparse
----------------

https://click.palletsprojects.com/

https://docs.python.org/3/library/argparse.html

Click is a Python package for creating beautiful command line interfaces in a composable way with as little code as necessary.

Features
~~~~~~~~

- Arbitrary nesting of commands (groups)

- Automatic help page generation

- Typed options and arguments, defaults, prompts, environment variables

- Lazy loading of subcommands at runtime

- Shell completion

- argparse: the same in the standard library, flat or with subparsers

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

The 2024 community survey: clingon 9%, unix-opts 7%, adopt 2%, clon 2% of respondents.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `clingon <https://github.com/dnaeon/clingon>`__
     - 2026-11
     - v0.7.0, in cl-repo. Subcommands, typed options, env vars, help, Bash and Zsh completion. The usual pick.
   * - `adopt <https://github.com/sjl/adopt>`__
     - 2026-03
     - v1.3.0, in cl-repo. Flat option parser, help and man page output. No subcommands.
   * - `command-line-args <https://git.sr.ht/~whereiseveryone/command-line-args>`__
     - 2025-01
     - In cl-repo. Turns a ``defun`` into a command; subcommands. AGPL.
   * - `defmain <https://github.com/40ants/defmain>`__
     - 2026-08
     - ``defmain`` macro over net.didierverna.clon. Not in cl-repo.
   * - `clon <https://github.com/didierverna/clon>`__
     - 2025-06
     - Full featured, heavy. Not in cl-repo.
   * - `unix-opts <https://github.com/libre-man/unix-opts>`__
     - 2021-01
     - Minimal getopt. Dormant. Not in cl-repo.
   * - `UIOP <https://asdf.common-lisp.dev/uiop.html>`__
     - 
     - ``uiop:command-line-arguments``, raw argv.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `cli-protocol <https://github.com/egao1980/cli-protocol>`__
     - 0.2.1
     - One command tree over clingon (default) or adopt. Accepts POSIX, PowerShell (``-Count 2``) and CMD (``/Count:2``) argv. ``main`` maps errors to exit codes.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

https://click.palletsprojects.com/en/stable/quickstart/

.. code:: python

    @click.command()
    @click.option("--count", default=1, help="Number of greetings.")
    @click.argument("name")
    def hello(count, name):
        for _ in range(count):
            click.echo(f"Hello, {name}!")

.. code:: common-lisp

    (asdf:load-system "clingon")
    (asdf:load-system "adopt")

clingon
^^^^^^^

.. code:: common-lisp

    (defun hello/handler (cmd)
      (dotimes (_ (clingon:getopt cmd :count))
        (format t "Hello, ~{~a~}!~%" (clingon:command-arguments cmd))))

    (defun hello/command ()
      (clingon:make-command
       :name "hello"
       :description "Greet NAME"
       :usage "[options] NAME"
       :options (list (clingon:make-option :integer
                                           :description "Number of greetings."
                                           :long-name "count"
                                           :short-name #\c
                                           :initial-value 1
                                           :key :count))
       :handler #'hello/handler))

``clingon:run`` parses ``(uiop:command-line-arguments)``, calls the handler and exits the process (not under SLIME / Sly). In a REPL parse and call the handler:

.. code:: common-lisp

    (let ((cmd (clingon:parse-command-line (hello/command) '("--count" "2" "Ada"))))
      (funcall (clingon:command-handler cmd) cmd))

::

    Hello, Ada!
    Hello, Ada!

Help is generated:

.. code:: common-lisp

    (clingon:print-usage (hello/command) t)

::

    NAME:
      hello - Greet NAME

    USAGE:
      hello [options] NAME

    OPTIONS:
          --help         display usage information and exit
          --version      display version and exit
      -c, --count <INT>  Number of greetings. [default: 1]

adopt
^^^^^

.. code:: common-lisp

    (defparameter *hello-ui*
      (adopt:make-interface
       :name "hello"
       :summary "Greet NAME"
       :usage "[options] NAME"
       :help "Say hello."
       :contents (list (adopt:make-option 'count
                                          :long "count" :short #\c
                                          :parameter "N"
                                          :help "Number of greetings."
                                          :initial-value 1
                                          :key #'parse-integer
                                          :reduce #'adopt:last))))

.. code:: common-lisp

    (multiple-value-bind (arguments options)
        (adopt:parse-options *hello-ui* '("-c" "3" "Ada"))
      (list arguments (gethash 'count options)))

::

    (("Ada") 3)

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/cli.md

.. code:: common-lisp

    (asdf:load-system "cli-backend-clingon")

.. code:: common-lisp

    (defparameter *app*
      (stack-cli:make-command
       :name "demo"
       :description "demo CLI"
       :version "0.1.0"
       :subcommands
       (list (stack-cli:make-command
              :name "hello"
              :description "Greet NAME"
              :options (list (stack-cli:make-option
                              :name "count" :short #\c :long "count"
                              :kind :integer :default 1 :help "Number of greetings." :key :count))
              :handler (lambda (opts free)
                         (dotimes (_ (stack-cli:get-option opts :count 1))
                           (format t "Hello, ~{~a~}!~%" free)))))))

The same command from POSIX, PowerShell and CMD argv. CMD style is on by default only on Windows:

.. code:: common-lisp

    (stack-cli:run *app* :argv '("hello" "--count" "2" "Ada"))
    (stack-cli:run *app* :argv '("hello" "-Count" "1" "Grace"))
    (stack-cli:run *app* :argv '("hello" "/Count:1" "Barbara") :styles '(:posix :cmd))

::

    Hello, Ada!
    Hello, Ada!
    Hello, Grace!
    Hello, Barbara!

In a binary, ``(stack-cli:main *app*)`` reads ``uiop:command-line-arguments`` and exits 0, 1 (handler error) or 2 (usage error).
