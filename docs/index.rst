.. python-to-cl documentation master file, created by
   sphinx-quickstart on Thu Jan  4 18:03:55 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

===================================================
Common Lisp alternatives to popular Python packages
===================================================

Common Lisp is a nice and expressive language and this project is an effort to promote or improve CL libraries by comparing to the corresponding Python alternatives.

Each page: the Python package's features, the third-party Common Lisp libraries, the `cl-stack <https://github.com/egao1980/cl-stack>`__ ones, then examples. Reviewed November 2026.

We attempt to document alternatives to few of the most popular Python libraries. Numbers are PyPI download ranks (`top-pypi-packages <https://hugovk.github.io/top-pypi-packages/>`__, 30 days to 2026-11-01); stdlib modules have none.

.. toctree::
   :maxdepth: 1
   :caption: HTTP and web

   Requests (#8) <requests>
   HTTPX / aiohttp (#31, #58) <httpx>
   Flask / FastAPI (#162, #65) <flask>
   boto3 (#1) <boto3>

.. toctree::
   :maxdepth: 1
   :caption: Data formats and validation

   json / orjson (stdlib, #152) <json>
   PyYAML (#12) <pyyaml>
   Pydantic (#18) <pydantic>
   lxml / Beautiful Soup (#81, #110) <lxml>

.. toctree::
   :maxdepth: 1
   :caption: Text and time

   Dateutil (#15) <dateutils>
   re / regex (stdlib, #93) <regex>
   Jinja (#37) <jinja2>

.. toctree::
   :maxdepth: 1
   :caption: Data

   SQLAlchemy / DB drivers (#86) <sqlalchemy>
   pandas / pyarrow (#38, #78) <pandas>
   NumPy (#21) <numpy>

.. toctree::
   :maxdepth: 1
   :caption: System

   pathlib / os.path / shutil (stdlib) <pathlib>
   subprocess (stdlib) <subprocess>
   asyncio (stdlib) <asyncio>
   logging / structlog (stdlib, #273) <logging>
   Click / argparse (#20, stdlib) <click>

.. toctree::
   :maxdepth: 1
   :caption: Security and testing

   cryptography / hashlib / PyJWT (#9, #36) <cryptography>
   pytest (#25) <pytest>

No good alternative yet
-----------------------

- PyTorch, TensorFlow, transformers: `MGL <https://github.com/melisgl/mgl>`__ (active, small) and `cl-waffe2 <https://github.com/hikettei/cl-waffe2>`__ (experimental, last commit 2024-12). Call Python through `py4cl2 <https://github.com/digikar99/py4cl2>`__.

- scikit-learn, SciPy: `CLML <https://github.com/mmaul/clml>`__ gets compatibility fixes only; GSLL fails to install with current CFFI (`Common Lisp tooling in 2025 <https://aliquote.org/post/cl-tooling-in-2025/>`__).

- matplotlib: `Lisp-Stat plot <https://github.com/Lisp-Stat/plot>`__ (Vega-Lite in a browser) or gnuplot wrappers. No native plotting library.

- Selenium / Playwright: `cl-webdriver-client <https://github.com/copyleft/cl-webdriver-client>`__, last commit 2024-09.

- Other AWS and Google Cloud SDKs: only S3, see :doc:`boto3 <boto3>`.

- psutil: nothing portable.

Loading libraries
-----------------

Examples load libraries with `cl-repo <https://github.com/egao1980/cl-repository>`__ from ``ghcr.io/egao1980/cl-systems``, not Quicklisp:

.. code:: sh

    curl -fsSL https://raw.githubusercontent.com/egao1980/cl-repository/main/install.sh | bash
    mkdir demo && cd demo
    echo '(defsystem "demo" :depends-on ("dexador" "cl-stack-http"))' > demo.asd
    cl-repo sync
    cl-repo ros +Q run

Then ``(asdf:load-system "dexador")`` in the REPL. ``+Q`` keeps Roswell's Quicklisp out, so a system missing from the project fails instead of loading from Quicklisp.

Libraries that are not in cl-repo are shown with their documented examples, marked "not run". Results come from SBCL 2.4.11 on macOS arm64, 2026-11-23.

Sources
-------

- PyPI ranks: `top-pypi-packages <https://hugovk.github.io/top-pypi-packages/>`__, 2026-11-01.

- Dan Haskin, `Common Lisp Community Survey 2024 Results <https://blog.djhaskin.com/blog/common-lisp-community-survey-2024-results/>`__, 2024-08-09. 293 respondents. No 2025 or 2026 survey results had been published by November 2026.

- vindarel, `These years in Common Lisp: 2023-2024 in review <https://lisp-journey.gitlab.io/blog/these-years-in-common-lisp-2023-2024-in-review/>`__, 2025-02-17.

- vindarel, `State of the Common Lisp ecosystem, 2020 <https://lisp-journey.gitlab.io/blog/state-of-the-common-lisp-ecosystem-2020/>`__, 2021-02-08.

- Fernando Borretti, `State of the Common Lisp Ecosystem, 2015 <https://borretti.me/article/common-lisp-sotu-2015>`__.

- vindarel, `CSV parsing in Common Lisp, data frames <https://lisp-journey.gitlab.io/blog/csv-parsing-in-common-lisp/>`__, 2026-11-07.

- `Common Lisp tooling in 2025 <https://aliquote.org/post/cl-tooling-in-2025/>`__, aliquote.org, 2025-01-22.

- `Awesome-CL <https://github.com/CodyReichert/awesome-cl>`__, `CLiki <https://www.cliki.net/>`__, `Ultralisp <https://ultralisp.org/>`__.

- cl-stack: `QUICKSTART <https://github.com/egao1980/cl-stack/blob/main/docs/QUICKSTART.md>`__ pins and `capability briefs <https://github.com/egao1980/cl-stack/tree/main/docs/capabilities>`__.

- "Last commit" is the default branch's latest commit (GitHub, Codeberg, sourcehut) on 2026-11-23. Versions are the ones cl-repo installs.
